/*
	*** Conjugate gradient minimiser
	*** src/methods/cg.cpp
	Copyright T. Youngs 2007-2011

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "methods/cg.h"
#include "model/model.h"
#include "ff/energystore.h"
#include "gui/gui.h"
#include "base/progress.h"

// Static Singleton
MethodCg cg;

// Constructor
MethodCg::MethodCg()
{
	nCycles_ = 100;
}

// Set maximum number of cycles to perform
void MethodCg::setNCycles(int i)
{
	nCycles_ = i;
}

// Get maximum number of cycles
int MethodCg::nCycles() const
{
	return nCycles_;
}

// Minimise Energy w.r.t. coordinates by Conjugate Gradient
void MethodCg::minimise(Model *srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	msg.enter("MethodCg::minimise");
	int cycle, i;
	double newEnergy, oldEnergy, deltaEnergy = 0.0, oldRms, newRms, g_old_sq, gamma, g_current_sq;
	double *g_old;
	Vec3<double> f;
	Atom **modelatoms;
	bool linedone, converged, success;
	Dnchar etatext;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if ((!srcmodel->createExpression()) || (srcmodel->nAtoms() == 0))
	{
	        msg.exit("MethodCg::minimise");
	        return;
	}
	
	// Calculate initial reference energy and RMS force
	modelatoms = srcmodel->atomArray();
	g_old = new double[srcmodel->nAtoms()*3];
	newEnergy = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
	        msg.exit("MethodCg::minimise");
	        return;
	}

	srcmodel->calculateForces(srcmodel);
	newRms = srcmodel->rmsForce();
	srcmodel->energy.print();

	converged = FALSE;
	linedone = FALSE;

	// Initialise the line minimiser
	initialise(srcmodel);

	msg.print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)\n");
	msg.print("Init  %12.5e  %12.5e        ---     %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s\n", newEnergy, newRms, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), "--:--:--");

	int pid = progress.initialise("Minimising (CG)", nCycles_, !gui.exists());

	srcmodel->normaliseForces(1.0, TRUE);

	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
		if (!progress.update(pid, cycle)) linedone = TRUE;
		else
		{
			newEnergy = oldEnergy;
			newRms = oldRms;
			newEnergy = lineMinimise(srcmodel);
			deltaEnergy = oldEnergy - newEnergy;
			newRms = srcmodel->rmsForce();
			// Check convergence criteria
			if ((fabs(deltaEnergy) < econ) && (newRms < fcon)) converged = TRUE;
		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1)) msg.print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s\n", cycle+1, newEnergy, deltaEnergy, newRms, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), progress.eta());

		if (prefs.shouldUpdateModel(cycle+1)) gui.update(GuiQt::CanvasTarget);

		// Store old forces and calculate new forces at the new line-minimised position
		for (i=0; i<srcmodel->nAtoms()*3; i += 3)
		{
			f = modelatoms[i/3]->f();
			g_old[i] = f.x;
			g_old[i+1] = f.y;
			g_old[i+2] = f.z;
		}
		srcmodel->calculateForces(srcmodel);
		srcmodel->normaliseForces(1.0, TRUE);

		// Calculate new conjugate gradient vector, if this isn't the first cycle
		if (cycle != 0)
		{
			/* The next gradient vector is given by (Polak-Ribiere):
			g[new] = g[current] + gamma * g[old]     where  gamma = (g[current] - g[old]).g[current]  /  g[old].g[old]
			**or** (Fletcher-Reeves)
			g[new] = g[current] + gamma * g[old]     where  gamma = g[current].g[current]  /  g[old].g[old]
			*/
			// Calculate 'g[current].g[current]' and '(g[current] - g[old]).g[current]'
			g_old_sq = 0.0;
			g_current_sq = 0.0;
			for (i=0; i<srcmodel->nAtoms()*3; i++) g_old_sq += g_old[i] * g_old[i];
			for (i=0; i<srcmodel->nAtoms(); i++)
			{
				f = modelatoms[i]->f();
				g_current_sq += f.x * f.x;
				g_current_sq += f.y * f.y;
				g_current_sq += f.z * f.z;
			}
			// Calculate gamma
			gamma = g_current_sq / g_old_sq;
			// Calculate new gradient vector
			for (int i=0; i<srcmodel->nAtoms()*3; i += 3)
			{
				f = modelatoms[i/3]->f();
				f.x = g_old[i] + gamma * f.x;
				f.y = g_old[i+1] + gamma * f.y;
				f.z = g_old[i+2] + gamma * f.z;
				modelatoms[i/3]->f() = f;
			}
		}
	}
 	progress.terminate(pid);

	if (converged) msg.print("Conjugate gradient converged in %i steps.\n",cycle+1);
	else msg.print("Conjugate gradient did not converge within %i steps.\n",nCycles_);
	msg.print("Final energy:\n");
	newEnergy = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->changeLog.add(Log::Coordinates);
	msg.exit("MethodCg::minimise");
}

