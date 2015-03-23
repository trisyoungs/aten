/*
	*** Conjugate gradient minimiser
	*** src/methods/cg.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/progress.h"

ATEN_BEGIN_NAMESPACE

// Static Singleton
MethodCg cg;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE
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
void MethodCg::minimise(Model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	Messenger::enter("MethodCg::minimise");
	int cycle, i;
	double currentEnergy, oldEnergy, deltaEnergy = 0.0, lastPrintedEnergy, oldForce, newForce, deltaForce = 0.0, g_old_sq, gamma, g_current_sq;
	double* g_old;
	Vec3<double> f;
	Atom** modelatoms;
	bool linedone, converged, success;
	Dnchar etatext;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if ((!srcmodel->isExpressionValid()) || (srcmodel->nAtoms() == 0))
	{
	        Messenger::exit("MethodCg::minimise");
	        return;
	}
	
	// Calculate initial reference energy and RMS force
	modelatoms = srcmodel->atomArray();
	g_old = new double[srcmodel->nAtoms()*3];
	currentEnergy = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
	        Messenger::exit("MethodCg::minimise");
	        return;
	}

	srcmodel->calculateForces(srcmodel);
	newForce = srcmodel->rmsForce();
	lastPrintedEnergy = currentEnergy;
	srcmodel->energy.print();

	converged = FALSE;
	linedone = FALSE;

	// Initialise the line minimiser
	initialise(srcmodel);

	Messenger::print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)");
	Messenger::print("Init  %12.5e        ---           ---     %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s", currentEnergy, newForce, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), "--:--:--");

	int pid = progress.initialise("Minimising (CG)", nCycles_);

	srcmodel->normaliseForces(1.0, TRUE);

	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
		if (!progress.update(pid, cycle)) linedone = TRUE;
		else
		{
			oldEnergy = currentEnergy;
			oldForce = newForce;
			currentEnergy = lineMinimise(srcmodel);
			newForce = srcmodel->rmsForce();
			deltaEnergy = currentEnergy - oldEnergy;
			deltaForce = newForce - oldForce;
			// Check convergence criteria
			if ((fabs(deltaEnergy) < econ) && (fabs(deltaForce) < fcon)) converged = TRUE;
		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1))
		{
			Messenger::print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s", cycle+1, currentEnergy, currentEnergy-lastPrintedEnergy, newForce, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), qPrintable(progress.eta()));
			lastPrintedEnergy = currentEnergy;
		}
		if (converged) break;

// 		if (prefs.shouldUpdateModel(cycle+1)) parent_.updateWidgets(AtenWindow::CanvasTarget); ATEN2 TODO

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

	if (converged) Messenger::print("Conjugate gradient converged in %i steps.",cycle+1);
	else Messenger::print("Conjugate gradient did not converge within %i steps.",nCycles_);
	Messenger::print("Final energy:");
	currentEnergy = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();

	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->changeLog.add(Log::Coordinates);
	Messenger::exit("MethodCg::minimise");
}

