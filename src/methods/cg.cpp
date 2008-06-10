/*
	*** Conjugate gradient minimiser
	*** src/methods/cg.cpp
	Copyright T. Youngs 2007,2008

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
#include "classes/energystore.h"
#include "base/prefs.h"
#include "gui/gui.h"

// Static Singleton
MethodCg cg;

// Constructor
MethodCg::MethodCg()
{
	nCycles_ = 100;
}

// Minimise Energy w.r.t. coordinates by Conjugate Gradient
void MethodCg::minimise(Model *srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	dbgBegin(Debug::Calls,"MethodCg::minimise");
	int cycle, m, i;
	double enew, ecurrent, edelta, rmscurrent, rmsnew, fdelta, g_old_sq, gamma, g_current_sq;
	double *g_old;
	Vec3<double> f;
	Atom **modelatoms;
	bool linedone, converged;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if ((!srcmodel->createExpression()) || (srcmodel->nAtoms() == 0))
	{
	        dbgEnd(Debug::Calls,"MethodCg::minimise");
	        return;
	}
	
	// Calculate initial reference energy and RMS force
	modelatoms = srcmodel->atomArray();
	g_old = new double[srcmodel->nAtoms()*3];
	ecurrent = srcmodel->totalEnergy(srcmodel);
	srcmodel->calculateForces(srcmodel);
	rmscurrent = srcmodel->calculateRmsForce();
	srcmodel->energy.print();

	converged = FALSE;
	linedone = FALSE;

	msg(Debug::None,"Step         Energy          DeltaE          RMS Force\n");
	msg(Debug::None,"Init  %15.5e          ---      %15.5e\n",ecurrent,rmscurrent);
	gui.progressCreate("Minimising (CG)", nCycles_);

	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// We need to (do we?) define some sort of length scale so we take sensible steps along the gradient vector.
		//srcmodel->normalise_forces(1.0);
		//for (i=0; i<srcmodel->nAtoms(); i++) modelatoms[i]->f /= elements.mass(modelatoms[i]);

		// Perform linesearch along the gradient vector
		if (!gui.progressUpdate(cycle)) linedone = TRUE;
		else
		{
			enew = lineMinimise(srcmodel);
			edelta = enew - ecurrent;
			rmsnew = srcmodel->calculateRmsForce();
			fdelta = rmsnew - rmscurrent;
			// Check convergence criteria
			if ((fabs(edelta) < econ) && (fabs(fdelta) < fcon)) converged = TRUE;
			ecurrent = enew;
			rmscurrent = rmsnew;
		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1)) msg(Debug::None,"%-5i %15.5e  %15.5e  %15.5e\n",cycle+1,ecurrent,edelta,rmscurrent);

		if (linedone || converged) break;

		// Store old forces and calculate new forces at the new line-minimised position
		for (i=0; i<srcmodel->nAtoms()*3; i += 3)
		{
			f = modelatoms[i/3]->f();
			g_old[i] = -f.x;
			g_old[i+1] = -f.y;
			g_old[i+2] = -f.z;
		}
		srcmodel->calculateForces(srcmodel);

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
	gui.progressTerminate();

	if (converged) msg(Debug::None,"Conjugate gradient converged in %i steps.\n",cycle+1);
	else msg(Debug::None,"Conjugate gradient did not converge within %i steps.\n",nCycles_);
	msg(Debug::None,"Final energy:\n");
	ecurrent = srcmodel->totalEnergy(srcmodel);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->logChange(Change::CoordinateLog);
	dbgEnd(Debug::Calls,"MethodCg::minimise");
}

