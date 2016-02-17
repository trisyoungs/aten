/*
	*** Conjugate gradient minimiser
	*** src/methods/cg.cpp
	Copyright T. Youngs 2007-2016

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
double MethodCg::minimise(Model* sourceModel, double eConverge, double fConverge)
{
	// Line Search (Steepest Descent) energy minimisation.
	Messenger::enter("MethodCg::minimise");
	int cycle, i;
	double currentEnergy, oldEnergy, deltaEnergy = 0.0, lastPrintedEnergy, oldForce, newForce, deltaForce = 0.0, g_old_sq, gamma, g_current_sq;
	double* g_old;
	Vec3<double> f;
	Atom** modelAtoms;
	bool lineDone, converged, success;

	/*
	 * Prepare the calculation
	 */
	// First, create expression for the current model and assign charges
	if ((!sourceModel->isExpressionValid()) || (sourceModel->nAtoms() == 0))
	{
	        Messenger::exit("MethodCg::minimise");
	        return 0.0;
	}
	
	// Calculate initial reference energy and RMS force
	modelAtoms = sourceModel->atomArray();
	g_old = new double[sourceModel->nAtoms()*3];
	currentEnergy = sourceModel->totalEnergy(sourceModel, success);
	if (!success)
	{
	        Messenger::exit("MethodCg::minimise");
	        return 0.0;
	}

	sourceModel->calculateForces(sourceModel);
	newForce = sourceModel->rmsForce();
	lastPrintedEnergy = currentEnergy;
	sourceModel->energy.print();

	converged = false;
	lineDone = false;

	// Initialise the line minimiser
	initialise(sourceModel);

	Messenger::print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)");
	Messenger::print("Init  %12.5e        ---           ---     %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s", currentEnergy, newForce, sourceModel->energy.vdw(), sourceModel->energy.electrostatic(), sourceModel->energy.bond(), sourceModel->energy.angle(), sourceModel->energy.torsion(), "--:--:--");

	Task* task = Messenger::initialiseTask("Minimising (CG)", nCycles_);

	sourceModel->normaliseForces(1.0, true);

	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
		if (!Messenger::updateTaskProgress(task, cycle)) break;
		else
		{
			oldEnergy = currentEnergy;
			oldForce = newForce;
			currentEnergy = lineMinimise(sourceModel);
			newForce = sourceModel->rmsForce();
			deltaEnergy = currentEnergy - oldEnergy;
			deltaForce = newForce - oldForce;
			// Check convergence criteria
			if ((fabs(deltaEnergy) < eConverge) && (fabs(deltaForce) < fConverge)) converged = true;
		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1))
		{
			Messenger::print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e", cycle+1, currentEnergy, currentEnergy-lastPrintedEnergy, newForce, sourceModel->energy.vdw(), sourceModel->energy.electrostatic(), sourceModel->energy.bond(), sourceModel->energy.angle(), sourceModel->energy.torsion());
			lastPrintedEnergy = currentEnergy;
		}
		if (converged) break;

// 		if (prefs.shouldUpdateModel(cycle+1)) parent_.updateWidgets(AtenWindow::MainViewTarget); ATEN2 TODO

		// Store old forces and calculate new forces at the new line-minimised position
		for (i=0; i<sourceModel->nAtoms()*3; i += 3)
		{
			f = modelAtoms[i/3]->f();
			g_old[i] = f.x;
			g_old[i+1] = f.y;
			g_old[i+2] = f.z;
		}
		sourceModel->calculateForces(sourceModel);
		sourceModel->normaliseForces(1.0, true);

		// Calculate new conjugate gradient vector, if this isn't the first cycle
		if (cycle != 0)
		{
			/* 
			 * The next gradient vector is given by (Polak-Ribiere):
			 *    g[new] = g[current] + gamma * g[old]     where  gamma = (g[current] - g[old]).g[current]  /  g[old].g[old]
			 *    **or** (Fletcher-Reeves)
			 *    g[new] = g[current] + gamma * g[old]     where  gamma = g[current].g[current]  /  g[old].g[old]
			 */

			// Calculate 'g[current].g[current]' and '(g[current] - g[old]).g[current]'
			g_old_sq = 0.0;
			g_current_sq = 0.0;
			for (i=0; i<sourceModel->nAtoms()*3; i++) g_old_sq += g_old[i] * g_old[i];
			for (i=0; i<sourceModel->nAtoms(); i++)
			{
				f = modelAtoms[i]->f();
				g_current_sq += f.x * f.x;
				g_current_sq += f.y * f.y;
				g_current_sq += f.z * f.z;
			}

			// Calculate gamma
			gamma = g_current_sq / g_old_sq;

			// Calculate new gradient vector
			for (int i=0; i<sourceModel->nAtoms()*3; i += 3)
			{
				f = modelAtoms[i/3]->f();
				f.x = g_old[i] + gamma * f.x;
				f.y = g_old[i+1] + gamma * f.y;
				f.z = g_old[i+2] + gamma * f.z;
				modelAtoms[i/3]->f() = f;
			}
		}
	}
 	Messenger::terminateTask(task);

	if (converged) Messenger::print("Conjugate gradient converged in %i steps.",cycle+1);
	else Messenger::print("Conjugate gradient did not converge within %i steps.",nCycles_);
	Messenger::print("Final energy:");
	currentEnergy = sourceModel->totalEnergy(sourceModel, success);
	sourceModel->energy.print();

	// Calculate fresh new forces for the model, log changes / update, and exit.
	sourceModel->calculateForces(sourceModel);
	sourceModel->updateMeasurements();
	sourceModel->logChange(Log::Coordinates);

	Messenger::exit("MethodCg::minimise");
	return currentEnergy;
}

