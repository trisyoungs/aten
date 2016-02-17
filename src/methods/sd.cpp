/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp
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

#include "methods/sd.h"
#include "model/model.h"
#include "ff/energystore.h"

ATEN_BEGIN_NAMESPACE

// Static Singleton
MethodSd sd;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Constructor
MethodSd::MethodSd()
{
	// Private variables
	nCycles_ = 100;
}

// Set maximum number of cycles to perform
void MethodSd::setNCycles(int i)
{
	nCycles_ = i;
}

// Get maximum number of  for MC move
int MethodSd::nCycles() const
{
	return nCycles_;
}

// Minimise Energy w.r.t. coordinates by Steepest Descent
double MethodSd::minimise(Model* sourceModel, double eConverge, double fConverge, bool simple)
{
	Messenger::enter("MethodSd::minimise");
	int cycle, nattempts;
	double oldEnergy, currentEnergy, deltaEnergy, lastPrintedEnergy, oldForce, newForce, deltaForce, stepsize;
	bool lineDone, converged, success;

	/*
	 * Prepare the calculation
	 */
	// First, create expression for the current model and assign charges
	if ((!sourceModel->isExpressionValid()) || (sourceModel->nAtoms() == 0))
	{
	        Messenger::exit("MethodSd::minimise");
	        return 0.0;
	}
	
	// Calculate initial reference energy and forces
	currentEnergy = sourceModel->totalEnergy(sourceModel, success);
	if (!success)
	{
	        Messenger::exit("MethodSd::minimise");
	        return 0.0;
	}
	lastPrintedEnergy = currentEnergy;
	sourceModel->energy.print();

	converged = false;
	lineDone = false;

	// Initialise the line minimiser
	initialise(sourceModel);

	// Calculate initial forces and corresponding rms
	sourceModel->calculateForces(sourceModel);
	newForce = sourceModel->rmsForce();

	Messenger::print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)");
	Messenger::print("Init  %12.5e       ---      %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e", currentEnergy, newForce, sourceModel->energy.vdw(), sourceModel->energy.electrostatic(), sourceModel->energy.bond(), sourceModel->energy.angle(), sourceModel->energy.torsion());

	Task* task = Messenger::initialiseTask("Minimising (SD)", nCycles_);

	stepsize = 1.0;
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
 		if (!Messenger::updateTaskProgress(task, cycle)) break;
		else
		{
			// Simple method begins here
			oldEnergy = currentEnergy;
			oldForce = newForce;

			// Minimise along gradient vector
			sourceModel->normaliseForces(1.0, true);
			if (simple)
			{
				// Step along gradient (with reducing step size until energy decreases)
				nattempts = 0;
				do
				{
					++nattempts;
					gradientMove(sourceModel, stepsize);
					currentEnergy = sourceModel->totalEnergy(&tempModel_, success);
					if (currentEnergy > oldEnergy) stepsize *= 0.5;
				} while (currentEnergy > oldEnergy);
				// If the very first attempt was successful, increase the stepsize again
				if (nattempts == 1) stepsize *= 1.5;
			}
			else currentEnergy = lineMinimise(sourceModel);
			sourceModel->copyAtomData(&tempModel_, Atom::PositionData);

			// Calculate forces ready for next cycle
			sourceModel->calculateForces(sourceModel);
			newForce = sourceModel->rmsForce();
			deltaEnergy = currentEnergy - oldEnergy;
			deltaForce = newForce - oldForce;

			// Check convergence criteria
			if ((fabs(deltaEnergy) < eConverge) && (fabs(newForce) < fConverge))
			{
				converged = true;
				break;
			}

		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1))
		{
			Messenger::print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e",cycle+1, currentEnergy, currentEnergy-lastPrintedEnergy, newForce, sourceModel->energy.vdw(), sourceModel->energy.electrostatic(), sourceModel->energy.bond(), sourceModel->energy.angle(), sourceModel->energy.torsion());
			lastPrintedEnergy = currentEnergy;
		}

// 		if (prefs.shouldUpdateModel(cycle+1)) parent_.updateWidgets(AtenWindow::MainViewTarget); ATEN2 TODO

		if (lineDone || converged) break;
	}
	Messenger::terminateTask(task);

	if (converged) Messenger::print("Steepest descent converged in %i steps.",cycle+1);
	else Messenger::print("Steepest descent did not converge within %i steps.",nCycles_);
	Messenger::print("Final energy:");
	currentEnergy = sourceModel->totalEnergy(sourceModel, success);
	sourceModel->energy.print();
	
	// Calculate fresh new forces for the model, log changes / update, and exit.
// 	sourceModel->calculateForces(sourceModel);
	sourceModel->updateMeasurements();
	sourceModel->logChange(Log::Coordinates);
	
	Messenger::exit("MethodSd::minimise");
	return currentEnergy;
}

