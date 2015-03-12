/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp
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

#include "methods/sd.h"
#include "model/model.h"
#include "ff/energystore.h"
#include "base/progress.h"

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
void MethodSd::minimise(Model* srcmodel, double econ, double fcon, bool simple)
{
	Messenger::enter("MethodSd::minimise");
	int cycle, nattempts;
	double oldEnergy, currentEnergy, deltaEnergy, lastPrintedEnergy, oldForce, newForce, deltaForce, stepsize;
	bool lineDone, converged, success;
	Dnchar etatext;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if ((!srcmodel->isExpressionValid()) || (srcmodel->nAtoms() == 0))
	{
	        Messenger::exit("MethodSd::minimise");
	        return;
	}
	
	// Calculate initial reference energy and forces
	currentEnergy = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
	        Messenger::exit("MethodSd::minimise");
	        return;
	}
	lastPrintedEnergy = currentEnergy;
	srcmodel->energy.print();

	converged = FALSE;
	lineDone = FALSE;

	// Initialise the line minimiser
	initialise(srcmodel);

	// Calculate initial forces and corresponding rms
	srcmodel->calculateForces(srcmodel);
	newForce = srcmodel->rmsForce();

	Messenger::print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)");
	Messenger::print("Init  %12.5e       ---      %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s", currentEnergy, newForce, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), "--:--:--");
	int pid = progress.initialise("Minimising (SD)", nCycles_);

	stepsize = 1.0;
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
 		if (!progress.update(pid, cycle)) lineDone = TRUE;
		else
		{
			// Simple method begins here
			oldEnergy = currentEnergy;
			oldForce = newForce;

			// Minimise along gradient vector
			srcmodel->normaliseForces(1.0, TRUE);
			if (simple)
			{
				// Step along gradient (with reducing step size until energy decreases)
				nattempts = 0;
				do
				{
					++nattempts;
					gradientMove(srcmodel, stepsize);
					currentEnergy = srcmodel->totalEnergy(&tempModel_, success);
					if (currentEnergy > oldEnergy) stepsize *= 0.5;
				} while (currentEnergy > oldEnergy);
				// If the very first attempt was successful, increase the stepsize again
				if (nattempts == 1) stepsize *= 1.5;
			}
			else currentEnergy = lineMinimise(srcmodel);
			srcmodel->copyAtomData(&tempModel_, Atom::PositionData);

			// Calculate forces ready for next cycle
			srcmodel->calculateForces(srcmodel);
			newForce = srcmodel->rmsForce();
			deltaEnergy = currentEnergy - oldEnergy;
			deltaForce = newForce - oldForce;

			// Check convergence criteria
			if ((fabs(deltaEnergy) < econ) && (fabs(newForce) < fcon))
			{
				converged = TRUE;
				break;
			}

		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1))
		{
			Messenger::print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s",cycle+1, currentEnergy, currentEnergy-lastPrintedEnergy, newForce, srcmodel->energy.vdw(), srcmodel->energy.electrostatic(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), progress.eta());
			lastPrintedEnergy = currentEnergy;
		}

// 		if (prefs.shouldUpdateModel(cycle+1)) parent_.updateWidgets(AtenWindow::CanvasTarget); ATEN2 TODO

		if (lineDone || converged) break;
	}
	progress.terminate(pid);

	if (converged) Messenger::print("Steepest descent converged in %i steps.",cycle+1);
	else Messenger::print("Steepest descent did not converge within %i steps.",nCycles_);
	Messenger::print("Final energy:");
	currentEnergy = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();
	
	// Calculate fresh new forces for the model, log changes / update, and exit.
// 	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->changeLog.add(Log::Coordinates);
	
	Messenger::exit("MethodSd::minimise");
}

