/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/gui.h"

// Static Singleton
MethodSd sd;

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
	// Line Search (Steepest Descent) energy minimisation.
	msg.enter("MethodSd::minimise");
	int cycle, nattempts;
	double oldEnergy, newEnergy, deltaEnergy, oldRms, newRms, deltaRms, stepsize;
	bool lineDone, converged, success;
	Dnchar etatext;

	/*
	// Prepare the calculation
	*/
	// First, create expression for the current model and assign charges
	if ((!srcmodel->createExpression()) || (srcmodel->nAtoms() == 0))
	{
	        msg.exit("MethodSd::minimise");
	        return;
	}
	
	// Calculate initial reference energy and forces
	newEnergy = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
	        msg.exit("MethodSd::minimise");
	        return;
	}
	srcmodel->energy.print();

	converged = FALSE;
	lineDone = FALSE;

	// Initialise the line minimiser
	initialise(srcmodel);

	// Calculate initial forces and corresponding rms
	srcmodel->calculateForces(srcmodel);
	newRms = srcmodel->rmsForce();

	msg.print("Step      Energy       DeltaE       RMS Force      E(vdW)        E(elec)       E(Bond)      E(Angle)     E(Torsion)\n");
	msg.print("Init  %12.5e        ---     %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s\n", newEnergy, newRms, srcmodel->energy.vdw(), srcmodel->energy.elec(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), etatext.get());
	gui.progressCreate("Minimising (SD)", nCycles_);

	stepsize = 1.0;
	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Perform linesearch along the gradient vector
		if (!gui.progressUpdate(cycle, &etatext)) lineDone = TRUE;
		else
		{
			// Simple method begins here
			oldEnergy = newEnergy;
			oldRms = newRms;

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
					newEnergy = srcmodel->totalEnergy(&tempModel_, success);
					if (newEnergy > oldEnergy) stepsize *= 0.5;
				} while (newEnergy > oldEnergy);
				// If the very first attempt was successful, increase the stepsize again
				if (nattempts == 1) stepsize *= 1.5;
			}
			else newEnergy = lineMinimise(srcmodel);
// printf("StepSize = %f (%i)\n", stepsize, nattempts);
			srcmodel->copyAtomData(&tempModel_, Atom::PositionData);

			// Calculate forces ready for next cycle
			srcmodel->calculateForces(srcmodel);
			newRms = srcmodel->rmsForce();
			deltaEnergy = newEnergy - oldEnergy;
			deltaRms = newRms - oldRms;

			// Check convergence criteria
			if ((fabs(deltaEnergy) < econ) && (fabs(newRms) < fcon))
			{
				converged = TRUE;
				break;
			}

		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle)) msg.print("%-5i %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e  %12.5e %s\n",cycle+1, newEnergy, deltaEnergy, newRms, srcmodel->energy.vdw(), srcmodel->energy.elec(), srcmodel->energy.bond(), srcmodel->energy.angle(), srcmodel->energy.torsion(), etatext.get());

		if (prefs.shouldUpdateModel(cycle)) gui.update(FALSE, FALSE, FALSE, FALSE);

		if (lineDone || converged) break;
	}
	gui.progressTerminate();

	if (converged) msg.print("Steepest descent converged in %i steps.\n",cycle+1);
	else msg.print("Steepest descent did not converge within %i steps.\n",nCycles_);
	msg.print("Final energy:\n");
	newEnergy = srcmodel->totalEnergy(srcmodel, success);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
// 	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->changeLog.add(Log::Coordinates);
	msg.exit("MethodSd::minimise");
}

