/*
	*** Steepest descent minimiser
	*** src/methods/sd.cpp
	Copyright T. Youngs 2007-2009

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
void MethodSd::minimise(Model* srcmodel, double econ, double fcon)
{
	// Line Search (Steepest Descent) energy minimisation.
	msg.enter("MethodSd::minimise");
	int cycle, i;
	double newEnergy, currentEnergy, deltaEnergy, currentRms, newRms;
	Atom **modelAtoms;
	bool lineDone, converged;
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
	
	// Calculate initial reference energy and RMS force
	modelAtoms = srcmodel->atomArray();
	currentEnergy = srcmodel->totalEnergy(srcmodel);
	srcmodel->calculateForces(srcmodel);
	currentRms = srcmodel->calculateRmsForce();
	srcmodel->energy.print();

	converged = FALSE;
	lineDone = FALSE;

	msg.print("Step         Energy          DeltaE          RMS Force\n");
	msg.print("Init  %15.5e          ---      %15.5e\n",currentEnergy,currentRms);
	gui.progressCreate("Minimising (SD)", nCycles_);

	for (cycle=0; cycle<nCycles_; cycle++)
	{
		// Calculate current forces which will be our gradient vector
		//srcmodel->zeroForces();
		//srcmodel->calculateForces(srcmodel);
		//srcmodel->zeroForcesFixed();
		// We need to (do we?) define some sort of length scale so we take sensible steps along the gradient vector.
		//srcmodel->normalise_forces(1.0);
		//for (i=0; i<srcmodel->nAtoms(); i++) modelAtoms[i]->f() /= elements.atomicMass(modelAtoms[i]);

		// Perform linesearch along the gradient vector
		if (!gui.progressUpdate(cycle, &etatext)) lineDone = TRUE;
		else
		{
			newEnergy = lineMinimise(srcmodel);
			deltaEnergy = newEnergy - currentEnergy;
			newRms = srcmodel->calculateRmsForce();
			//fdelta = newRms - currentRms;
			// Check convergence criteria
			if ((fabs(deltaEnergy) < econ) && (newRms < fcon)) converged = TRUE;
			currentEnergy = newEnergy;
			currentRms = newRms;
		}

		// Print out the step data
		if (prefs.shouldUpdateEnergy(cycle+1)) msg.print("%-5i %15.5e  %15.5e  %15.5e %s\n",cycle+1,currentEnergy,deltaEnergy,currentRms,etatext.get());

		if (lineDone || converged) break;
	}
	gui.progressTerminate();

	if (converged) msg.print("Steepest descent converged in %i steps.\n",cycle+1);
	else msg.print("Steepest descent did not converge within %i steps.\n",nCycles_);
	msg.print("Final energy:\n");
	currentEnergy = srcmodel->totalEnergy(srcmodel);
	srcmodel->energy.print();
	// Calculate fresh new forces for the model, log changes / update, and exit.
	srcmodel->calculateForces(srcmodel);
	srcmodel->updateMeasurements();
	srcmodel->changeLog.add(Log::Coordinates);
	msg.exit("MethodSd::minimise");
}

