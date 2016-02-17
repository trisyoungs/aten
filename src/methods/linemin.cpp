/*
	*** Line minimiser
	*** src/methods/line.cpp
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

#include "model/model.h"
#include "methods/linemin.h"

ATEN_USING_NAMESPACE

// Constructor
LineMinimiser::LineMinimiser()
{
	tolerance_ = 0.00001;
}

// Set the tolerance
void LineMinimiser::setTolerance(double t)
{
	tolerance_ = t;
}

// Return current tolerance
double LineMinimiser::tolerance() const
{
	return tolerance_;
}

// Copy contents of source model, initialising the minimiser
void LineMinimiser::initialise(Model* srcmodel)
{
	// Make tempModel_ the same size as srcmodel
	tempModel_.clear();
	tempModel_.setCell(srcmodel->cell());
	Atom* newatom;
	for (Atom* i = srcmodel->atoms(); i != NULL; i = i->next)
	{
		newatom = tempModel_.addAtomAtPen(i->element());
		newatom->setCharge(i->charge());
	}
}

// Perform gradient move
void LineMinimiser::gradientMove(Model* srcmodel, double delta)
{
	// Generate a new set of coordinates in destmodel following the normalised gradient vector present in srcmodel, with the stepsize given
	Messenger::enter("LineMinimiser::gradientMove");
	Atom** srcatoms = srcmodel->atomArray();
	Atom** destatoms = tempModel_.atomArray();
	for (int i=0; i<srcmodel->nAtoms(); ++i)
	{
		destatoms[i]->r() = srcatoms[i]->r();
		if (!srcatoms[i]->isPositionFixed()) destatoms[i]->r() += srcatoms[i]->f() * delta;
	}
	Messenger::exit("LineMinimiser::gradientMove");
}

// Perform Golden Search within specified bounds
void LineMinimiser::goldenSearch(Model* srcmodel, double* bounds, double* energies)
{
	Messenger::enter("LineMinimiser::goldenSearch");
	int targetbound;
	bool success;
	double enew, newmin, delta[3], x1, x2, a, b, c;
	// Check convergence, ready for early return
	if (fabs(bounds[2]-bounds[0]) < tolerance_)
	{
		Messenger::enter("LineMinimiser::goldenSearch");
		return;
	}
	delta[0] = bounds[0] - bounds[1];
	delta[2] = bounds[2] - bounds[1];
	Messenger::print(Messenger::Verbose, "Trying Golden Search - left=%12.5e   right=%12.5e", delta[0], delta[2]);
	// Select largest of two intervals (0-1 or 1-2) to be the target of the search
	targetbound = delta[0] > delta[2] ? 0 : 2;
	newmin = bounds[1] + 0.3819660 * (bounds[targetbound] - bounds[1]);
	gradientMove(srcmodel, newmin);
	enew = srcmodel->totalEnergy(&tempModel_, success);

	Messenger::print(Messenger::Verbose, "--> GOLD point is %12.5e [%12.5e] ", enew, newmin);
	// Check new energy against current minimum, then current bounding point. If it is better than neither, search into other section
	if (enew < energies[1])
	{
		Messenger::print(Messenger::Verbose, "--> GOLD point is lower than current minimum...");
		// Overwrite the outermost bound with the old minimum
		bounds[targetbound] = bounds[1];
		energies[targetbound] = energies[1];
		bounds[1] = newmin;
		energies[1] = enew;
		// Recurse into the new region
		goldenSearch(srcmodel, bounds, energies);
	}
	else if (enew < energies[targetbound])
	{
		Messenger::print(Messenger::Verbose, "--> GOLD point is better than old bounds[%i] : %12.5e [%12.5e]...", targetbound, energies[targetbound], bounds[targetbound]);
		bounds[targetbound] = newmin;
		energies[targetbound] = enew;
		// Recurse into the new region
		goldenSearch(srcmodel, bounds, energies);
	}
	else
	{
	}
	Messenger::exit("LineMinimiser::goldenSearch");
}

// Line minimise supplied model along its current gradient vector (forces)
double LineMinimiser::lineMinimise(Model* srcmodel)
{
	Messenger::enter("LineMinimiser::lineMinimise");
	double enew, ecurrent, bounds[3], energies[3], newmin, a, b, b10, b12;
	bool failed, leftbound, success;
	int count = 0;

	// Brent-style line minimiser with parabolic interpolation and Golden Search backup.
	// We assume that the energy expression for the source model is correct.
	// We also expect the gradient vector (i.e. forces) to follow to be in the srcmodel.

	// Check that tempModel_ has been initialised correctly
	if (srcmodel->nAtoms() != tempModel_.nAtoms())
	{
		printf("Internal Error : LineMinimiser's temporary model has not been initialised to the correct size.\n");
		Messenger::exit("LineMinimiser::lineMinimise");
		return 0.0;
	}

	failed = false;

	// Set initial bounding values
	bounds[0] = 0.0;
	energies[0] = srcmodel->totalEnergy(srcmodel, success);
	if (!success)
	{
		Messenger::exit("LineMinimiser::lineMinimise");
		return 0.0;
	}
	bounds[1] = 0.01;
	gradientMove(srcmodel, bounds[1]);
	energies[1] = srcmodel->totalEnergy(&tempModel_, success);
	bounds[2] = 0.02;
	gradientMove(srcmodel, bounds[2]);
	energies[2] = srcmodel->totalEnergy(&tempModel_, success);

	// Sort w.r.t. energy so that the minimum is in the central point.
	if (energies[1] > energies[0])
	{
		enew = energies[0];
		energies[0] = energies[1];
		energies[1] = enew;
		enew = bounds[0];
		bounds[0] = bounds[1];
		bounds[1] = enew;
	}
	if (energies[1] > energies[2])
	{
		enew = energies[2];
		energies[2] = energies[1];
		energies[1] = enew;
		enew = bounds[2];
		bounds[2] = bounds[1];
		bounds[1] = enew;
	}
	Messenger::print(Messenger::Verbose, "Initial bounding values/energies = %12.5e (%12.5e) %12.5e (%12.5e) %12.5e (%12.5e)",bounds[0],energies[0],bounds[1],energies[1],bounds[2],energies[2]);

	do
	{
		// Perform linesearch along the gradient vector
		Messenger::print(Messenger::Verbose, "Energies [Bounds] = %12.5e (%12.5e) %12.5e (%12.5e) %12.5e (%12.5e)",energies[0],bounds[0],energies[1],bounds[1],energies[2],bounds[2]);
		// Perform parabolic interpolation to find new minimium point
		b10 = bounds[1] - bounds[0];
		b12 = bounds[1] - bounds[2];
		a = (b10 * b10 * (energies[1] - energies[2])) - (b12 * b12 * (energies[1] - energies[0]));
		b = (b10 * (energies[1] - energies[2])) - (b12 * (energies[1] - energies[0]));
		newmin = bounds[1] - 0.5 * (a / b);

		// Compute energy of new point and check that it went down...
		gradientMove(srcmodel, newmin);
		enew = srcmodel->totalEnergy(&tempModel_, success);

		Messenger::print(Messenger::Verbose, "PARABOLIC point gives energy %12.5e @ %12.5e",enew,newmin);
		if (enew < energies[1])
		{
			// New point found...
			Messenger::print(Messenger::Verbose, "--> PARABOLIC point is new minimum...");
			ecurrent = enew;
			// Overwrite the largest of bounds[0] and bounds[2]
			if (energies[2] > energies[0])
			{
				bounds[2] = bounds[1];
				energies[2] = energies[1];
			}
			else
			{
				bounds[0] = bounds[1];
				energies[0] = energies[1];
			}
			bounds[1] = newmin;
			energies[1] = ecurrent;
		}
		else
		{
			// Is the parabolic point better than the relevant bound in that direction?
			if ((energies[2] - enew) > tolerance_)
			{
				Messenger::print(Messenger::Verbose, "--> PARABOLIC point is better than bounds[2]...");
				bounds[2] = newmin;
				energies[2] = enew;
			}
			else if ((energies[0] - enew) > tolerance_)
			{
				Messenger::print(Messenger::Verbose, "--> PARABOLIC point is better than bounds[0]...");
				bounds[0] = newmin;
				energies[0] = enew;
			}
			else
			{
				Messenger::print(Messenger::Verbose, "--> PARABOLIC point is worse than all current values...");
				// Try recursive Golden Search instead, into the largest of the two sections.
				goldenSearch(srcmodel, bounds, energies);
				ecurrent = energies[1];
			}
		}
// 		printf("DIFF = %f, 2tol = %f\n", fabs(bounds[0]-bounds[2]), 2.0 * tolerance_);
		++count;
		if (count > 10) break;
	} while (fabs(bounds[0]-bounds[2]) > (2.0 * tolerance_));
// 	printf("Final bounding values are %f %f %f\n",bounds[0],bounds[1],bounds[2]);
	gradientMove(srcmodel, bounds[1]);
	srcmodel->copyAtomData(&tempModel_, Atom::PositionData);
	srcmodel->logChange(Log::Coordinates);
	srcmodel->updateMeasurements();
	Messenger::exit("LineMinimiser::lineMinimise");
	return energies[1];
}

