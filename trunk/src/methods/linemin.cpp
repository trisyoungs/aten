/*
	*** Line minimiser
	*** src/methods/line.cpp
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

#include "model/model.h"
#include "methods/linemin.h"

// Constructor
LineMinimiser::LineMinimiser()
{
	tolerance_ = 0.00001;
}

// Perform gradient move
void LineMinimiser::gradientMove(Model *srcmodel, Model *destmodel, double delta)
{
	// Generate a new set of coordinates in destmodel following the normalised gradient vector present in srcmodel, with the stepsize given
	msg.enter("LineMinimiser::gradientMove");
	int i;
	Atom **srcatoms = srcmodel->atomArray();
	Atom **destatoms = destmodel->atomArray();
	for (i=0; i<srcmodel->nAtoms(); ++i) destatoms[i]->r() = srcatoms[i]->r() + srcatoms[i]->f() * delta;
	msg.exit("LineMinimiser::gradientMove");
}

// Line minimise supplied model along its current gradient vector (forces)
double LineMinimiser::lineMinimise(Model *srcmodel)
{
	msg.enter("LineMinimiser::lineMinimise");
	double enew, ecurrent, bound[3], energy[3], newmin, a, b, b10, b12;
	Model destmodel;
	bool failed, leftbound, success;

	// Brent-style line minimiser with parabolic interpolation and Golden Search backup.
	// We assume that the energy expression for the source model is correct.
	// We also expect the gradient vector (i.e. forces) to follow to be in the srcmodel.

	// Copy source model coordinates so the size of destmodel is the same
	destmodel.copy(srcmodel);

	failed = FALSE;

	// Set initial bounding values
	bound[1] = 0.0;
	energy[1] = srcmodel->totalEnergy(&destmodel, success);
	if (!success)
	{
		msg.exit("LineMinimiser::lineMinimise");
		return 0.0;
	}
	bound[0] = 0.25;
	gradientMove(srcmodel, &destmodel, bound[0]);
	energy[0] = srcmodel->totalEnergy(&destmodel, success);
	bound[2] = 0.5;
	gradientMove(srcmodel, &destmodel, bound[2]);
	energy[2] = srcmodel->totalEnergy(&destmodel, success);

	// Sort w.r.t. energy so that the minimum is in the central point.
	if (energy[1] > energy[0])
	{
		enew = energy[0];
		energy[0] = energy[1];
		energy[1] = enew;
		enew = bound[0];
		bound[0] = bound[1];
		bound[1] = enew;
	}
	if (energy[1] > energy[2])
	{
		enew = energy[2];
		energy[2] = energy[1];
		energy[1] = enew;
		enew = bound[2];
		bound[2] = bound[1];
		bound[1] = enew;
	}
	msg.print(Messenger::Verbose,"Initial bounding values/energies = %f (%f) %f (%f) %f (%f)\n",bound[0],energy[0],bound[1],energy[1],bound[2],energy[2]);
// 	printf("START OF LINEMIN:\n");
// 	for (int n=-20; n<=20; n++)
// 	{
// 		gradientMove(srcmodel, &destmodel, n*0.001);
// 		printf("Energy at %f = %f\n", n*0.001, srcmodel->totalEnergy(&destmodel));
// 	}
// 	return 0.0;
	do
	{
		// Perform linesearch along the gradient vector
// 		printf("Energies = %f %f %f\n",energy[0],energy[1],energy[2]);
// 		printf("Bounds   = %f %f %f\n",bound[0], bound[1], bound[2]);
		// Perform parabolic interpolation to find new minimium point
		b10 = bound[1] - bound[0];
		b12 = bound[1] - bound[2];
		a = (b10 * b10 * (energy[1] - energy[2])) - (b12 * b12 * (energy[1] - energy[0]));
		b = (b10 * (energy[1] - energy[2])) - (b12 * (energy[1] - energy[0]));
		newmin = bound[1] - 0.5 * (a / b);

		// Compute energy of new point and check that it went down...
		gradientMove(srcmodel, &destmodel, newmin);
		enew = srcmodel->totalEnergy(&destmodel, success);

// 		printf("PARABOLIC point gives energy %f @ %f\n",enew,newmin);
		if (enew < energy[1])
		{
			// New point found...
			//printf("PARABOLIC point is new minimum...\n");
			ecurrent = enew;
			// Overwrite the largest of bound[0] and bound[2]
			if (energy[2] > energy[0])
			{
				bound[2] = bound[1];
				energy[2] = energy[1];
			}
			else
			{
				bound[0] = bound[1];
				energy[0] = energy[1];
			}
			bound[1] = newmin;
			energy[1] = ecurrent;
		}
		else
		{
			// Is the parabolic point better than the relevant bound in that direction?
			if ((energy[2] - enew) > tolerance_)
			{
				bound[2] = newmin;
				energy[2] = enew;
			}
			else if ((energy[0] - enew) > tolerance_)
			{
				bound[0] = newmin;
				energy[0] = enew;
			}
			else
			{
				// Try Golden Search instead, into the largest of the two sections.
				//printf("d1 %f   d2 %f\n",bound[1] - bound[0],bound[1] - bound[2]);
				leftbound = fabs(bound[0]-bound[1]) > fabs(bound[1]-bound[2]);
				if (leftbound) newmin = bound[1] + 0.3819660 * (bound[1] - bound[0]);
				else newmin = bound[1] + 0.3819660 * (bound[2] - bound[1]);
				gradientMove(srcmodel, &destmodel, newmin);
				enew = srcmodel->totalEnergy(&destmodel, success);
				//printf("GOLD point is %f @ %f \n",enew,newmin);
				if (enew < energy[1])
				{
					//printf("---GOLD point is lower than current minimum...\n");
					// New point found...
					ecurrent = enew;
					// Overwrite the largest of bound[0] and bound[2]
					if (leftbound)
					{
						bound[0] = bound[1];
						energy[2] = energy[1];
					}
					else
					{
						bound[2] = bound[1];
						energy[2] = energy[1];
					}
					bound[1] = newmin;
					energy[1] = ecurrent;
				}
				else
				{
					//printf("---GOLD point is better than one of the boundary values...\n");
					if (leftbound)
					{
						bound[0] = newmin;
						energy[0] = enew;
					}
					else
					{
						bound[2] = newmin;
						energy[2] = enew;
					}
				}
			}
		}
// 		printf("DIFF = %f, 2tol = %f\n", fabs(bound[0]-bound[2]), 2.0 * tolerance_);
	} while (fabs(bound[0]-bound[2]) > (2.0 * tolerance_));
// 	printf("Final bounding values are %f %f %f\n",bound[0],bound[1],bound[2]);
	gradientMove(srcmodel, &destmodel, bound[1]);
	srcmodel->copyAtomData(&destmodel, Atom::PositionData);
	msg.exit("LineMinimiser::minimise");
	return energy[1];
}

