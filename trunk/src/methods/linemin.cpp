/*
	*** Line minimiser
	*** src/methods/line.cpp
	Copyright T. Youngs 2007

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

// Perform gradient move
void linemin::gradient_move(model *srcmodel, model *destmodel, double delta)
{
	// Generate a new set of coordinates in destmodel following the normalised gradient vector present in srcmodel, with the stepsize given
	dbg_begin(DM_CALLS,"linemin::gradient_move");
	int i;
	atom **srcatoms = srcmodel->get_staticatoms();
	atom **destatoms = destmodel->get_staticatoms();
	for (i=0; i<srcmodel->get_natoms(); i++)
	{
		destatoms[i]->r.x = srcatoms[i]->r.x + srcatoms[i]->f.x * delta;
		destatoms[i]->r.y = srcatoms[i]->r.y + srcatoms[i]->f.y * delta;
		destatoms[i]->r.z = srcatoms[i]->r.z + srcatoms[i]->f.z * delta;
	}
	dbg_end(DM_CALLS,"linemin::gradient_move");
}

// Line minimise supplied model along gradient vector 
void linemin::line_minimise(model *srcmodel, double tol)
{
	dbg_begin(DM_CALLS,"linemin::line_minimise");
	int m, i;
	double enew1, enew2, ecurrent, step, bound[3], energy[3], newmin1, newmin2, a, b, b10, b12;
	model destmodel;
	bool failed;
	atom **modelatoms = srcmodel->get_staticatoms();

	// We assume that the energy expression for the source model is correct
	// Copy source model coordinates so the size of destmodel is the same
	destmodel.copy(srcmodel);
	ecurrent = srcmodel->total_energy(srcmodel);
	srcmodel->energy.print();

	failed = FALSE;

	srcmodel->zero_forces();
	srcmodel->calculate_forces(srcmodel);
	srcmodel->zero_forces_fixed();
	srcmodel->normalise_forces(1.0);

	// Set initial bounding values
	bound[0] = -0.2;
	bound[1] = 0.1;
	bound[2] = 0.2;
	// Compute gradient at each bounding point
	gradient_move(srcmodel, &destmodel, bound[0]);
	energy[0] = srcmodel->total_energy(&destmodel);
	gradient_move(srcmodel, &destmodel, bound[1]);
	energy[1] = srcmodel->total_energy(&destmodel);
	gradient_move(srcmodel, &destmodel, bound[2]);
	energy[2] = srcmodel->total_energy(&destmodel);
	// Sort w.r.t. energy so that the minimum is in the central point.
	if (energy[1] > energy[0])
	{
		enew1 = energy[0];
		energy[0] = energy[1];
		energy[1] = enew1;
		enew1 = bound[0];
		bound[0] = bound[1];
		bound[1] = enew1;
	}
	if (energy[1] > energy[2])
	{
		enew1 = energy[2];
		energy[2] = energy[1];
		energy[1] = enew1;
		enew1 = bound[2];
		bound[2] = bound[1];
		bound[1] = enew1;
	}
	msg(DM_VERBOSE,"Initial bounding values/energies = %f (%f) %f (%f) %f (%f)\n",bound[0],energy[0],bound[1],energy[1],bound[2],energy[2]);

	do
	{
		// Perform linesearch along the gradient vector
		// Set initial three points if this is the first iteration
		printf("Energies = %f %f %f\n",energy[0],energy[1],energy[2]);
		printf("Bounds   = %f %f %f\n",bound[0], bound[1], bound[2]);

		// Perform parabolic interpolation to find new minimium point
		b10 = bound[1] - bound[0];
		b12 = bound[1] - bound[2];
		a = (b10 * b10 * (energy[1] - energy[2])) - (b12 * b12 * (energy[1] - energy[0]));
		b = (b10 * (energy[1] - energy[2])) - (b12 * (energy[1] - energy[0]));
		newmin1 = bound[1] - 0.5 * (a / b);

		// Compute energy of new point and check that it went down...
		gradient_move(srcmodel, &destmodel, newmin1);
		enew1 = srcmodel->total_energy(&destmodel);

		printf("PARABOLIC point gives %f @ %f\n",enew1,newmin1);
		if (enew1 < ecurrent)
		{
			// New point found, so copy destmodel coordinates to model and set new energy
			printf("PARABOLIC point is new minimum...\n");
			ecurrent = enew1;
			srcmodel->copy_atom_data(&destmodel, AD_R);
			// Overwrite the largest of bound[0] and bound[2]
			if (bound[2] > bound[0])
			{
				bound[2] = bound[1];
				energy[2] = energy[1];
			}
			else
			{
				bound[0] = bound[1];
				energy[0] = energy[1];
			}
			bound[1] = newmin1;
			energy[1] = ecurrent;
		}
		else
		{
			// Try Golden Search Instead
			newmin1 = bound[1] + 0.3819660 * (bound[1] - bound[2]);
			gradient_move(srcmodel, &destmodel, newmin1);
			enew1 = srcmodel->total_energy(&destmodel);
			// Try another method
			newmin2 = bound[1] + 0.3819660 * (bound[0] - bound[1]);
			gradient_move(srcmodel, &destmodel, newmin2);
			enew2 = srcmodel->total_energy(&destmodel);
			if (enew2 < enew1)
			{
				enew1 = enew2;
				newmin1 = newmin2;
			}
			printf("Best point found by GOLD is %f @ %f\n",enew1,newmin1);
			if (enew1 < energy[1])
			{
				printf("---GOLD point is lower than current minimum...\n");
				// New point found, so copy destmodel coordinates to model and set new energy
				ecurrent = enew1;
				srcmodel->copy_atom_data(&destmodel, AD_R);
				// Overwrite the largest of bound[0] and bound[2]
				if (bound[2] > bound[0])
				{
					bound[2] = bound[1];
					energy[2] = energy[1];
				}
				else
				{
					bound[0] = bound[1];
					energy[0] = energy[1];
				}
				bound[1] = newmin1;
				energy[1] = ecurrent;
			}
			else
			{
				// Overwrite the largest of bound[0] and bound[2]
				if (enew1 < min(energy[2],energy[0]))
				{
					printf("---GOLD point is better than one of the boundary values...\n");
					if (energy[2] > energy[0])
					{
						bound[2] = newmin1;
						energy[2] = enew1;
					}
					else
					{
						bound[0] = newmin1;
						energy[0] = enew1;
					}
				}
				else failed = TRUE;
			}
		}
	} while ((!failed) && (fabs(bound[0]-bound[2]) > tol));
	
	dbg_end(DM_CALLS,"linemin::minimise");
}

