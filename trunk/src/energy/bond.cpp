/*
	*** Bond energy / force calculation
	*** src/energy/bond.cpp

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

#include <math.h>
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "classes/energystore.h"
#include "templates/vector3.h"

// Calculate bond energy of pattern
void pattern::bond_energy(model *srcmodel, energystore *estore)
{
	dbg_begin(DM_CALLS,"pattern::bond_energy");
	int i,j,m1,aoff;
	static vec3<double> mim_i;
	static double forcek, eq, r, energy;
	static ffparams params;
	patbound *pb;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;
	energy = 0.0;
	aoff = startatom;
	//printf("BOND NRG: NAME=%s, START %i, NMOLS %i, NATOMS %i, NBONDS %3i\n",name,startatom,nmols,natoms,nbonds);
	for (m1=0; m1<nmols; m1++)
	{
		for (pb = bonds.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0)+ aoff;
			j = pb->get_atomid(1) + aoff;
			params = pb->get_data()->get_params();
			switch (pb->get_data()->get_funcform().bondfunc)
			{
				case (BF_UNSPECIFIED):
					printf("pattern::bond_energy <<<< Bond function is UNSPECIFIED >>>>\n");
					break;
				case (BF_HARMONIC): 
					// U = 0.5 * forcek * (r - eq)**2
					forcek = params.data[BF_HARMONIC_K];
					forcek = fabs(forcek);
					eq = params.data[BF_HARMONIC_EQ];
					r = cell->distance(modelatoms[i]->r, modelatoms[j]->r);
					r -= eq;
					energy += 0.5 * forcek * r * r;
					break;
			}
		}
		aoff = aoff + natoms;
	}
	// Increment energy for pattern
	estore->add(ET_BOND,energy,id);
	dbg_end(DM_CALLS,"pattern::bond_energy");
}

// Calculate bond forces in pattern
void pattern::bond_forces(model *srcmodel)
{
	dbg_begin(DM_CALLS,"pattern::bond_forces");
	int n,i,j,m1,aoff;
	static vec3<double> mim_i, fi;
	static double forcek, eq, rij, du_dr;
	static ffparams params;
	patbound *pb;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		for (pb = bonds.first(); pb != NULL; pb = pb->next)
		{
			// Calculate bond vector
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(1) + aoff;
			mim_i = cell->mimd(modelatoms[j]->r, modelatoms[i]->r);
			rij = mim_i.magnitude();
			// Select energy function
			params = pb->get_data()->get_params();
			switch (pb->get_data()->get_funcform().bondfunc)
			{
				case (BF_UNSPECIFIED):
					printf("pattern::bond_forces <<<< Bond function is UNSPECIFIED >>>>\n");
					du_dr = 0.0;
					break;
				case (BF_HARMONIC): 
					// F(r) = forcek * (r - eq)
					forcek = params.data[BF_HARMONIC_K];
					eq = params.data[BF_HARMONIC_EQ];
					du_dr = forcek * (rij - eq) / rij;
					break;
			}
			// Calculate forces
			fi = mim_i * du_dr;
			modelatoms[i]->f += fi;
			modelatoms[j]->f -= fi;
		}
		aoff = aoff + natoms;
	}
	dbg_end(DM_CALLS,"pattern::bond_forces");
}
