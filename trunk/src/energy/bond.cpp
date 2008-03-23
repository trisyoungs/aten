/*
	*** Bond energy / force calculation
	*** src/energy/bond.cpp
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

#include <math.h>
#include "templates/vector3.h"
#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "classes/energystore.h"
#include "classes/cell.h"
#include "model/model.h"

// Calculate bond energy of pattern (or molecule in pattern)
void Pattern::bondEnergy(Model *srcmodel, EnergyStore *estore, int molecule)
{
	dbgBegin(DM_CALLS,"Pattern::bondEnergy");
	int i,j,m1,aoff;
	static Vec3<double> mim_i;
	static double forcek, eq, r, energy;
	static ForcefieldParams params;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	//printf("BOND NRG: NAME=%s, START %i, NMOLS %i, NATOMS %i, NBONDS %3i\n",name,startAtom_,nMols_,nAtoms_,nbonds);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMols_ : molecule+1); m1++)
	{
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			params = pb->data()->params();
			switch (pb->data()->functionalForm().bondFunc)
			{
				case (BF_UNSPECIFIED):
					printf("Pattern::bondEnergy <<<< Bond function is UNSPECIFIED >>>>\n");
					break;
				case (BF_HARMONIC): 
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(params.data[BF_HARMONIC_K]);
					eq = params.data[BF_HARMONIC_EQ];
					r = cell->distance(modelatoms[i]->r(), modelatoms[j]->r());
					r -= eq;
					energy += 0.5 * forcek * r * r;
					break;
				default:
					printf("No equation coded for bond energy type %i.\n",pb->data()->functionalForm().bondFunc);
					break;
			}
		}
		aoff = aoff + nAtoms_;
	}
	// Increment energy for pattern
	estore->add(ET_BOND,energy,id_);
	dbgEnd(DM_CALLS,"Pattern::bondEnergy");
}

// Calculate bond forces in pattern
void Pattern::bondForces(Model *srcmodel)
{
	dbgBegin(DM_CALLS,"Pattern::bondForcess");
	int n,i,j,m1,aoff;
	static Vec3<double> mim_i, fi;
	static double forcek, eq, rij, du_dr;
	static ForcefieldParams params;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMols_; m1++)
	{
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			// Calculate bond vector
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			mim_i = cell->mimd(modelatoms[j]->r(), modelatoms[i]->r());
			rij = mim_i.magnitude();
			// Select energy function
			params = pb->data()->params();
			switch (pb->data()->functionalForm().bondFunc)
			{
				case (BF_UNSPECIFIED):
					printf("Pattern::bondForcess <<<< Bond function is UNSPECIFIED >>>>\n");
					du_dr = 0.0;
					break;
				case (BF_HARMONIC): 
					// F(r) = forcek * (r - eq)
					forcek = params.data[BF_HARMONIC_K];
					eq = params.data[BF_HARMONIC_EQ];
					du_dr = forcek * (rij - eq) / rij;
					break;
				default:
					printf("No equation coded for bond forces type %i.\n",pb->data()->functionalForm().bondFunc);
					break;
			}
			// Calculate forces
			fi = mim_i * du_dr;
			modelatoms[i]->f() += fi;
			modelatoms[j]->f() -= fi;
		}
		aoff += nAtoms_;
	}
	dbgEnd(DM_CALLS,"Pattern::bondForcess");
}
