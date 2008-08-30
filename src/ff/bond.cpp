/*
	*** Bond energy / force calculation
	*** src/ff/bond.cpp
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

#include "model/model.h"
#include "classes/forcefieldbound.h"
#include "base/pattern.h"

// Calculate bond energy of pattern (or molecule in pattern)
void Pattern::bondEnergy(Model *srcmodel, Energy *estore, int molecule)
{
	msg.enter("Pattern::bondEnergy");
	int i, j, m1, aoff;
	//static Vec3<double> mim_i;
	static double forcek, eq, rij, energy, d, expo;
	static ForcefieldParams params;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	//printf("BOND NRG: NAME=%s, START %i, NMOLS %i, NATOMS %i, NBONDS %3i\n",name,startAtom_,nMolecules_,nAtoms_,nbonds);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule+1); m1++)
	{
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			params = pb->data()->params();
			rij = cell->distance(modelatoms[i]->r(), modelatoms[j]->r());
			switch (pb->data()->bondStyle())
			{
				case (BondFunctions::None):
					msg.print("Warning: No function is specified for bond energy %i-%i.\n", i, j);
					break;
				case (BondFunctions::Constraint):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(params.data[BondFunctions::ConstraintK]);
					eq = params.data[BondFunctions::ConstraintEq];
					rij -= eq;
					energy += 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Harmonic):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(params.data[BondFunctions::HarmonicK]);
					eq = params.data[BondFunctions::HarmonicEq];
					rij -= eq;
					energy += 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Morse):
					// U = E0 * ( (1 - exp( -k(rij - r0) ) )**2 - 1)
					d = params.data[BondFunctions::MorseD];
					forcek = fabs(params.data[BondFunctions::MorseK]);
					eq = params.data[BondFunctions::MorseEq];
					rij -= eq;
					expo = 1.0 - exp( -forcek * rij );
					energy += d * ( expo*expo - 1.0);
					break;
				case (BondFunctions::Morse2):
					// U = E0 * ( (exp( -k(rij - r0) ) - 1)**2)
					d = params.data[BondFunctions::MorseD];
					forcek = fabs(params.data[BondFunctions::MorseK]);
					eq = params.data[BondFunctions::MorseEq];
					rij -= eq;
					expo = exp( -forcek * rij ) - 1.0;
					energy += d * expo * expo;
					break;
				default:
					msg.print( "No equation coded for bond energy of type '%s'.\n", BondFunctions::BondFunctions[pb->data()->bondStyle()].name);;
					break;
			}
		}
		aoff = aoff + nAtoms_;
	}
	// Increment energy for pattern
	estore->add(Energy::BondEnergy,energy,id_);
	msg.exit("Pattern::bondEnergy");
}

// Calculate bond forces in pattern
void Pattern::bondForces(Model *srcmodel)
{
	msg.enter("Pattern::bondForcess");
	int i, j, m1, aoff;
	static Vec3<double> mim_i, fi;
	static double forcek, eq, rij, d, expo, du_dr;
	static ForcefieldParams params;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
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
			switch (pb->data()->bondStyle())
			{
				case (BondFunctions::None):
					msg.print("Warning: No function is specified for bond force %i-%i.\n", i, j);
					du_dr = 0.0;
					break;
				case (BondFunctions::Constraint):
					// dU/dr = forcek * (r - eq)
					forcek = params.data[BondFunctions::ConstraintK];
					eq = params.data[BondFunctions::ConstraintEq];
					du_dr = forcek * (rij - eq);
					break;
				case (BondFunctions::Harmonic):
					// dU/dr = forcek * (r - eq)
					forcek = params.data[BondFunctions::HarmonicK];
					eq = params.data[BondFunctions::HarmonicEq];
					du_dr = forcek * (rij - eq);
					break;
				case (BondFunctions::Morse):
					// dU/dr = 2.0 * k * E0 * (1 - exp( -k(rij - r0) ) ) * exp( -k*(rij - r0) )
					d = params.data[BondFunctions::MorseD];
					forcek = fabs(params.data[BondFunctions::MorseK]);
					eq = params.data[BondFunctions::MorseEq];
					expo = exp( -forcek * (rij - eq) );
					du_dr = 2.0 * forcek * d * (1.0 - expo) * expo;
					break;
				case (BondFunctions::Morse2):
					// dU/dR = -2 * E0 * k * exp( -k*(rij - e0) ) * ( exp -k*(rij - e0) - 1)
					d = params.data[BondFunctions::MorseD];
					forcek = fabs(params.data[BondFunctions::MorseK]);
					eq = params.data[BondFunctions::MorseEq];
					expo = exp( -forcek * (rij - eq) );
					du_dr = -2.0 * d * forcek * expo * (expo - 1.0);
					break;
				default:
					msg.print( "No equation coded for bond forces of type '%s'.\n", BondFunctions::BondFunctions[pb->data()->bondStyle()].name);;
					break;
			}
			// Calculate forces
			fi = (mim_i / rij) * -du_dr;
			modelatoms[i]->f() -= fi;
			modelatoms[j]->f() += fi;
		}
		aoff += nAtoms_;
	}
	msg.exit("Pattern::bondForcess");
}
