/*
	*** Bond energy / force calculation
	*** src/ff/bond.cpp
	Copyright T. Youngs 2007-2017

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
#include "base/forcefieldbound.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Calculate bond energy of pattern (or molecule in pattern)
void Pattern::bondEnergy(Model* srcmodel, EnergyStore* estore, int molecule)
{
	Messenger::enter("Pattern::bondEnergy");
	int i, j, m1, aoff;
	double forcek, eq, rij, energy, ubenergy, bondenergy, d, expo, beta;
	ForcefieldBound* ffb;
	PatternBound* pb;
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	//printf("BOND NRG: NAME=%s, START %i, NMOLS %i, NATOMS %i, NBONDS %3i\n",name,startAtom_,nMolecules_,nAtoms_,nbonds);
	bondenergy = 0.0;
	ubenergy = 0.0;
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule+1); m1++)
	{
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			ffb = pb->data();
			rij = cell.distance(modelatoms[i]->r(), modelatoms[j]->r());
			switch (pb->data()->bondForm())
			{
				case (BondFunctions::None):
					Messenger::print("Warning: No function is specified for bond energy %i-%i.", i, j);
				case (BondFunctions::Ignore):
					energy = 0.0;
					break;
				case (BondFunctions::Constraint):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(ffb->parameter(BondFunctions::ConstraintK));
					eq = ffb->parameter(BondFunctions::ConstraintEq);
					rij -= eq;
					energy = 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Harmonic):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(ffb->parameter(BondFunctions::HarmonicK));
					eq = ffb->parameter(BondFunctions::HarmonicEq);
					rij -= eq;
					energy = 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Morse):
					// U = E0 * (1 - exp( -B(rij - r0) ) )**2
					d = ffb->parameter(BondFunctions::MorseD);
					beta = fabs(ffb->parameter(BondFunctions::MorseK));
					eq = ffb->parameter(BondFunctions::MorseEq);
					rij -= eq;
					expo = 1.0 - exp( -beta * rij );
					energy = d * ( expo*expo );
					break;
				default:
					Messenger::print("No equation coded for bond energy of type '%s'.", BondFunctions::functionData[pb->data()->bondForm()].name);
					energy = 0.0;
					break;
			}
			// Accumulate
			if (ffb->type() == ForcefieldBound::BondInteraction) bondenergy += energy;
			else ubenergy += energy;
		}
		aoff = aoff + nAtoms_;
	}
	// Increment energy for pattern
	estore->add(EnergyStore::BondEnergy,bondenergy,id_);
	estore->add(EnergyStore::UreyBradleyEnergy,ubenergy,id_);
	Messenger::exit("Pattern::bondEnergy");
}

// Calculate bond forces in pattern
void Pattern::bondForces(Model* srcmodel)
{
	Messenger::enter("Pattern::bondForcess");
	int i, j, m1, aoff;
	static Vec3<double> vec_ij, fi;
	static double forcek, eq, rij, d, expo, du_dr, beta;
	static ForcefieldBound* ffb;;
	PatternBound* pb;
	Atom** modelatoms = srcmodel->atomArray();
	UnitCell& cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			// Calculate bond vector
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			vec_ij = cell.mimVector(modelatoms[i]->r(), modelatoms[j]->r());
			rij = vec_ij.magnitude();
			// Select energy function
			ffb = pb->data();
			switch (pb->data()->bondForm())
			{
				case (BondFunctions::None):
					Messenger::print("Warning: No function is specified for bond force %i-%i.", i, j);
				case (BondFunctions::Ignore):
					du_dr = 0.0;
					break;
				case (BondFunctions::Constraint):
					// dU/dr = forcek * (r - eq)
					forcek = ffb->parameter(BondFunctions::ConstraintK);
					eq = ffb->parameter(BondFunctions::ConstraintEq);
					du_dr = forcek * (rij - eq);
					break;
				case (BondFunctions::Harmonic):
					// dU/dr = forcek * (r - eq)
					forcek = ffb->parameter(BondFunctions::HarmonicK);
					eq = ffb->parameter(BondFunctions::HarmonicEq);
					du_dr = forcek * (rij - eq);
					break;
				case (BondFunctions::Morse):
					// dU/dr = 2 * beta * E0 * (1 - exp( -k(rij - r0) ) ) * exp( -k*(rij - r0) )
					d = ffb->parameter(BondFunctions::MorseD);
					beta = fabs(ffb->parameter(BondFunctions::MorseK));
					eq = ffb->parameter(BondFunctions::MorseEq);
					expo = exp( -beta * (rij - eq) );
					du_dr = 2.0 * beta * d * (1.0 - expo) * expo;
					break;
				default:
					Messenger::print("No equation coded for bond forces of type '%s'.", BondFunctions::functionData[pb->data()->bondForm()].name);;
					break;
			}
			// Calculate forces
			fi = (vec_ij / rij) * -du_dr;
			modelatoms[i]->f() -= fi;
			modelatoms[j]->f() += fi;
		}
		aoff += nAtoms_;
	}
	Messenger::exit("Pattern::bondForcess");
}
