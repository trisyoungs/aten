/*
	*** Urey-Bradley energy / force calculation
	*** src/ff/ureybradley.cpp
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
#include "classes/forcefieldbound.h"
#include "base/pattern.h"

// Calculate Urey-Bradley energy of pattern (or molecule in pattern)
void Pattern::ureyBradleyEnergy(Model *srcmodel, EnergyStore *estore, int molecule)
{
	msg.enter("Pattern::ureyBradleyEnergy");
	int i, j, m1, aoff;
	static double forcek, eq, rij, energy, d, expo, beta;
	static ForcefieldBound *ffb;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule+1); m1++)
	{
		for (pb = ureyBradleys_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			ffb = pb->data();
			rij = cell->distance(modelatoms[i]->r(), modelatoms[j]->r());
			switch (pb->data()->bondForm())
			{
				case (BondFunctions::None):
					msg.print("Warning: No function is specified for Urey-Bradley energy %i-%i.\n", i, j);
				case (BondFunctions::Ignore):
					break;
				case (BondFunctions::Constraint):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(ffb->parameter(BondFunctions::ConstraintK));
					eq = ffb->parameter(BondFunctions::ConstraintEq);
					rij -= eq;
					energy += 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Harmonic):
					// U = 0.5 * forcek * (r - eq)**2
					forcek = fabs(ffb->parameter(BondFunctions::HarmonicK));
					eq = ffb->parameter(BondFunctions::HarmonicEq);
					rij -= eq;
					energy += 0.5 * forcek * rij * rij;
					break;
				case (BondFunctions::Morse):
					// U = E0 * ( (1 - exp( -k(rij - r0) ) )**2 - 1)
					d = ffb->parameter(BondFunctions::MorseD);
					beta = fabs(ffb->parameter(BondFunctions::MorseB));
					eq = ffb->parameter(BondFunctions::MorseEq);
					rij -= eq;
					expo = 1.0 - exp( -beta * rij );
					energy += d * ( expo*expo - 1.0);
					break;
				default:
					msg.print( "No equation coded for Urey-Bradley energy of type '%s'.\n", BondFunctions::BondFunctions[pb->data()->bondForm()].name);;
					break;
			}
		}
		aoff = aoff + nAtoms_;
	}
	// Increment energy for pattern
	estore->add(EnergyStore::UreyBradleyEnergy,energy,id_);
	msg.exit("Pattern::ureyBradleyEnergy");
}

// Calculate Urey-Bradley forces in pattern
void Pattern::ureyBradleyForces(Model *srcmodel)
{
	msg.enter("Pattern::ureyBradleyForcess");
	int i, j, m1, aoff;
	static Vec3<double> mim_i, fi;
	static double forcek, eq, rij, d, expo, du_dr, beta;
	static ForcefieldBound *ffb;;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		for (pb = ureyBradleys_.first(); pb != NULL; pb = pb->next)
		{
			// Calculate atom-atom vector
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			mim_i = cell->mimd(modelatoms[j]->r(), modelatoms[i]->r());
			rij = mim_i.magnitude();
			// Select energy function
			ffb = pb->data();
			switch (pb->data()->bondForm())
			{
				case (BondFunctions::None):
					msg.print("Warning: No function is specified for Urey-Bradley force %i-%i.\n", i, j);
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
					// dU/dr = 2.0 * k * E0 * (1 - exp( -k(rij - r0) ) ) * exp( -k*(rij - r0) )
					d = ffb->parameter(BondFunctions::MorseD);
					beta = fabs(ffb->parameter(BondFunctions::MorseB));
					eq = ffb->parameter(BondFunctions::MorseEq);
					expo = exp( -beta * (rij - eq) );
					du_dr = 2.0 * beta * d * (1.0 - expo) * expo;
					break;
				default:
					msg.print( "No equation coded for Urey-Bradley forces of type '%s'.\n", BondFunctions::BondFunctions[pb->data()->bondForm()].name);;
					break;
			}
			// Calculate forces
			fi = (mim_i / rij) * -du_dr;
			modelatoms[i]->f() -= fi;
			modelatoms[j]->f() += fi;
		}
		aoff += nAtoms_;
	}
	msg.exit("Pattern::ureyBradleyForcess");
}
