/*
	*** Forcefield export
	*** src/ff/saveforcefield.cpp
	Copyright T. Youngs 2007-2011

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied wareadVdwanty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <fstream>
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "classes/forcefieldbound.h"
#include "templates/namemap.h"

// Save the specified forcefield
bool Forcefield::save(const char *filename)
{
	msg.enter("Forcefield::save");
	bool done, okay;
	int success, n;
	Prefs::EnergyUnit ffunit;
	// Open file for writing
	ffparser.openFile(filename, TRUE);

	// Write forcefield name and energy units
 	ffparser.writeLineF("name \"%s\"\n", name_.get());
	ffparser.writeLineF("units %s\n", Prefs::energyUnit(energyUnit_));

	// Global type definitions
	if (typeDefines_.nItems() != 0)
	{
		ffparser.writeLine("defines");
		Dnchar typedefine;
		for (Neta *neta = typeDefines_.first(); neta != NULL; neta = neta->next)
		{
			neta->netaPrint(typedefine);
			ffparser.writeLineF("%s\t\"%s\"\n", neta->name(), typedefine.get());
		}
		ffparser.writeLine("end\n");
	}

	// Atomtype definitions
	// TODO TGAY How to distinguish between types and UAtypes?
	if (types_.nItems() != 0)
	{
		for (ForcefieldAtom *ffa = types_.first(); ffa != NULL; ffa = ffa->next)
		{
			if (ffa->description() == NULL) ffparser.writeLineF("%i\t%s\t%s\t\"%s\"\n", ffa->typeId(), ffa->name(), elements().symbol(ffa->element()), ffa->netaString());
			else ffparser.writeLineF("%i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), ffa->name(), elements().symbol(ffa->element()), ffa->netaString(), ffa->description());
		}
	}
	ffparser.writeLine("end\n");

	// Atomtype Equivalents
	// TGAY The original lines from the file will need to be stored in the Forcefield Structure, and so editing in the GUI needs to be allowed

	// Data block *and* Convert list
	// TGAY Needs storage of original data types and names (NameMap) in FF structure (convert list already stored?)

	// Functions
	// TGAY How to write this? Probably best to store original lines from file rather than regenerate from stored Trees.
	
	// Intermolecular potential definition
	if (types_.nItems() != 0)
	{
		// First, get populations of specified parameters in each VDW form
		int count[VdwFunctions::nVdwFunctions];
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) count[n] = 0;
		for (ForcefieldAtom *ffa = types_.first(); ffa != NULL; ffa = ffa->next) ++count[ffa->vdwForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("bonds %s\n", VdwFunctions::VdwFunctions[n].keyword);
			for (ForcefieldAtom *ffa = types_.first(); ffa != NULL; ffa = ffa->next)
			{
				if (ffa->vdwForm() != n) continue;
				ffparser.writeLineF("%i\t%s\t%f", ffa->typeId(), ffa->name(), ffa->charge());
				for (int n=0; n<VdwFunctions::VdwFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffa->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	// Bond potential definition
	if (bonds_.nItems() != 0)
	{
		// First, get populations of specified parameters in each bond form
		int count[BondFunctions::nBondFunctions];
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = bonds_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("bonds %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound *ffb = bonds_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				ffparser.writeLineF("%s\t%s", ffb->typeName(0), ffb->typeName(1));
				for (int n=0; n<BondFunctions::BondFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffb->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	// Angle potential definition
	if (angles_.nItems() != 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[AngleFunctions::nAngleFunctions];
		for (n = 0; n < AngleFunctions::nAngleFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = angles_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->angleForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < AngleFunctions::nAngleFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("angles %s\n", AngleFunctions::AngleFunctions[n].keyword);
			for (ForcefieldBound *ffb = angles_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->angleForm() != n) continue;
				ffparser.writeLineF("%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2));
				for (int n=0; n<AngleFunctions::AngleFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffb->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	// Torsion potential definition
	// TGAY Scaling Factors for torsions - makes things a bit more complicated!
	if (torsions_.nItems() != 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[TorsionFunctions::nTorsionFunctions];
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = torsions_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("torsions %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound *ffb = torsions_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				ffparser.writeLineF("%s\t%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3));
				for (int n=0; n<TorsionFunctions::TorsionFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffb->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	// Improper torsion potential definition
	if (impropers_.nItems() != 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[TorsionFunctions::nTorsionFunctions];
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = impropers_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("impropers %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound *ffb = impropers_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				ffparser.writeLineF("%s\t%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3));
				for (int n=0; n<TorsionFunctions::TorsionFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffb->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	// Urey-Bradley potential definition
	if (ureyBradleys_.nItems() != 0)
	{
		// First, get populations of specified parameters in each bond form
		int count[BondFunctions::nBondFunctions];
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			ffparser.writeLineF("ureybradleys %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound *ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				ffparser.writeLineF("%s\t%s", ffb->typeName(0), ffb->typeName(1));
				for (int n=0; n<BondFunctions::BondFunctions[n].nParameters; ++n) ffparser.writeLineF("\t%f", ffb->parameter(n));
				ffparser.writeLineF("\n");
			}
			ffparser.writeLineF("end\n\n");
		}
	}

	msg.exit("Forcefield::save");
	return TRUE;
}

