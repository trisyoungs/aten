/*
	*** Forcefield Save
	*** src/ff/saveforcefield.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/kvmap.h"
#include "base/sysfunc.h"
#include "templates/namemap.h"

// Save the specified forcefield
bool Forcefield::save()
{
	msg.enter("Forcefield::save");
	bool done, okay, result = TRUE;
	int success, n, m;
	Prefs::EnergyUnit ffunit;
	LineParser parser;
	
	// Open file for writing
	if (!parser.openOutput(filename_, TRUE))
	{
		msg.print("Couldn't open file '%s' for writing..\n", filename_.get());
		msg.exit("Forcefield::save");
		return FALSE;
	}

	// Write forcefield name and energy units
	parser.writeLineF("name \"%s\"\n\n", name_.get());
	parser.writeLineF("units %s\n\n", Prefs::energyUnit(energyUnit_));

	// Global type definitions
	if (typeDefines_.nItems() != 0)
	{
		parser.writeLine("defines\n");
		Dnchar typedefine;
		for (Neta *neta = typeDefines_.first(); neta != NULL; neta = neta->next)
		{
			neta->netaPrint(typedefine);
			parser.writeLineF("%s\t\"%s\"\n", neta->name(), typedefine.get());
		}
		parser.writeLine("end\n\n");
	}

	// Atomtype definitions
	// Count UATypes first...
	int nUATypes = 0;
	for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next) if (ffa->isUnitedAtom()) ++nUATypes;
	if ((types_.nItems() - nUATypes - 1) > 0)
	{
		parser.writeLine("types\n");
		for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			// Skip UATypes in this pass...
			if (ffa->isUnitedAtom()) continue;
			if (isEmpty(ffa->description())) parser.writeLineF("%i\t%s\t%s\t\"%s\"\n", ffa->typeId(), ffa->name(), Elements().symbol(ffa->element()), ffa->netaString());
			else parser.writeLineF("%i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), ffa->name(), Elements().symbol(ffa->element()), ffa->netaString(), ffa->description());
		}
		parser.writeLine("end\n\n");
	}
	if (nUATypes > 0)
	{
		parser.writeLine("uatypes\n");
		for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			// Skip normal types in this pass...
			if (!ffa->isUnitedAtom()) continue;
				
			if (isEmpty(ffa->description())) parser.writeLineF("%i\t%s\t%s\t%f\t\"%s\"\n", ffa->typeId(), ffa->name(), Elements().symbol(ffa->element()), ffa->elementMass(), ffa->netaString());
			else parser.writeLineF("%i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), ffa->name(), Elements().symbol(ffa->element()), ffa->netaString(), ffa->description());
		}
		parser.writeLine("end\n\n");
	}

	// Atomtype Equivalents
	// Loop over defined atomtypes, checking equivalent name with atomtype name. If different, search the KVMap for the equivalent name. If found, add the atomtype name to the list, otherwise start a new entry and add it to that
	KVMap equivalentMap;
	KVPair *kvp;
	Dnchar tname(100);
	for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next)
	{
		// Are equivalent and type names the same? If so, just continue.
		if (strcmp(ffa->equivalent(),ffa->name()) == 0) continue;
		
		// Equivalent and type names not the same, so search equivalentMap for the equivalent name
		kvp = equivalentMap.search(ffa->equivalent());
		tname.sprintf(" %s ",ffa->name());
		if (kvp == NULL) equivalentMap.add(ffa->equivalent(), tname.get());
		else
		{
			// Does the atomtype name already exist in the list? There probably shouldn't, but check anyway.
			if (strstr(kvp->value(),tname.get()) == 0)
			{
				tname.sprintf("%s %s ", kvp->value(), tname.get());
				kvp->setValue(tname.get());
			}
		}
	}
	// Now, loop over stored lists and write out to file
	if (equivalentMap.nPairs() != 0)
	{
		parser.writeLine("equivalents\n");
		for (kvp = equivalentMap.pairs(); kvp != NULL; kvp = kvp->next) parser.writeLineF("%s\t%s\n", kvp->key(), kvp->value());
		parser.writeLine("end\n\n");
	}

	// Data block
	if (typeData_.nItems() > 0)
	{
		// Write header, including variable type/name definitions
		parser.writeLine("data \"");
		for (NameMap<VTypes::DataType> *nm = typeData_.first(); nm != NULL; nm = nm->next)
		{
			switch (nm->data())
			{
				case (VTypes::IntegerData):
				case (VTypes::DoubleData):
				case (VTypes::StringData):
					parser.writeLineF("%s %s", VTypes::dataType(nm->data()), nm->name());
					break;
				default:
					msg.print("Error: Unsuitable datatype '%s' for data item '%s'.\n", VTypes::dataType(nm->data()), nm->name());
					result = FALSE;
					continue;
			}
			if (nm->next != NULL) parser.writeLine(", ");
		}
		parser.writeLine("\"\n");

		Variable *v;
		ReturnValue rv;
		for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			parser.writeLineF("%i\t%s\t", ffa->typeId(), ffa->name());
			for (NameMap<VTypes::DataType> *nm = typeData_.first(); nm != NULL; nm = nm->next)
			{
				// Find data...
				v = ffa->data(nm->name());
				if (v == NULL)
				{
					msg.print("Warning: Data '%s' has not been defined in type '%s' (id %i).\n", nm->name(), ffa->name(), ffa->typeId());
					switch (nm->data())
					{
						case (VTypes::IntegerData):
							rv.set(0);
							break;
						case (VTypes::DoubleData):
							rv.set(0.0);
							break;
						case (VTypes::StringData):
							rv.set("NULL");
							break;
						default:
							rv.reset();
					}
				}
				else v->execute(rv);
				switch (nm->data())
				{
					case (VTypes::IntegerData):
						parser.writeLineF("%i", rv.asInteger());
						break;
					case (VTypes::DoubleData):
						parser.writeLineF("%e", rv.asDouble());
						break;
					case (VTypes::StringData):
						parser.writeLineF("\"%s\"", rv.asString());
						break;
					default:
						msg.print("Error: Unsuitable datatype '%s' for data item '%s'.\n", VTypes::dataType(nm->data()), nm->name());
						result = FALSE;
						continue;
				}
				if (nm->next != NULL) parser.writeLine("\t");
				else parser.writeLine("\n");
			}
		}
		parser.writeLine("end\n\n");
	}

	// Generator Functions
	if (generatorFunctionText_.nItems() > 0)
	{
		parser.writeLine("function\n");
		for (Dnchar *d = generatorFunctionText_.first(); d != NULL; d = d->next) parser.writeLineF("%s\n", d->get());
		parser.writeLine("end\n\n");
	}
	
	// Intermolecular potential definition
	if (types_.nItems() > 0)
	{
		// First, get populations of specified parameters in each VDW form
		int count[VdwFunctions::nVdwFunctions];
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) count[n] = 0;
		for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next) ++count[ffa->vdwForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("inter %s\n", VdwFunctions::VdwFunctions[n].keyword);
			for (ForcefieldAtom *ffa = types_.second(); ffa != NULL; ffa = ffa->next)
			{
				if (ffa->vdwForm() != n) continue;
				parser.writeLineF("%i\t%s\t%f", ffa->typeId(), ffa->name(), ffa->charge());
				for (m=0; m<VdwFunctions::VdwFunctions[n].nParameters; ++m) parser.writeLineF("\t%f", ffa->parameter(m));
				parser.writeLine("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	// Bond potential definition
	if (bonds_.nItems() > 0)
	{
		// First, get populations of specified parameters in each bond form
		int count[BondFunctions::nBondFunctions];
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = bonds_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("bonds %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound *ffb = bonds_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				parser.writeLineF("%s\t%s", ffb->typeName(0), ffb->typeName(1));
				for (m=0; m<BondFunctions::BondFunctions[n].nParameters; ++m) parser.writeLineF("\t%f", ffb->parameter(m));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	// Angle potential definition
	if (angles_.nItems() > 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[AngleFunctions::nAngleFunctions];
		for (n = 0; n < AngleFunctions::nAngleFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = angles_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->angleForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < AngleFunctions::nAngleFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("angles %s\n", AngleFunctions::AngleFunctions[n].keyword);
			for (ForcefieldBound *ffb = angles_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->angleForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2));
				for (m=0; m<AngleFunctions::AngleFunctions[n].nParameters; ++m) parser.writeLineF("\t%f", ffb->parameter(m));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	// Torsion potential definition
	// TGAY Scaling Factors for torsions - makes things a bit more complicated!
	if (torsions_.nItems() > 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[TorsionFunctions::nTorsionFunctions];
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = torsions_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("torsions %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound *ffb = torsions_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3));
				for (m=0; m<TorsionFunctions::TorsionFunctions[n].nParameters; ++m) parser.writeLineF("\t%f", ffb->parameter(m));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	// Improper torsion potential definition
	if (impropers_.nItems() > 0)
	{
		// First, get populations of specified parameters in each angle form
		int count[TorsionFunctions::nTorsionFunctions];
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = impropers_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("impropers %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound *ffb = impropers_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s\t%s", ffb->typeName(0), ffb->typeName(1), ffb->typeName(2), ffb->typeName(3));
				for (m=0; m<TorsionFunctions::TorsionFunctions[n].nParameters; ++m) parser.writeLineF("\t%f", ffb->parameter(m));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	// Urey-Bradley potential definition
	if (ureyBradleys_.nItems() > 0)
	{
		// First, get populations of specified parameters in each bond form
		int count[BondFunctions::nBondFunctions];
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) count[n] = 0;
		for (ForcefieldBound *ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("ureybradleys %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound *ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				parser.writeLineF("%s\t%s", ffb->typeName(0), ffb->typeName(1));
				for (int n=0; n<BondFunctions::BondFunctions[n].nParameters; ++n) parser.writeLineF("\t%f", ffb->parameter(n));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	msg.print("Done.\n");

	msg.exit("Forcefield::save");
	return TRUE;
}

