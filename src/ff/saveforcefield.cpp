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

#include "ff/forcefield.h"
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"
#include "base/kvmap.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// Save the specified forcefield
bool Forcefield::save()
{
	Messenger::enter("Forcefield::save");
	bool done, okay, result = TRUE;
	int success, n, m;
	Prefs::EnergyUnit ffunit;
	LineParser parser;
	
	// Open file for writing
	if (!parser.openOutput(filename_, TRUE))
	{
		Messenger::print("Couldn't open file '%s' for writing..", qPrintable(filename_));
		Messenger::exit("Forcefield::save");
		return FALSE;
	}

	// Write forcefield name and energy units
	parser.writeLineF("name \"%s\"\n\n", qPrintable(name_));
	parser.writeLineF("units %s\n\n", Prefs::energyUnit(energyUnit_));

	// Global type definitions
	if (typeDefines_.nItems() != 0)
	{
		parser.writeLine("defines\n");
		QString typeDefine;
		for (Neta* neta = typeDefines_.first(); neta != NULL; neta = neta->next)
		{
			neta->netaPrint(typeDefine);
			parser.writeLineF("%s\t\"%s\"\n", qPrintable(neta->name()), qPrintable(typeDefine));
		}
		parser.writeLine("end\n\n");
	}

	// Atomtype definitions
	// Count UATypes first...
	int nUATypes = 0;
	for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next) if (ffa->isUnitedAtom()) ++nUATypes;
	if ((types_.nItems() - nUATypes - 1) > 0)
	{
		parser.writeLine("types\n");
		for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			// Skip UATypes in this pass...
			if (ffa->isUnitedAtom()) continue;
			if (ffa->description().isEmpty()) parser.writeLineF("%i\t%s\t%s\t\"%s\"\n", ffa->typeId(), qPrintable(ffa->name()), Elements().symbol(ffa->element()), qPrintable(ffa->netaString()));
			else parser.writeLineF("%i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), qPrintable(ffa->name()), Elements().symbol(ffa->element()), qPrintable(ffa->netaString()), qPrintable(ffa->description()));
		}
		parser.writeLine("end\n\n");
	}
	if (nUATypes > 0)
	{
		parser.writeLine("uatypes\n");
		for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			// Skip normal types in this pass...
			if (!ffa->isUnitedAtom()) continue;
				
			if (ffa->description().isEmpty()) parser.writeLineF("%i\t%s\t%s\t%f\t\"%s\"\n", ffa->typeId(), qPrintable(ffa->name()), Elements().symbol(ffa->element()), ffa->elementMass(), qPrintable(ffa->netaString()));
			else parser.writeLineF("%i\t%s\t%s\t\"%s\"\t\"%s\"\n", ffa->typeId(), qPrintable(ffa->name()), Elements().symbol(ffa->element()), qPrintable(ffa->netaString()), qPrintable(ffa->description()));
		}
		parser.writeLine("end\n\n");
	}

	// Atomtype Equivalents
	// Loop over defined atomtypes, checking equivalent name with atomtype name. If different, search the KVMap for the equivalent name. If found, add the atomtype name to the list, otherwise start a new entry and add it to that
	KVMap equivalentMap;
	KVPair* kvp;
	for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next)
	{
		// Are equivalent and type names the same? If so, just continue.
		if (ffa->equivalent() == ffa->name()) continue;
		
		// Equivalent and type names not the same, so search equivalentMap for the equivalent name
		kvp = equivalentMap.search(ffa->equivalent());
		if (kvp == NULL) equivalentMap.add(ffa->equivalent(), ffa->name());
		else kvp->setValue(kvp->value() + " " + ffa->name());
	}

	// Now, loop over stored lists and write out to file
	if (equivalentMap.nPairs() != 0)
	{
		parser.writeLine("equivalents\n");
		for (kvp = equivalentMap.pairs(); kvp != NULL; kvp = kvp->next) parser.writeLineF("%s\t%s\n", qPrintable(kvp->key()), qPrintable(kvp->value()));
		parser.writeLine("end\n\n");
	}

	// Data block
	if (typeData_.nItems() > 0)
	{
		// Write header, including variable type/name definitions
		parser.writeLine("data \"");
		for (NameMap<VTypes::DataType>* nm = typeData_.first(); nm != NULL; nm = nm->next)
		{
			switch (nm->data())
			{
				case (VTypes::IntegerData):
				case (VTypes::DoubleData):
				case (VTypes::StringData):
					parser.writeLineF("%s %s", VTypes::dataType(nm->data()), qPrintable(nm->name()));
					break;
				default:
					Messenger::print("Error: Unsuitable datatype '%s' for data item '%s'.", VTypes::dataType(nm->data()), qPrintable(nm->name()));
					result = FALSE;
					continue;
			}
			if (nm->next != NULL) parser.writeLine(", ");
		}
		parser.writeLine("\"\n");

		Variable* v;
		ReturnValue rv;
		for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next)
		{
			parser.writeLineF("%i\t%s\t", ffa->typeId(), qPrintable(ffa->name()));
			for (NameMap<VTypes::DataType>* nm = typeData_.first(); nm != NULL; nm = nm->next)
			{
				// Find data...
				v = ffa->data(nm->name());
				if (v == NULL)
				{
					Messenger::print("Warning: Data '%s' has not been defined in type '%s' (id %i).", qPrintable(nm->name()), qPrintable(ffa->name()), ffa->typeId());
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
						parser.writeLineF("\"%s\"", qPrintable(rv.asString()));
						break;
					default:
						Messenger::print("Error: Unsuitable datatype '%s' for data item '%s'.", VTypes::dataType(nm->data()), qPrintable(nm->name()));
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
	if (generatorFunctionText_.count() > 0)
	{
		parser.writeLine("function\n");
		for (n=0; n<generatorFunctionText_.count(); ++n) parser.writeLineF("%s\n", qPrintable(generatorFunctionText_.at(n)));
		parser.writeLine("end\n\n");
	}
	
	// Intermolecular potential definition
	if (types_.nItems() > 0)
	{
		// First, get populations of specified parameters in each VDW form
		int count[VdwFunctions::nVdwFunctions];
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) count[n] = 0;
		for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next) ++count[ffa->vdwForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < VdwFunctions::nVdwFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("inter %s\n", VdwFunctions::VdwFunctions[n].keyword);
			for (ForcefieldAtom* ffa = types_.second(); ffa != NULL; ffa = ffa->next)
			{
				if (ffa->vdwForm() != n) continue;
				parser.writeLineF("%i\t%s\t%f", ffa->typeId(), qPrintable(ffa->name()), ffa->charge());
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
		for (ForcefieldBound* ffb = bonds_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("bonds %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound* ffb = bonds_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				parser.writeLineF("%s\t%s", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)));
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
		for (ForcefieldBound* ffb = angles_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->angleForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < AngleFunctions::nAngleFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("angles %s\n", AngleFunctions::AngleFunctions[n].keyword);
			for (ForcefieldBound* ffb = angles_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->angleForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)));
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
		for (ForcefieldBound* ffb = torsions_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("torsions %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound* ffb = torsions_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s\t%s", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), qPrintable(ffb->typeName(3)));
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
		for (ForcefieldBound* ffb = impropers_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->torsionForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < TorsionFunctions::nTorsionFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("impropers %s\n", TorsionFunctions::TorsionFunctions[n].keyword);
			for (ForcefieldBound* ffb = impropers_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->torsionForm() != n) continue;
				parser.writeLineF("%s\t%s\t%s\t%s", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)), qPrintable(ffb->typeName(2)), qPrintable(ffb->typeName(3)));
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
		for (ForcefieldBound* ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next) ++count[ffb->bondForm()];

		// Now, write blocks for each form (if necessary)
		for (n = 0; n < BondFunctions::nBondFunctions; ++n) if (count[n] != 0)
		{
			parser.writeLineF("ureybradleys %s\n", BondFunctions::BondFunctions[n].keyword);
			for (ForcefieldBound* ffb = ureyBradleys_.first(); ffb != NULL; ffb = ffb->next)
			{
				if (ffb->bondForm() != n) continue;
				parser.writeLineF("%s\t%s", qPrintable(ffb->typeName(0)), qPrintable(ffb->typeName(1)));
				for (int n=0; n<BondFunctions::BondFunctions[n].nParameters; ++n) parser.writeLineF("\t%f", ffb->parameter(n));
				parser.writeLineF("\n");
			}
			parser.writeLine("end\n\n");
		}
	}

	Messenger::print("Done.");

	Messenger::exit("Forcefield::save");
	return TRUE;
}

