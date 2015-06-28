/*
	*** Forcefield import
	*** src/ff/loadforcefield.cpp
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
#include "base/neta_parser.h"

ATEN_USING_NAMESPACE

// Load the specified forcefield
bool Forcefield::load(QString filename)
{
	Messenger::enter("Forcefield::load");
	bool done, okay;
	int success, n;
	Prefs::EnergyUnit ffunit;
	// Open file for reading
	ffparser.openInput(filename);
	// Store the filename of the forcefield
	filename_ = filename;
	// Now follows blocks of keywords
	done = false;
	Messenger::print("Opening forcefield : %s...", qPrintable(filename));
	do
	{
		okay = false;
		success = ffparser.getArgsDelim(LineParser::UseQuotes+LineParser::SkipBlanks);
		if (success == 1)
		{
			Messenger::print("Error reading FF directive.");
			ffparser.closeFiles();
			Messenger::exit("Forcefield::load");
			return false;
		}
		if (success == -1) break;
		// Call subroutines to read in data based on keywords
		switch (forcefieldCommand(ffparser.argc(0)))
		{
			case (Forcefield::MessageCommand):
				Messenger::print("####\t %s", qPrintable(ffparser.argc(1)));
				okay = true;
				break;
			case (Forcefield::NameCommand):
				name_ = ffparser.argc(1);
				Messenger::print("\t: '%s'", qPrintable(name_));
				okay = true;
				break;
			case (Forcefield::UnitsCommand):
				ffunit = Prefs::energyUnit(ffparser.argc(1));
				if (ffunit != Prefs::nEnergyUnits)
				{
					energyUnit_ = ffunit;
					Messenger::print("\t: Energy units are %s", Prefs::energyUnit(energyUnit_));
					okay = true;
				}
				else okay = false;
				break;
			case (Forcefield::DefinesCommand):
				okay = readDefines();
				break;
			case (Forcefield::UATypesCommand):
				okay = readUnitedAtomTypes();
				break;
			case (Forcefield::TypesCommand):
				okay = readTypes();
				break;
			case (Forcefield::DataCommand):
				okay = readData(ffparser.argc(1));
				break;
			case (Forcefield::EquivalentsCommand):
				if (types_.nItems() == 0) Messenger::print("\nWarning - Equivalent definitions have been provided before any types have been defined.\n");
				okay = readEquivalents();
				break;
			case (Forcefield::FunctionCommand):
				okay = readFunctions();
				break;
			case (Forcefield::ConvertCommand):
				// Add simple list of energetic data parameters
				for (n=1; n<ffparser.nArgs(); n++) addEnergyData(ffparser.argc(n));
				okay = true;
				break;
			case (Forcefield::VdwCommand):
				Messenger::print("The 'vdw' keyword is deprecated - use 'inter' instead.");
			case (Forcefield::InterCommand):
				okay = readInter();
				break;
			case (Forcefield::BondsCommand):
				okay = readBonds();
				break;
			case (Forcefield::AnglesCommand):
				okay = readAngles();
				break;
			case (Forcefield::TorsionsCommand):
				okay = readTorsions();
				break;
			case (Forcefield::ImproperCommand):
				okay = readImpropers();
				break;
			case (Forcefield::UreyBradleyCommand):
				okay = readUreyBradley();
				break;
			case (Forcefield::VScaleCommand):
				Messenger::print("Error: Use of 'vscale' command is deprecated.\n\tSpecify 1-4 scaling factors in the torsion block header.");
				okay = false;
				break;
			case (Forcefield::EScaleCommand):
				Messenger::print("Error: Use of 'escale' command is deprecated.\n\tSpecify 1-4 scaling factors in the torsion block header.");
				okay = false;
				break;
			default:
				Messenger::print("Unrecognised forcefield keyword '%s'.", qPrintable(ffparser.argc(0)));
				break;
		}
		// Check on 'okay'
		if (!okay)
		{
			//Messenger::print("EreadVdwor reading forcefield file. Aborted.");
			Messenger::print("Error at line %i of file.", ffparser.lastLineNo());
			Messenger::exit("Forcefield::load");
			ffparser.closeFiles();
			return false;
		}
	} while (okay);
	ffparser.closeFiles();

	// Check that some forcefield types were defined...
	if (types_.nItems() <= 1) Messenger::print("Warning - no types are defined in this forcefield.");

	// Link forcefield type references (&N) to their actual forcefield types
	for (ForcefieldAtom* ffa = types_.first(); ffa != NULL; ffa = ffa->next) ffa->neta()->linkReferenceTypes();
	// Last thing - convert energetic units in the forcefield to the internal units of the program
	convertParameters();
	Messenger::exit("Forcefield::load");
	return true;
}

// Read in forcefield type defines
bool Forcefield::readDefines()
{
	Messenger::enter("Forcefield::readDefines");
	int success, nadded = 0;
	bool done;
	Neta* neta;
	done = false;
	// Format of lines is 'name "neta"'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading atom type defines %i.", types_.nItems());
			if (success == -1) Messenger::print("End of file while reading atom type defines %i.", types_.nItems());
			Messenger::exit("Forcefield::readDefines");
			return false;
		}
		else if (ffparser.argc(0) == "end") break;

		// Search for this define name to make sure it hasn't already been used
		for (neta = typeDefines_.first(); neta != NULL; neta = neta->next) if (ffparser.argc(0) == neta->name()) break;
		if (neta != NULL)
		{
			Messenger::print("Error: Duplicate type define name specified (%s) at line %i.", qPrintable(ffparser.argc(0)), ffparser.lastLineNo());
			Messenger::exit("Forcefield::readDefines");
			return false;
		}
		neta = typeDefines_.add();
		nadded ++;
		neta->setName(ffparser.argc(0));
		neta->setParentForcefield(this);
		if (!netaparser.createNeta(neta, ffparser.argc(1), this))
		{
			Messenger::print("Error parsing type define at line %i.", ffparser.lastLineNo());
			Messenger::exit("Forcefield::readDefines");
			return false;
		}
	} while (!done);
	if (nadded == 0) Messenger::print("Warning - No atype defines specified in this block (at line %i)!", ffparser.lastLineNo());
	else Messenger::print("\t: Read in %i type defines", nadded);
	Messenger::exit("Forcefield::readDefines");
	return true;
}

// Read in forcefield atom types.
bool Forcefield::readTypes()
{
	Messenger::enter("Forcefield::readTypes");
	int success, newffid, nadded = 0;
	bool done;
	ForcefieldAtom* ffa, *idsearch;
	done = false;
	// Format of lines is 'ffid typename element description [text]'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading atom type description %i.", types_.nItems());
			if (success == -1) Messenger::print("End of file while reading atom type description %i.", types_.nItems());
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		else if (ffparser.argc(0) == "end") break;

		// Search for this ID to make sure it hasn't already been used
		newffid = ffparser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			Messenger::print("Duplicate forcefield type ID '%i' - already used by type '%s'.", newffid, qPrintable(idsearch->name()));
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		ffa = types_.add();
		++nadded;
		ffa->setParent(this);
		ffa->setTypeId(newffid);

		// Check number of items on line in file
		if (ffparser.nArgs() < 4)
		{
			Messenger::print("Error: Missing data in 'types' block.\n\tFormat of lines in block is 'ffid  typename  element   NETA  [description]'");
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		ffa->setName(ffparser.argc(1));
		int el = Elements().find(ffparser.argc(2), ElementMap::AlphaZMap);
		if (el == 0)
		{
			Messenger::print("Error: Unrecognised element '%s' found for forcefield type '%s' (%i).", qPrintable(ffparser.argc(2)), qPrintable(ffa->name()), ffa->typeId());
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		ffa->setElement(el);
		ffa->setEquivalent(ffparser.argc(1));
		ffa->neta()->setCharacterElement(el);
		if (!ffa->setNeta(ffparser.argc(3), this))
		{
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		if (ffparser.hasArg(4)) ffa->setDescription(ffparser.argc(4));
	} while (!done);
	if (nadded == 0) Messenger::print("Warning - No atom types specified in this block (at line %i)!", ffparser.lastLineNo());
	else Messenger::print("\t: Read in %i type descriptions", nadded);
	Messenger::exit("Forcefield::readTypes");
	return true;
}

// Read in united atom forcefield types.
bool Forcefield::readUnitedAtomTypes()
{
	Messenger::enter("Forcefield::readUnitedAtomTypes");
	int success, newffid, nadded = 0;
	bool done;
	ForcefieldAtom* ffa, *idsearch;
	done = false;
	// Format of lines is 'ffid typename element description [text]'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading united atom type description %i.", types_.nItems());
			if (success == -1) Messenger::print("End of file while reading united atom type description %i.", types_.nItems());
			Messenger::exit("Forcefield::readUnitedAtomTypes");
			return false;
		}
		else if (ffparser.argc(0) == "end") break;
		// Search for this ID to make sure it hasn't already been used
		newffid = ffparser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			Messenger::print("Duplicate forcefield type ID '%i' - already used by type '%s'.", newffid, qPrintable(idsearch->name()));
			Messenger::exit("Forcefield::readTypes");
			return false;
		}
		ffa = types_.add();
		nadded ++;
		ffa->setParent(this);
		ffa->setTypeId(newffid);
		// Check number of items on line in file
		if (ffparser.nArgs() < 5)
		{
			Messenger::print("Error: Missing data in 'uatypes' block.\n\tFormat of lines in block is 'ffid  typename  element  mass  NETA  [description]'");
			Messenger::exit("Forcefield::readUnitedAtomTypes");
			return false;
		}
		ffa->setName(ffparser.argc(1));
		ffa->setEquivalent(ffparser.argc(1));
		int el = Elements().find(ffparser.argc(2), ElementMap::AlphaZMap);
		if (el == 0)
		{
			Messenger::print("Error: Unrecognised element '%s' found for forcefield type '%s' (%i).", qPrintable(ffparser.argc(2)), qPrintable(ffa->name()), ffa->typeId());
			Messenger::exit("Forcefield::readUnitedAtomTypes");
			return false;
		}
		ffa->setElement(el);
		ffa->setElementMass(ffparser.argd(3));
		ffa->neta()->setCharacterElement(Elements().find(ffparser.argc(2), ElementMap::AlphaZMap));
		if (!ffa->setNeta(ffparser.argc(4), this))
		{
			Messenger::exit("Forcefield::readUnitedAtomTypes");
			return false;
		}
		if (ffparser.hasArg(5)) ffa->setDescription(ffparser.argc(4));
	} while (!done);
	if (nadded == 0) Messenger::print("Warning - No united atom types specified in this block (at line %i)!", ffparser.lastLineNo());
	else Messenger::print("\t: Read in %i united-atom type descriptions", nadded);
	Messenger::exit("Forcefield::readUnitedAtomTypes");
	return true;
}

// Reads in extra data for atoms
bool Forcefield::readData(QString vars)
{
	Messenger::enter("Forcefield::readData");

	// Store current number of data defined, since there may be multiple 'data' blocks given
	int lastn = typeData_.nItems();

	// First, parse list of data items to get name
	typeData_.setDefaultValue(VTypes::nDataTypes);
	LineParser parser;
	parser.getArgsDelim(LineParser::SkipBlanks, vars);
	int n;
	for (n=0; n<parser.nArgs(); n += 2)
	{
		// Determine data type
		VTypes::DataType vt = VTypes::dataType(parser.argc(n));
		if (vt == VTypes::nDataTypes)
		{
			Messenger::print("Unrecognised type ('%s') found in list in 'data' block header.", qPrintable(parser.argc(0)));
			Messenger::exit("Forcefield::readData");
			return false;
		}
		typeData_.add(parser.argc(n+1), vt);
	}

	// Next, each line contains the forcefield atom to which the data relates, the type name (for bookkeeping, nothing more)
	// and then the data in the order specified in the header above.
	bool done = false;
	int nadded = 0, success, index;
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading atom type data.");
			if (success == -1) Messenger::print("End of file while reading atom type data.");
			Messenger::exit("Forcefield::readData");
			return false;
		}
		else if (ffparser.argc(0) == "end") break;

		// Search for this ffatom ID and retrieve it
		ForcefieldAtom* ffa = findType(ffparser.argi(0));
		if (ffa == NULL)
		{
			Messenger::print("Error: forcefield type ID '%i' has not been specified, so can't add data to it.", ffparser.argi(0));
			Messenger::exit("Forcefield::readData");
			return false;
		}
		nadded ++;

		// Get data from lines in the order it was specified above
		for (n=2; n<ffparser.nArgs(); ++n)
		{
			// If the parser argument is blank we've run out of arguments early
			if (!ffparser.hasArg(n))
			{
				Messenger::print("Warning: Forcefield atom id %i (%s) has an incomplete set of data (line %i in file).", ffa->typeId(), qPrintable(ffa->name()), ffparser.lastLineNo());
				continue;
			}
			index = lastn + n - 2;
			switch (typeData_.data(index))
			{
				case (VTypes::IntegerData):
					ffa->addData(typeData_.name(index), ffparser.argi(n));
					break;
				case (VTypes::DoubleData):
					ffa->addData(typeData_.name(index), ffparser.argd(n));
					break;
				case (VTypes::StringData):
					ffa->addData(typeData_.name(index), ffparser.argc(n));
					break;
				default:
					Messenger::print("Error: Unsuitable datatype for data item %i.", n-1);
					Messenger::exit("Forcefield::readData");
					return true;
			}
		}
	} while (!done);
	if (nadded == 0) Messenger::print("Warning - No data specified in this block (at line %i)!", ffparser.lastLineNo());
	else Messenger::print("\t: Read in data for %i types", nadded);
	Messenger::exit("Forcefield::readData");
	return true;
}

// Read in generator function definitions
bool Forcefield::readFunctions()
{
	Messenger::enter("Forcefield::readFunctions");

	// Store every line from the file up to the next 'end' block
	bool done = false;
	generatorFunctionText_.clear();
	int success;
	do
	{
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading function block.");
			if (success == -1) Messenger::print("End of file while reading function block.");
			Messenger::exit("Forcefield::readFunctions");
			return false;
		}
		if (ffparser.argc(0) != "end")
		{
			// Add line to internal list and continue
			generatorFunctionText_ << ffparser.line();
		}
		else done = true;
	} while (!done);

	// Check for empty string list
	if (generatorFunctionText_.count() == 0)
	{
		Messenger::print("Found an empty 'functions' block - ignored.");
		Messenger::exit("Forcefield::readFunctions");
		return true;
	}

	// Now, attempt to parser the lines we just read in to create functions....
	bool result = generatorFunctions_.generateFromStringList(generatorFunctionText_, "GeneratorFuncs", "Generator Function", false);

	// Search for functions we recognise
	vdwGenerator_ = generatorFunctions_.findFunction("vdwgenerator");
	if (vdwGenerator_) Messenger::print("\t: Found 'vdwgenerator' function.");
	else Messenger::print("\t: Warning - No 'vdwgenerator' function defined.");
	bondGenerator_ = generatorFunctions_.findFunction("bondgenerator");
	if (bondGenerator_) Messenger::print("\t: Found 'bondgenerator' function.");
	else Messenger::print("\t: Warning - No 'bondgenerator' function defined.");
	angleGenerator_ = generatorFunctions_.findFunction("anglegenerator");
	if (angleGenerator_) Messenger::print("\t: Found 'anglegenerator' function.");
	else Messenger::print("\t: Warning - No 'anglegenerator' function defined.");
	torsionGenerator_ = generatorFunctions_.findFunction("torsiongenerator");
	if (torsionGenerator_) Messenger::print("\t: Found 'torsiongenerator' function.");
	else Messenger::print("\t: Warning - No 'torsiongenerator' function defined.");

	Messenger::exit("Forcefield::readFunctions");
	return result;
}

// Read in equivalent atom type names.
bool Forcefield::readEquivalents()
{
	/*
	By default, the 'equiv' name is set to the same as the atomtype name.
	Here, we search/replace specified definitions and set the equiv names to the first name in the list.
	The equivname doesn't have to exist in the atomtypes itself since the equivalent names are only used in intramolecular parameter searching.
	*/
	Messenger::enter("Forcefield::readEquivalents");
	int count, success, argpos;
	ForcefieldAtom* ffa;
	bool done = false;
	count = 0;
	do
	{
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error while reading equivalents data for atom %i.", count+1);
			if (success == -1) Messenger::print("End of file while reading equivalents data for atom %i.", count+1);
			Messenger::exit("Forcefield::readEquivalents");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Format of lines is : equivname_  fftype1  fftype2  ... fftypeN
			// Search atom types for typenames given in the list
			for (argpos=1; argpos<ffparser.nArgs(); argpos++)
			{
				for (ffa = types_.first(); ffa != NULL; ffa = ffa->next)
					if (matchType(ffa->name(),ffparser.argc(argpos)) < 10) ffa->setEquivalent(ffparser.argc(0));
			}
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Processed %i atomtype equivalents.", count);

	Messenger::exit("Forcefield::readEquivalents");
	return true;
}

// Read in interatomic (vdW) terms
bool Forcefield::readInter()
{
	// Format of lines is: 'ffid  fftype  charge  data1  data2  ... dataN'
	Messenger::enter("Forcefield::readInter");
	int success, count, n;
	ForcefieldAtom* ffa;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(ffparser.argc(1));
	if (vdwstyle == VdwFunctions::nVdwFunctions)
	{
		vdwstyle = VdwFunctions::None;
		Messenger::print("VDW functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		return false;
	}
	bool done = false;
	count = 0;
	do
	{
		// Format of lines is: 'ffid  fftype   charge  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading VDW data for atom %i.", count+1);
			if (success == -1) Messenger::print("End of file while reading VDW data for atom %i.", count+1);
			Messenger::exit("Forcefield::readInter");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Need not specify the data in the same order as for the type data above, so search for the fftype read in...
			ffa = findType(ffparser.argi(0));
			if (ffa == NULL)
			{
				Messenger::print("Unrecognised forcefield atom id in VDW list: '%s'", qPrintable(ffparser.argc(0)));
				Messenger::exit("Forcefield::readInter");
				return false;
			}
			ffa->setCharge(ffparser.argd(2));
			ffa->setVdwForm(vdwstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) ffa->setParameter(n, ffparser.argd(n+3));
			Messenger::print(Messenger::Verbose, "VDW Data %i : %s q=%8.4f, f%8.4f %8.4f %8.4f %8.4f", ffa->typeId(), qPrintable(ffa->name()), ffa->charge(), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->parameter(3), ffa->parameter(4), ffa->parameter(5));
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i atomic VDW parameters (%s)", count, VdwFunctions::VdwFunctions[vdwstyle].name);

	Messenger::exit("Forcefield::readInter");
	return true;
}

// Read in bond specifications
bool Forcefield::readBonds()
{
	Messenger::enter("Forcefield::readBonds");
	ForcefieldBound* newffbond;
	bool done = false;
	int count, success, n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(ffparser.argc(1));
	if (bondstyle == BondFunctions::nBondFunctions)
	{
		bondstyle = BondFunctions::None;
		Messenger::print("Bond stretch functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		return false;
	}
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2   data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading bond data %i.", count+1);
			if (success == -1) Messenger::print("End of file error reading bond data %i.", count+1);
			Messenger::exit("Forcefield::readBonds");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<2; n++)
			{
				if ((!ffparser.argc(n).contains('*')) && (!findType(ffparser.argc(n))))
					Messenger::print("\t... Warning - bond atom '%s' does not exist in the forcefield!", qPrintable(ffparser.argc(n)));
			}
			// Create new ff_bond structure
			newffbond = bonds_.add();
			newffbond->setType(ForcefieldBound::BondInteraction);
			newffbond->setTypeName(0, ffparser.argc(0));
			newffbond->setTypeName(1, ffparser.argc(1));
			newffbond->setBondForm(bondstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+2)) newffbond->setParameter(n, ffparser.argd(n+2));
			Messenger::print(Messenger::Verbose, "BOND %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", n, qPrintable(newffbond->typeName(0)), qPrintable(newffbond->typeName(1)) , newffbond->parameter(0), newffbond->parameter(1), newffbond->parameter(2), newffbond->parameter(3), newffbond->parameter(4), newffbond->parameter(5)); 
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i bond definitions (%s)", count, BondFunctions::BondFunctions[bondstyle].name);
	Messenger::exit("Forcefield::readBonds");
	return true;
}

// Read in angle specifications
bool Forcefield::readAngles()
{
	Messenger::enter("Forcefield::readAngles");
	ForcefieldBound* newffangle;
	int count, success, n;
	// Grab functional form of angle potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(ffparser.argc(1));
	if (anglestyle == AngleFunctions::nAngleFunctions)
	{
		anglestyle = AngleFunctions::None;
		Messenger::print("Angle bend functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		return false;
	}
	bool done = false;
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading angle data %i.",count+1);
			if (success == -1) Messenger::print("End of file while reading angle data %i.",count+1);
			Messenger::exit("Forcefield::readAngles");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((!ffparser.argc(n).contains('*')) && (!findType(ffparser.argc(n))))
					Messenger::print("\t... Warning - angle atom '%s' does not exist in the forcefield!", qPrintable(ffparser.argc(n)));
			}
			// Create new ff_angle structure
			newffangle = angles_.add();
			newffangle->setType(ForcefieldBound::AngleInteraction);
			newffangle->setTypeName(0, ffparser.argc(0));
			newffangle->setTypeName(1, ffparser.argc(1));
			newffangle->setTypeName(2, ffparser.argc(2));
			newffangle->setAngleForm(anglestyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) newffangle->setParameter(n,ffparser.argd(n+3));
			Messenger::print(Messenger::Verbose, "ANGLE %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", n, qPrintable(newffangle->typeName(0)), qPrintable(newffangle->typeName(1)), qPrintable(newffangle->typeName(2)), newffangle->parameter(0), newffangle->parameter(1), newffangle->parameter(2), newffangle->parameter(3), newffangle->parameter(4), newffangle->parameter(5)); 
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i angle definitions (%s)", count, AngleFunctions::AngleFunctions[anglestyle].name);
	Messenger::exit("Forcefield::readAngles");
	return true;
}

// Read in torsion data
bool Forcefield::readTorsions()
{
	Messenger::enter("Forcefield::readTorsions");
	ForcefieldBound* newfftorsion;
	int count, success, n;
	double escale = 0.5, vscale = 0.5;
	// Get functional form of torsion potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(ffparser.argc(1));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions)
	{
		torsionstyle = TorsionFunctions::None;
		Messenger::print("Torsion twist functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		return false;
	}
	// Have new scaling factors been provided?
	if (!ffparser.hasArg(2)) Messenger::print("   Electrostatic/VDW 1-4 scale factors not provided - defaults of %f and %f used instead.", escale, vscale);
	else if (!ffparser.hasArg(3))
	{
		Messenger::print("Error: Only electrostatic 1-4 scale factor was provided in torsion block header (line %i).",ffparser.lastLineNo());
		return false;
	}
	else
	{
		escale = ffparser.argd(2);
		vscale = ffparser.argd(3);
		Messenger::print("   Electrostatic/VDW 1-4 scale factors for these torsions are %f and %f.", escale, vscale);
	}
	count = 0;
	bool done = false;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading torsion data %i.",count+1);
			if (success == -1) Messenger::print("End of file error reading torsion data %i.",count+1);
			Messenger::exit("Forcefield::readTorsions");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((!ffparser.argc(n).contains('*')) && (!findType(ffparser.argc(n))))
					Messenger::print("\t... Warning - torsion atom '%s' does not exist in the forcefield!", qPrintable(ffparser.argc(n)));
			}
			// Create new ff_angle structure
			newfftorsion = torsions_.add();
			newfftorsion->setType(ForcefieldBound::TorsionInteraction);
			newfftorsion->setTypeName(0, ffparser.argc(0));
			newfftorsion->setTypeName(1, ffparser.argc(1));
			newfftorsion->setTypeName(2, ffparser.argc(2));
			newfftorsion->setTypeName(3, ffparser.argc(3));
			newfftorsion->setTorsionForm(torsionstyle);
			newfftorsion->setScaleFactors(escale, vscale);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+4)) newfftorsion->setParameter(n, ffparser.argd(n+4));
			Messenger::print(Messenger::Verbose, "TORSION %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", n, qPrintable(newfftorsion->typeName(0)), qPrintable(newfftorsion->typeName(1)), qPrintable(newfftorsion->typeName(2)), qPrintable(newfftorsion->typeName(3)), newfftorsion->parameter(0), newfftorsion->parameter(1), newfftorsion->parameter(2), newfftorsion->parameter(3), newfftorsion->parameter(4), newfftorsion->parameter(5));
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i torsion definitions (%s)", count, TorsionFunctions::TorsionFunctions[torsionstyle].name);
	Messenger::exit("Forcefield::readTorsions");
	return true;
}

// Read in improper definitions
bool Forcefield::readImpropers()
{
	Messenger::enter("Forcefield::readImpropers");
	ForcefieldBound* newffimproper;
	int count, success, n;
	// Get functional form of torsion potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(ffparser.argc(1));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions)
	{
		torsionstyle = TorsionFunctions::None;
		Messenger::print("Improper torsion functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		TorsionFunctions::printValid();
		return false;
	}
	count = 0;
	bool done = false;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading improper torsion data %i.",count+1);
			if (success == -1) Messenger::print("End of file error reading improper torsion data %i.",count+1);
			Messenger::exit("Forcefield::readImpropers");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((!ffparser.argc(n).contains('*')) && (!findType(ffparser.argc(n))))
					Messenger::print("\t... Warning - improper torsion atom '%s' does not exist in the forcefield!", qPrintable(ffparser.argc(n)));
			}
			// Create new ff_angle structure
			newffimproper = impropers_.add();
			newffimproper->setType(ForcefieldBound::ImproperInteraction);
			newffimproper->setTypeName(0, ffparser.argc(0));
			newffimproper->setTypeName(1, ffparser.argc(1));
			newffimproper->setTypeName(2, ffparser.argc(2));
			newffimproper->setTypeName(3, ffparser.argc(3));
			newffimproper->setTorsionForm(torsionstyle);
			newffimproper->setScaleFactors(1.0,1.0);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+4)) newffimproper->setParameter(n, ffparser.argd(n+4));
			Messenger::print(Messenger::Verbose, "IMPROPER %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", n, qPrintable(newffimproper->typeName(0)), qPrintable(newffimproper->typeName(1)), qPrintable(newffimproper->typeName(2)), qPrintable(newffimproper->typeName(3)), newffimproper->parameter(0), newffimproper->parameter(1), newffimproper->parameter(2), newffimproper->parameter(3), newffimproper->parameter(4), newffimproper->parameter(5));
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i improper torsion definitions (%s)", count, TorsionFunctions::TorsionFunctions[torsionstyle].name);
	Messenger::exit("Forcefield::readImpropers");
	return true;
}

// Read in Urey-Bradley data
bool Forcefield::readUreyBradley()
{
	Messenger::enter("Forcefield::readUreyBradley");
	ForcefieldBound* newffurey;
	int count, success, n;
	// Get functional form of potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(ffparser.argc(1));
	if (bondstyle == BondFunctions::nBondFunctions)
	{
		bondstyle = BondFunctions::None;
		Messenger::print("Urey-Bradley (bond) functional form not recognised - '%s'", qPrintable(ffparser.argc(1)));
		BondFunctions::printValid();
		return false;
	}
	count = 0;
	bool done = false;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) Messenger::print("File error reading Urey-Bradley data %i.",count+1);
			if (success == -1) Messenger::print("End of file error reading Urey-Bradley data %i.",count+1);
			Messenger::exit("Forcefield::readUreyBradley");
			return false;
		}
		if (ffparser.argc(0) == "end") done = true;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((!ffparser.argc(n).contains('*')) && (!findType(ffparser.argc(n))))
					Messenger::print("\t... Warning - Urey-Bradley atom '%s' does not exist in the forcefield!", qPrintable(ffparser.argc(n)));
			}
			// Create new ff_angle structure
			newffurey = ureyBradleys_.add();
			newffurey->setType(ForcefieldBound::UreyBradleyInteraction);
			newffurey->setTypeName(0, ffparser.argc(0));
			newffurey->setTypeName(1, ffparser.argc(1));
			newffurey->setTypeName(2, ffparser.argc(2));
			newffurey->setBondForm(bondstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) newffurey->setParameter(n, ffparser.argd(n+3));
			Messenger::print(Messenger::Verbose, "UREY-BRADLEY %i : %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f", n, qPrintable(newffurey->typeName(0)), qPrintable(newffurey->typeName(1)), qPrintable(newffurey->typeName(2)), newffurey->parameter(0), newffurey->parameter(1), newffurey->parameter(2), newffurey->parameter(3), newffurey->parameter(4), newffurey->parameter(5));
			count ++;
		}
	} while (!done);
	Messenger::print("\t: Read in %i Urey-Bradley definitions (%s)", count, BondFunctions::BondFunctions[bondstyle].name);

	Messenger::exit("Forcefield::readUreyBradley");
	return true;
}
