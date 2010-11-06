/*
	*** Forcefield import
	*** src/ff/loadforcefield.cpp
	Copyright T. Youngs 2007-2010

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
#include "classes/neta_parser.h"
#include "templates/namemap.h"

// Load the specified forcefield
bool Forcefield::load(const char *filename)
{
	msg.enter("Forcefield::load");
	bool done, okay;
	int success, n;
	Prefs::EnergyUnit ffunit;
	// Open file for reading
	ffparser.openFile(filename);
	// Store the filename of the forcefield
	filename_.set(filename);
	// Now follows blocks of keywords
	done = FALSE;
	msg.print("Opening forcefield : %s...\n",filename);
	do
	{
		okay = FALSE;
		success = ffparser.getArgsDelim(LineParser::UseQuotes+LineParser::SkipBlanks);
		if (success == 1)
		{
			msg.print("Error reading FF directive.\n");
			ffparser.closeFile();
			msg.exit("Forcefield::load");
			return FALSE;
		}
		if (success == -1) break;
		// Call subroutines to read in data based on keywords
		switch (forcefieldCommand(ffparser.argc(0)))
		{
			case (Forcefield::MessageCommand):
				msg.print("####\t %s\n",ffparser.argc(1));
				okay = TRUE;
				break;
			case (Forcefield::NameCommand):
				name_.set(ffparser.argc(1));
				msg.print("\t: '%s'\n",name_.get());
				okay = TRUE;
				break;
			case (Forcefield::UnitsCommand):
				ffunit = Prefs::energyUnit(ffparser.argc(1));
				if (ffunit != Prefs::nEnergyUnits)
				{
					energyUnit_ = ffunit;
					msg.print("\t: Energy units are %s\n", Prefs::energyUnit(energyUnit_));
					okay = TRUE;
				}
				else okay = FALSE;
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
				if (types_.nItems() == 0) msg.print("\nWarning - Equivalent definitions have been provided before any types have been defined.\n\n");
				okay = readEquivalents();
				break;
			case (Forcefield::FunctionCommand):
				okay = readFunctions();
				break;
			case (Forcefield::ConvertCommand):
				// Add simple list of energetic data parameters
				for (n=1; n<ffparser.nArgs(); n++) addEnergyData(ffparser.argc(n));
				okay = TRUE;
				break;
			case (Forcefield::VdwCommand):
				msg.print("The 'vdw' keyword is deprecated - use 'inter' instead.\n");
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
				msg.print("Error: Use of 'vscale' command is deprecated.\n\tSpecify 1-4 scaling factors in the torsion block header.\n");
				okay = FALSE;
				break;
			case (Forcefield::EScaleCommand):
				msg.print("Error: Use of 'escale' command is deprecated.\n\tSpecify 1-4 scaling factors in the torsion block header.\n");
				okay = FALSE;
				break;
			default:
				msg.print("Unrecognised forcefield keyword '%s'.\n",ffparser.argc(0));
				break;
		}
		// Check on 'okay'
		if (!okay)
		{
			//msg.print("EreadVdwor reading forcefield file. Aborted.\n");
			msg.print("Error at line %i of file.\n", ffparser.lastLineNo());
			msg.exit("Forcefield::load");
			ffparser.closeFile();
			return FALSE;
		}
	} while (okay);
	ffparser.closeFile();

	// Check that some forcefield types were defined...
	if (types_.nItems() <= 1) msg.print("Warning - no types are defined in this forcefield.\n");

	// Link forcefield type references (&N) to their actual forcefield types
	for (ForcefieldAtom *ffa = types_.first(); ffa != NULL; ffa = ffa->next) ffa->neta()->linkReferenceTypes();
	// Last thing - convert energetic units in the forcefield to the internal units of the program
	convertParameters();
	msg.exit("Forcefield::load");
	return TRUE;
}

// Read in forcefield type defines
bool Forcefield::readDefines()
{
	msg.enter("Forcefield::readDefines");
	int success, nadded = 0;
	bool done;
	Neta *neta;
	done = FALSE;
	// Format of lines is 'ffid typename element description [text]'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading atom type defines %i.\n", types_.nItems());
			if (success == -1) msg.print("End of file while reading atom type defines %i.\n", types_.nItems());
			msg.exit("Forcefield::readDefines");
			return FALSE;
		}
		else if (strcmp(ffparser.argc(0),"end") == 0) break;
		// Search for this define name to make sure it hasn't already been used
		for (neta = typeDefines_.first(); neta != NULL; neta = neta->next) if (strcmp(ffparser.argc(0), neta->name()) == 0) break;
		if (neta != NULL)
		{
			msg.print("Error: Duplicate type define name specified (%s) at line %i.\n", ffparser.argc(0), ffparser.lastLineNo());
			msg.exit("Forcefield::readDefines");
			return FALSE;
		}
		neta = typeDefines_.add();
		nadded ++;
		neta->setName(ffparser.argc(0));
		neta->setParentForcefield(this);
		if (!netaparser.createNeta(neta, ffparser.argc(1), this))
		{
			msg.print("Error parsing type define at line %i.\n", ffparser.lastLineNo());
			msg.exit("Forcefield::readDefines");
			return FALSE;
		}
	} while (!done);
	if (nadded == 0) msg.print("Warning - No atype defines specified in this block (at line %i)!\n", ffparser.lastLineNo());
	else msg.print("\t: Read in %i type defines\n", nadded);
	msg.exit("Forcefield::readDefines");
	return TRUE;
}

// Read in forcefield atom types.
bool Forcefield::readTypes()
{
	msg.enter("Forcefield::readTypes");
	int success, newffid, nadded = 0;
	bool done;
	ForcefieldAtom *ffa, *idsearch;
	done = FALSE;
	// Format of lines is 'ffid typename element description [text]'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading atom type description %i.\n", types_.nItems());
			if (success == -1) msg.print("End of file while reading atom type description %i.\n", types_.nItems());
			msg.exit("Forcefield::readTypes");
			return FALSE;
		}
		else if (strcmp(ffparser.argc(0),"end") == 0) break;
		// Search for this ID to make sure it hasn't already been used
		newffid = ffparser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			msg.print("Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
			msg.exit("Forcefield::readTypes");
			return FALSE;
		}
		ffa = types_.add();
		nadded ++;
		ffa->setParent(this);
		ffa->setTypeId(newffid);
		// Check number of items on line in file
		if (ffparser.nArgs() < 4)
		{
			msg.print("Error: Missing data in 'types' block.\n\tFormat of lines in block is 'ffid  typename  element   NETA  [description]'\n");
			msg.exit("Forcefield::readTypes");
			return FALSE;
		}
		ffa->setName(ffparser.argc(1));
		int el = elements().findAlpha(ffparser.argc(2));
		ffa->setElement(el);
		ffa->setEquivalent(ffparser.argc(1));
		ffa->neta()->setCharacterElement(el);
		if (!ffa->setNeta(ffparser.argc(3), this))
		{
			msg.exit("Forcefield::readTypes");
			return FALSE;
		}
		if (ffparser.hasArg(4)) ffa->setDescription(ffparser.argc(4));
	} while (!done);
	if (nadded == 0) msg.print("Warning - No atom types specified in this block (at line %i)!\n", ffparser.lastLineNo());
	else msg.print("\t: Read in %i type descriptions\n", nadded);
	msg.exit("Forcefield::readTypes");
	return TRUE;
}

// Read in united atom forcefield types.
bool Forcefield::readUnitedAtomTypes()
{
	msg.enter("Forcefield::readUnitedAtomTypes");
	int success, newffid, nadded = 0;
	bool done;
	ForcefieldAtom *ffa, *idsearch;
	done = FALSE;
	// Format of lines is 'ffid typename element description [text]'
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading united atom type description %i.\n", types_.nItems());
			if (success == -1) msg.print("End of file while reading united atom type description %i.\n", types_.nItems());
			msg.exit("Forcefield::readUnitedAtomTypes");
			return FALSE;
		}
		else if (strcmp(ffparser.argc(0),"end") == 0) break;
		// Search for this ID to make sure it hasn't already been used
		newffid = ffparser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			msg.print("Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
			msg.exit("Forcefield::readTypes");
			return FALSE;
		}
		ffa = types_.add();
		nadded ++;
		ffa->setParent(this);
		ffa->setTypeId(newffid);
		// Check number of items on line in file
		if (ffparser.nArgs() < 5)
		{
			msg.print("Error: Missing data in 'uatypes' block.\n\tFormat of lines in block is 'ffid  typename  element  mass  NETA  [description]'\n");
			msg.exit("Forcefield::readUnitedAtomTypes");
			return FALSE;
		}
		ffa->setName(ffparser.argc(1));
		ffa->setEquivalent(ffparser.argc(1));
		ffa->setElement(-1);
		ffa->setElementMass(ffparser.argd(3));
		ffa->neta()->setCharacterElement(elements().findAlpha(ffparser.argc(2)));
		if (!ffa->setNeta(ffparser.argc(4), this))
		{
			msg.exit("Forcefield::readUnitedAtomTypes");
			return FALSE;
		}
		if (ffparser.hasArg(5)) ffa->setDescription(ffparser.argc(4));
	} while (!done);
	if (nadded == 0) msg.print("Warning - No united atom types specified in this block (at line %i)!\n", ffparser.lastLineNo());
	else msg.print("\t: Read in %i united-atom type descriptions\n", nadded);
	msg.exit("Forcefield::readUnitedAtomTypes");
	return TRUE;
}

// Reads in extra data for atoms
bool Forcefield::readData(const char *vars)
{
	msg.enter("Forcefield::readData");
	// First, parse list of data items to get name
	NameMapList<VTypes::DataType> items(VTypes::nDataTypes);
	LineParser parser;
	parser.getArgsDelim(vars);
	int n;
	for (n=0; n<parser.nArgs(); n += 2)
	{
		// Determine data type
		VTypes::DataType vt = VTypes::dataType(parser.argc(n));
		if (vt == VTypes::nDataTypes)
		{
			msg.print("Unrecognised type ('%s') found in list in 'data' block header.\n", parser.argc(0));
			msg.exit("Forcefield::readData");
			return FALSE;
		}
		items.add(parser.argc(n+1), vt);
	}
	// Next, each line contains the forcefield atom to which the data relates, the type name (for bookkeeping, nothing more)
	// and then the data in the order specified in the header above.
	bool done = FALSE;
	int nadded = 0, success;
	do
	{
		success = ffparser.getArgsDelim(LineParser::UseQuotes+ LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading atom type data.\n");
			if (success == -1) msg.print("End of file while reading atom type data.\n");
			msg.exit("Forcefield::readData");
			return FALSE;
		}
		else if (strcmp(ffparser.argc(0),"end") == 0) break;
		// Search for this ffatom ID and retrieve it
		ForcefieldAtom *ffa = findType(ffparser.argi(0));
		if (ffa == NULL)
		{
			msg.print("Error: forcefield type ID '%i' has not been specified, so can't add data to it.\n", ffparser.argi(0));
			msg.exit("Forcefield::readData");
			return FALSE;
		}
		nadded ++;
		// Get data from lines in the order it was specified above
		for (n=2; n<ffparser.nArgs(); ++n)
		{
			// If the parser argument is blank we've run out of arguments early
			if (!ffparser.hasArg(n))
			{
				msg.print("Warning: Forcefield atom id %i (%s) has an incomplete set of data (line %i in file).\n", ffa->typeId(), ffa->name(), ffparser.lastLineNo());
				break;
			}
			switch (items.data(n-2))
			{
				case (VTypes::IntegerData):
					ffa->addData(items.name(n-2), ffparser.argi(n));
					break;
				case (VTypes::DoubleData):
					ffa->addData(items.name(n-2), ffparser.argd(n));
					break;
				case (VTypes::StringData):
					ffa->addData(items.name(n-2), ffparser.argc(n));
					break;
				default:
					msg.print("Error: Unsuitable datatype for data item %i.\n", n-1);
					msg.exit("Forcefield::readData");
					return TRUE;
			}
		}
	} while (!done);
	if (nadded == 0) msg.print("Warning - No data specified in this block (at line %i)!\n", ffparser.lastLineNo());
	else msg.print("\t: Read in data for %i types\n", nadded);
	msg.exit("Forcefield::readData");
	return TRUE;
}

// Read generator data
bool Forcefield::readGenerator(const char *vars)
{
	// Read in generator data for atom types in rule-based forcefields
	// We expect there to be the same number of sets of data as there are types...
	// Argument to 'generator' keyword is number of data per atom
	msg.enter("Forcefield::readGenerator");
	int count, success, n;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	// First, parse list of data items to get names and types of variables
	NameMapList<VTypes::DataType> items(VTypes::nDataTypes);
	LineParser parser;
	parser.getArgsDelim(vars);
	for (n=0; n<parser.nArgs(); n += 2)
	{
		// Determine data type
		VTypes::DataType vt = VTypes::dataType(parser.argc(n));
		if (vt == VTypes::nDataTypes)
		{
			msg.print("Unrecognised type ('%s') found in list in 'generator' block header.\n", parser.argc(0));
			msg.exit("Forcefield::readGenerator");
			return FALSE;
		}
		items.add(parser.argc(n+1), vt);
	}
	count = 0;
	do
	{
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading generator data for atom %i.\n",count+1);
			if (success == -1) msg.print("End of file while reading generator data for atom %i.\n",count+1);
			msg.exit("Forcefield::readGenerator");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) break;
		// Search for this ffatom ID and retrieve it
		ForcefieldAtom *ffa = findType(ffparser.argi(0));
		if (ffa == NULL)
		{
			msg.print("Error: forcefield type ID '%i' has not been specified, so can't add generator data to it.\n", ffparser.argi(0));
			msg.exit("Forcefield::readGenerator");
			return FALSE;
		}
		count ++;
		// Get data from lines in the order it was specified above
		for (n=2; n<ffparser.nArgs(); ++n)
		{
			// If the parser argument is blank we've run out of arguments early
			if (!ffparser.hasArg(n))
			{
				msg.print("Warning: Forcefield atom id %i (%s) has an incomplete set of data (line %i in file).\n", ffa->typeId(), ffa->name(), ffparser.lastLineNo());
				break;
			}
			switch (items.data(n-2))
			{
				case (VTypes::IntegerData):
					ffa->addData(items.name(n-2), ffparser.argi(n));
					break;
				case (VTypes::DoubleData):
					ffa->addData(items.name(n-2), ffparser.argd(n));
					break;
				case (VTypes::StringData):
					ffa->addData(items.name(n-2), ffparser.argc(n));
					break;
				default:
					msg.print("Error: Unsuitable datatype for data item %i.\n", n-1);
					msg.exit("Forcefield::readGenerator");
					return TRUE;
			}
		}
	} while (!done);
	if (count != types_.nItems()-1) msg.print("Warning: Not all atom types had generator data defined (%i missing).\n", types_.nItems()-count-1);
	msg.print("\t: Read in generator data for %i atomtypes.\n", count);
	msg.exit("Forcefield::readGenerator");
	return TRUE;
}

// Read in generator function definitions
bool Forcefield::readFunctions()
{
	msg.enter("Forcefield::readFunctions");
	// Store every line from the file up to the next 'end' block
	List<Dnchar> stringList;
	bool done = FALSE;
	int success;
	do
	{
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading function block.\n");
			if (success == -1) msg.print("End of file while reading function block.\n");
			msg.exit("Forcefield::readFunctions");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") != 0)
		{
			// Add line to internal list and continue
			Dnchar *newline = stringList.add();
			newline->set(ffparser.line());
		}
		else done = TRUE;
	} while (!done);
	// Check for empty string list
	if (stringList.nItems() == 0)
	{
		msg.print("Found an empty 'functions' block - ignored.\n");
		msg.exit("Forcefield::readFunctions");
		return TRUE;
	}
	// Now, attempt to parser the lines we just read in to create functions....
	bool result = generatorFunctions_.generateFromStringList(stringList.first(), "GeneratorFuncs", TRUE);
	// Search for functions we recognise
	vdwGenerator_ = generatorFunctions_.findGlobalFunction("vdwgenerator");
	if (vdwGenerator_) msg.print("\t: Found 'vdwgenerator' function.\n");
	else msg.print("\t: Warning - No 'vdwgenerator' function defined.\n");
	bondGenerator_ = generatorFunctions_.findGlobalFunction("bondgenerator");
	if (bondGenerator_) msg.print("\t: Found 'bondgenerator' function.\n");
	else msg.print("\t: Warning - No 'bondgenerator' function defined.\n");
	angleGenerator_ = generatorFunctions_.findGlobalFunction("anglegenerator");
	if (angleGenerator_) msg.print("\t: Found 'anglegenerator' function.\n");
	else msg.print("\t: Warning - No 'anglegenerator' function defined.\n");
	torsionGenerator_ = generatorFunctions_.findGlobalFunction("torsiongenerator");
	if (torsionGenerator_) msg.print("\t: Found 'torsiongenerator' function.\n");
	else msg.print("\t: Warning - No 'torsiongenerator' function defined.\n");
	msg.exit("Forcefield::readFunctions");
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
	msg.enter("Forcefield::readEquivalents");
	int count, success, argpos;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	count = 0;
	do
	{
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error while reading equivalents data for atom %i.\n",count+1);
			if (success == -1) msg.print("End of file while reading equivalents data for atom %i.\n",count+1);
			msg.exit("Forcefield::readEquivalents");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
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
	msg.print("\t: Processed %i atomtype equivalents.\n",count);
	msg.exit("Forcefield::readEquivalents");
	return TRUE;
}

// Read in interatomic (vdW) terms
bool Forcefield::readInter()
{
	// Format of lines is: 'ffid  fftype  charge  data1  data2  ... dataN'
	msg.enter("Forcefield::readInter");
	int success, count, n;
	ForcefieldAtom *ffa;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(ffparser.argc(1));
	if (vdwstyle == VdwFunctions::nVdwFunctions)
	{
		vdwstyle = VdwFunctions::None;
		msg.print("VDW functional form not recognised - '%s'\n",ffparser.argc(1));
		return FALSE;
	}
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'ffid  fftype   charge  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading VDW data for atom %i.\n",count+1);
			if (success == -1) msg.print("End of file while reading VDW data for atom %i.\n",count+1);
			msg.exit("Forcefield::readInter");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Need not specify the data in the same order as for the type data above, so search for the fftype read in...
			ffa = findType(ffparser.argi(0));
			if (ffa == NULL)
			{
				msg.print("Unrecognised forcefield atom id in VDW list: '%s'\n", ffparser.argc(0));
				msg.exit("Forcefield::readInter");
				return FALSE;
			}
			ffa->setCharge(ffparser.argd(2));
			ffa->setVdwForm(vdwstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) ffa->setParameter(n, ffparser.argd(n+3));
			msg.print(Messenger::Verbose,"VDW Data %i : %s q=%8.4f, f%8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->charge(), ffa->parameter(0), ffa->parameter(1), ffa->parameter(2), ffa->parameter(3), ffa->parameter(4), ffa->parameter(5));
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i atomic VDW parameters (%s)\n", count, VdwFunctions::VdwFunctions[vdwstyle].name);
	msg.exit("Forcefield::readInter");
	return TRUE;
}

// Read in bond specifications
bool Forcefield::readBonds()
{
	msg.enter("Forcefield::readBonds");
	ForcefieldBound *newffbond;
	bool done = FALSE;
	int count, success, n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(ffparser.argc(1));
	if (bondstyle == BondFunctions::nBondFunctions)
	{
		bondstyle = BondFunctions::None;
		msg.print("Bond stretch functional form not recognised - '%s'\n",ffparser.argc(1));
		return FALSE;
	}
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2   data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading bond data %i.\n",count+1);
			if (success == -1) msg.print("End of file error reading bond data %i.\n",count+1);
			msg.exit("Forcefield::readBonds");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '?', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<2; n++)
			{
				if ((strchr(ffparser.argc(n),'*') == NULL) && (findType(ffparser.argc(n)) == NULL))
					msg.print("\t... Warning - bond atom '%s' does not exist in the forcefield!\n", ffparser.argc(n));
			}
			// Create new ff_bond structure
			newffbond = bonds_.add();
			newffbond->setType(ForcefieldBound::BondInteraction);
			newffbond->setTypeName(0,ffparser.argc(0));
			newffbond->setTypeName(1,ffparser.argc(1));
			newffbond->setBondForm(bondstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+2)) newffbond->setParameter(n, ffparser.argd(n+2));
			msg.print(Messenger::Verbose,"BOND %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffbond->typeName(0), newffbond->typeName(1) , newffbond->parameter(0), newffbond->parameter(1), newffbond->parameter(2), newffbond->parameter(3), newffbond->parameter(4), newffbond->parameter(5)); 
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i bond definitions (%s)\n", count, BondFunctions::BondFunctions[bondstyle].name);
	msg.exit("Forcefield::readBonds");
	return TRUE;
}

// Read in angle specifications
bool Forcefield::readAngles()
{
	msg.enter("Forcefield::readAngles");
	ForcefieldBound *newffangle;
	int count, success, n;
	// Grab functional form of angle potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(ffparser.argc(1));
	if (anglestyle == AngleFunctions::nAngleFunctions)
	{
		anglestyle = AngleFunctions::None;
		msg.print("Angle bend functional form not recognised - '%s'\n",ffparser.argc(1));
		return FALSE;
	}
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading angle data %i.\n",count+1);
			if (success == -1) msg.print("End of file while reading angle data %i.\n",count+1);
			msg.exit("Forcefield::readAngles");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((strchr(ffparser.argc(n),'*') == NULL) && (findType(ffparser.argc(n)) == NULL))
					msg.print("\t... Warning - angle atom '%s' does not exist in the forcefield!\n",ffparser.argc(n));
			}
			// Create new ff_angle structure
			newffangle = angles_.add();
			newffangle->setType(ForcefieldBound::AngleInteraction);
			newffangle->setTypeName(0, ffparser.argc(0));
			newffangle->setTypeName(1, ffparser.argc(1));
			newffangle->setTypeName(2, ffparser.argc(2));
			newffangle->setAngleForm(anglestyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) newffangle->setParameter(n,ffparser.argd(n+3));
			msg.print(Messenger::Verbose,"ANGLE %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffangle->typeName(0), newffangle->typeName(1), newffangle->typeName(2), newffangle->parameter(0), newffangle->parameter(1), newffangle->parameter(2), newffangle->parameter(3), newffangle->parameter(4), newffangle->parameter(5)); 
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i angle definitions (%s)\n", count, AngleFunctions::AngleFunctions[anglestyle].name);
	msg.exit("Forcefield::readAngles");
	return TRUE;
}

// Read in torsion data
bool Forcefield::readTorsions()
{
	msg.enter("Forcefield::readTorsions");
	ForcefieldBound *newfftorsion;
	int count, success, n;
	double escale = 0.5, vscale = 0.5;
	// Get functional form of torsion potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(ffparser.argc(1));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions)
	{
		torsionstyle = TorsionFunctions::None;
		msg.print("Torsion twist functional form not recognised - '%s'\n",ffparser.argc(1));
		return FALSE;
	}
	// Have new scaling factors been provided?
	if (!ffparser.hasArg(2)) msg.print("   Electrostatic/VDW 1-4 scale factors not provided - defaults of %f and %f used instead.\n", escale, vscale);
	else if (!ffparser.hasArg(3))
	{
		msg.print("Error: Only electrostatic 1-4 scale factor was provided in torsion block header (line %i).\n",ffparser.lastLineNo());
		return FALSE;
	}
	else
	{
		escale = ffparser.argd(2);
		vscale = ffparser.argd(3);
		msg.print("   Electrostatic/VDW 1-4 scale factors for these torsions are %f and %f.\n", escale, vscale);
	}
	count = 0;
	bool done = FALSE;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading torsion data %i.\n",count+1);
			if (success == -1) msg.print("End of file error reading torsion data %i.\n",count+1);
			msg.exit("Forcefield::readTorsions");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((strchr(ffparser.argc(n),'*') == NULL) && (findType(ffparser.argc(n)) == NULL))
					msg.print("\t... Warning - torsion atom '%s' does not exist in the forcefield!\n",ffparser.argc(n));
			}
			// Create new ff_angle structure
			newfftorsion = torsions_.add();
			newfftorsion->setType(ForcefieldBound::TorsionInteraction);
			newfftorsion->setTypeName(0,ffparser.argc(0));
			newfftorsion->setTypeName(1,ffparser.argc(1));
			newfftorsion->setTypeName(2,ffparser.argc(2));
			newfftorsion->setTypeName(3,ffparser.argc(3));
			newfftorsion->setTorsionForm(torsionstyle);
			newfftorsion->setScaleFactors(escale, vscale);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+4)) newfftorsion->setParameter(n, ffparser.argd(n+4));
			msg.print(Messenger::Verbose,"TORSION %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newfftorsion->typeName(0), newfftorsion->typeName(1), newfftorsion->typeName(2), newfftorsion->typeName(3), newfftorsion->parameter(0), newfftorsion->parameter(1), newfftorsion->parameter(2), newfftorsion->parameter(3), newfftorsion->parameter(4), newfftorsion->parameter(5));
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i torsion definitions (%s)\n", count, TorsionFunctions::TorsionFunctions[torsionstyle].name);
	msg.exit("Forcefield::readTorsions");
	return TRUE;
}

// Read in improper definitions
bool Forcefield::readImpropers()
{
	msg.enter("Forcefield::readImpropers");
	ForcefieldBound *newffimproper;
	int count, success, n;
	// Get functional form of torsion potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(ffparser.argc(1));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions)
	{
		torsionstyle = TorsionFunctions::None;
		msg.print("Improper torsion functional form not recognised - '%s'\n",ffparser.argc(1));
		TorsionFunctions::printValid();
		return FALSE;
	}
	count = 0;
	bool done = FALSE;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading improper torsion data %i.\n",count+1);
			if (success == -1) msg.print("End of file error reading improper torsion data %i.\n",count+1);
			msg.exit("Forcefield::readImpropers");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((strchr(ffparser.argc(n),'*') == NULL) && (findType(ffparser.argc(n)) == NULL))
					msg.print("\t... Warning - improper torsion atom '%s' does not exist in the forcefield!\n",ffparser.argc(n));
			}
			// Create new ff_angle structure
			newffimproper = impropers_.add();
			newffimproper->setType(ForcefieldBound::ImproperInteraction);
			newffimproper->setTypeName(0,ffparser.argc(0));
			newffimproper->setTypeName(1,ffparser.argc(1));
			newffimproper->setTypeName(2,ffparser.argc(2));
			newffimproper->setTypeName(3,ffparser.argc(3));
			newffimproper->setTorsionForm(torsionstyle);
			newffimproper->setScaleFactors(1.0,1.0);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+4)) newffimproper->setParameter(n, ffparser.argd(n+4));
			msg.print(Messenger::Verbose,"IMPROPER %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffimproper->typeName(0), newffimproper->typeName(1), newffimproper->typeName(2), newffimproper->typeName(3), newffimproper->parameter(0), newffimproper->parameter(1), newffimproper->parameter(2), newffimproper->parameter(3), newffimproper->parameter(4), newffimproper->parameter(5));
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i improper torsion definitions (%s)\n", count, TorsionFunctions::TorsionFunctions[torsionstyle].name);
	msg.exit("Forcefield::readImpropers");
	return TRUE;
}

// Read in Urey-Bradley data
bool Forcefield::readUreyBradley()
{
	msg.enter("Forcefield::readUreyBradley");
	ForcefieldBound *newffurey;
	int count, success, n;
	// Get functional form of potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(ffparser.argc(1));
	if (bondstyle == BondFunctions::nBondFunctions)
	{
		bondstyle = BondFunctions::None;
		msg.print("Urey-Bradley (bond) functional form not recognised - '%s'\n",ffparser.argc(1));
		BondFunctions::printValid();
		return FALSE;
	}
	count = 0;
	bool done = FALSE;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		success = ffparser.getArgsDelim(LineParser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg.print("File error reading Urey-Bradley data %i.\n",count+1);
			if (success == -1) msg.print("End of file error reading Urey-Bradley data %i.\n",count+1);
			msg.exit("Forcefield::readUreyBradley");
			return FALSE;
		}
		if (strcmp(ffparser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((strchr(ffparser.argc(n),'*') == NULL) && (findType(ffparser.argc(n)) == NULL))
					msg.print("\t... Warning - Urey-Bradley atom '%s' does not exist in the forcefield!\n",ffparser.argc(n));
			}
			// Create new ff_angle structure
			newffurey = ureyBradleys_.add();
			newffurey->setType(ForcefieldBound::UreyBradleyInteraction);
			newffurey->setTypeName(0,ffparser.argc(0));
			newffurey->setTypeName(1,ffparser.argc(1));
			newffurey->setTypeName(2,ffparser.argc(2));
			newffurey->setBondForm(bondstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (ffparser.hasArg(n+3)) newffurey->setParameter(n, ffparser.argd(n+3));
			msg.print(Messenger::Verbose,"UREY-BRADLEY %i : %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffurey->typeName(0), newffurey->typeName(1), newffurey->typeName(2), newffurey->parameter(0), newffurey->parameter(1), newffurey->parameter(2), newffurey->parameter(3), newffurey->parameter(4), newffurey->parameter(5));
			count ++;
		}
	} while (!done);
	msg.print("\t: Read in %i Urey-Bradley definitions (%s)\n", count, BondFunctions::BondFunctions[bondstyle].name);
	msg.exit("Forcefield::readUreyBradley");
	return TRUE;
}
