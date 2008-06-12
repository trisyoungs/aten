/*
	*** Forcefield import
	*** src/parse/forcefield.cpp
	Copyright T. Youngs 2007,2008

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
#include "classes/forcefield.h"
#include "base/elements.h"
#include "parse/parser.h"
#include "base/sysfunc.h"

// Local variables
double escale14 = 0.5;
double vscale14 = 0.5;

// Load the specified forcefield
bool Forcefield::load(const char *filename)
{
	dbgBegin(Debug::Calls,"Forcefield::load");
	bool done, okay;
	int success, n, m, count;
	Prefs::EnergyUnit ffunit;
	ifstream fffile(filename,ios::in);
	if (!fffile.good())
	{
		fffile.close();
		msg(Debug::None,"Failed to open forcefield file.\n");
		dbgEnd(Debug::Calls,"Forcefield::load");
		return FALSE;
	}
	// Store the filename of the forcefield
	filename_.set(filename);
	// Now follows blocks of keywords
	done = FALSE;
	msg(Debug::None,"Opening forcefield : %s...\n",filename);
	do
	{
		okay = FALSE;
		success = parser.getArgsDelim(&fffile,Parser::UseQuotes+Parser::SkipBlanks);
		if (success == 1)
		{
			msg(Debug::None,"Error reading FF directive.\n");
			fffile.close();
			dbgEnd(Debug::Calls,"Forcefield::load");
			return FALSE;
		}
		if (success == -1) break;
		// Call subroutines to read in data based on keywords
		switch (forcefieldCommand(parser.argc(0)))
		{
			case (Forcefield::NameCommand):
				name_.set(parser.argc(1));
				msg(Debug::None,"\t: '%s'\n",name_.get());
				okay = TRUE;
				break;
			case (Forcefield::UnitsCommand):
				ffunit = Prefs::energyUnit(parser.argc(1));
				if (ffunit != Prefs::nEnergyUnits)
				{
					energyUnit_ = ffunit;
					msg(Debug::None,"\t: Energy units are %s\n", Prefs::energyUnit(energyUnit_));
					okay = TRUE;
				}
				else okay = FALSE;
				break;
			case (Forcefield::RulesCommand):
				rules_ = Rules::forcefieldRules(parser.argc(1));
				msg(Debug::None,"\t: Rule-set to use is '%s'\n", Rules::forcefieldRules(rules_));
				okay = TRUE;
				break;
			case (Forcefield::TypesCommand):
				okay = readTypes(fffile);
				break;
			case (Forcefield::GeneratorCommand):
				okay = readGenerator(fffile);
				break;
			case (Forcefield::EquivalentsCommand):
				okay = readEquivalents(fffile);
				break;
			case (Forcefield::ConvertCommand):
				// Check that generator data has been initialised
				for (n=1; n<parser.nArgs(); n++) energyGenerators_[parser.argi(n)-1] = TRUE;
				okay = TRUE;
				break;
			case (Forcefield::VdwCommand):
				okay = readVdw(fffile);
				break;
			case (Forcefield::BondsCommand):
				okay = readBonds(fffile);
				break;
			case (Forcefield::AnglesCommand):
				okay = readAngles(fffile);
				break;
			case (Forcefield::TorsionsCommand):
				okay = readTorsions(fffile);
				break;
			case (Forcefield::VScaleCommand):
				vscale14 = parser.argd(1);	// 1-4 VDW scaling
				msg(Debug::None,"\t: VDW 1-4 scale factor = %6.3f\n", vscale14);
				okay = TRUE;
				break;
			case (Forcefield::EScaleCommand):
				escale14 = parser.argd(1);	// 1-4 electrostatic scaling
				msg(Debug::None,"\t: Electrostatic 1-4 scale factor = %6.3f\n", vscale14);
				okay = TRUE;
				break;
			default:
				msg(Debug::None,"Unrecognised forcefield keyword '%s'\n.",parser.argc(0));
				break;
		}
		// Check on 'okay'
		if (!okay)
		{
			//msg(Debug::None,"EreadVdwor reading forcefield file. Aborted.\n");
			dbgEnd(Debug::Calls,"Forcefield::load");
			fffile.close();
			return FALSE;
		}
	} while ( !fffile.eof() );
	fffile.close();
	// Last thing - convert energetic units in the forcefield to the internal units of the program
	convertParameters();
	dbgEnd(Debug::Calls,"Forcefield::load");
	return TRUE;
}

// Read in forcefield atom types.
bool Forcefield::readTypes(ifstream &fffile)
{
	dbgBegin(Debug::Calls,"Forcefield::readTypes");
	int success, newffid;
	bool done;
	ForcefieldAtom *ffa, *idsearch;
	done = FALSE;
	// Format of lines is 'ffid typename element description'
	do
	{
		success = parser.getArgsDelim(&fffile,Parser::UseQuotes+Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error while reading atom type description %i.\n", types_.nItems());
			if (success == -1) msg(Debug::None,"End of file while reading atom type description %i.\n", types_.nItems());
			dbgEnd(Debug::Calls,"Forcefield::readTypes");
			return FALSE;
		}
		else if (strcmp(parser.argc(0),"end") == 0) break;
		// Search for this ID to make sure it hasn't already been used
		newffid = parser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			msg(Debug::None,"Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name());
			dbgEnd(Debug::Calls,"Forcefield::readTypes");
			return FALSE;
		}
		ffa = types_.add();
		ffa->setParent(this);
		ffa->setTypeId(newffid);
		ffa->setName(parser.argc(1));
		ffa->setEquivalent(parser.argc(1));
		ffa->atomtype()->setCharacterElement(elements.find(parser.argc(2),Prefs::AlphaZmap));
		ffa->setAtomtype(parser.argc(3), this, ffa);
		ffa->setDescription(parser.argc(4));
	} while (!done);
	if (types_.nItems() == 1)
	{
		msg(Debug::None,"No atom types specified!\n");
		dbgEnd(Debug::Calls,"Forcefield::readTypes");
		return FALSE;
	}
	msg(Debug::None,"\t: Read in %i type descriptions\n", types_.nItems() - 1);
	dbgEnd(Debug::Calls,"Forcefield::readTypes");
	return TRUE;
}

bool Forcefield::readGenerator(ifstream &fffile)
{
	// Read in generator data for atom types in rule-based forcefields
	// We expect there to be the same number of sets of data as there are types...
	// Argument to 'generator' keyword is number of data per atom
	dbgBegin(Debug::Calls,"Forcefield::readGenerator");
	int count, success, n;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	count = 0;
	do
	{
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error while reading generator data for atom %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file while reading generator data for atom %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readGenerator");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Convert type name to internal index and read in generator data...
			// Format of lines is : ffid  typename data1  data2 ...
			// Typename is unused, but is present in the file to aid readability
			ffa = findType(parser.argi(0));
			if (ffa == NULL)
			{
				msg(Debug::None,"Unrecognised forcefield atom id in generator list: '%s'\n",parser.argc(0));
				dbgEnd(Debug::Calls,"Forcefield::readGenerator");
				return FALSE;
			}
			ffa->initialiseGenerator();
			for (n=0; n<MAXFFGENDATA; n++) ffa->setGenerator(n,parser.argd(n+2));
			count ++;
		}
	} while (!done);
	if (count != types_.nItems()-1)
	{
		msg(Debug::None,"Not all atom types had generator data defined (%i missing).\n", types_.nItems()-count-1);
		dbgEnd(Debug::Calls,"Forcefield::readGenerator");
		return FALSE;
	}
	msg(Debug::None,"\t: Read in generator data for %i atomtypes.\n", count);
	dbgEnd(Debug::Calls,"Forcefield::readGenerator");
	return TRUE;
}

bool Forcefield::readEquivalents(ifstream &fffile)
{
	/* Read in equivalent atom type names.
	By default, the 'equiv' name is set to the same as the atomtype name.
	Here, we search/replace specified definitions and set the equiv names to the first name in the list.
	The equivname doesn't have to exist in the atomtypes itself since the equivalent names are only used in intramolecular parameter searching.
	*/
	dbgBegin(Debug::Calls,"Forcefield::readEquivalents");
	int count, success, argpos;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	count = 0;
	do
	{
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error while reading equivalents data for atom %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file while reading equivalents data for atom %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readEquivalents");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Format of lines is : equivname_  fftype1  fftype2  ... fftypeN
			// Search atom types for typenames given in the list
			for (argpos=1; argpos<parser.nArgs(); argpos++)
			{
				for (ffa = types_.first(); ffa != NULL; ffa = ffa->next)
					if (matchType(ffa->name(),parser.argc(argpos)) < 10) ffa->setEquivalent(parser.argc(0));
			}
			count ++;
		}
	} while (!done);
	msg(Debug::None,"\t: Processed %i atomtype equivalents.\n",count);
	dbgEnd(Debug::Calls,"Forcefield::readEquivalents");
	return TRUE;
}

bool Forcefield::readVdw(ifstream &fffile)
{
	// Format of lines is: 'ffid  fftype  charge  data1  data2  ... dataN'
	dbgBegin(Debug::Calls,"Forcefield::readVdw");
	int success, count, n;
	ForcefieldAtom *ffa;
	// Get functional form of vdw
	VdwFunctions::VdwFunction vdwstyle = VdwFunctions::vdwFunction(parser.argc(1));
	if (vdwstyle == VdwFunctions::nVdwFunctions)
	{
		vdwstyle = VdwFunctions::None;
		msg(Debug::None,"VDW functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'ffid  fftype   charge  data1  data2  ...  dataN'
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error reading VDW data for atom %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file while reading VDW data for atom %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readVdw");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Need not specify the data in the same order as for the type data above, so search for the fftype read in...
			ffa = findType(parser.argi(0));
			if (ffa == NULL)
			{
				msg(Debug::None,"Unrecognised forcefield atom id in VDW list: '%s'\n", parser.argc(0));
				dbgEnd(Debug::Calls,"Forcefield::readVdw");
				return FALSE;
			}
			ffa->setCharge(parser.argd(2));
			for (n=0; n<MAXFFPARAMDATA; n++) if (!parser.isBlank(n+3)) ffa->params().data[n] = parser.argd(n+3);
			ffa->setVdwForm(vdwstyle);
			msg(Debug::Verbose,"VDW Data %i : %s q=%8.4f, f%8.4f %8.4f %8.4f %8.4f\n", ffa->typeId(), ffa->name(), ffa->charge(), ffa->params().data[0], ffa->params().data[1], ffa->params().data[2], ffa->params().data[3], ffa->params().data[4], ffa->params().data[5]);
			count ++;
		}
	} while (!done);
	msg(Debug::None,"\t: Read in %i atomic VDW parameters\n",count);
	dbgEnd(Debug::Calls,"Forcefield::readVdw");
	return TRUE;
}

bool Forcefield::readBonds(ifstream &fffile)
{
	// Read in bond specifications
	dbgBegin(Debug::Calls,"Forcefield::readBonds");
	ForcefieldBound *newffbond;
	bool done = FALSE;
	int count, success, n;
	// Get functional form of bond potential
	BondFunctions::BondFunction bondstyle = BondFunctions::bondFunction(parser.argc(1));
	if (bondstyle == BondFunctions::nBondFunctions)
	{
		bondstyle = BondFunctions::None;
		msg(Debug::None,"Bond stretch functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2   data1  data2  ...  dataN'
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error reading bond data %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file error reading bond data %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readBonds");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '?', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<2; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (findType(parser.argc(n)) == NULL))
					msg(Debug::None,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n", parser.argc(n));
			}
			// Create new ff_bond structure
			newffbond = bonds_.add();
			newffbond->setType(ForcefieldBound::BondInteraction);
			newffbond->setTypeName(0,parser.argc(0));
			newffbond->setTypeName(1,parser.argc(1));
			newffbond->setBondStyle(bondstyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (!parser.isBlank(n+2)) newffbond->params().data[n] = parser.argd(n+2);
			msg(Debug::Verbose,"BOND %i : %s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffbond->typeName(0), newffbond->typeName(1) , newffbond->params().data[0], newffbond->params().data[1], newffbond->params().data[2], newffbond->params().data[3], newffbond->params().data[4], newffbond->params().data[5]); 
			count ++;
		}
	} while (!done);
	msg(Debug::None,"\t: Read in %i bond definitions (%s)\n", count, BondFunctions::BondFunctions[bondstyle].name);
	dbgEnd(Debug::Calls,"Forcefield::readBonds");
	return TRUE;
}

bool Forcefield::readAngles(ifstream &fffile)
{
	// Read in angle specifications
	dbgBegin(Debug::Calls,"Forcefield::readAngles");
	ForcefieldBound *newffangle;
	int count, success, n;
	// Grab functional form of angle potential
	AngleFunctions::AngleFunction anglestyle = AngleFunctions::angleFunction(parser.argc(1));
	if (anglestyle == AngleFunctions::nAngleFunctions)
	{
		anglestyle = AngleFunctions::None;
		msg(Debug::None,"Angle bend functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error reading angle data %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file while reading angle data %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readAngles");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (findType(parser.argc(n)) == NULL))
					msg(Debug::None,"\t... Warning - angle atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newffangle = angles_.add();
			newffangle->setType(ForcefieldBound::AngleInteraction);
			newffangle->setTypeName(0, parser.argc(0));
			newffangle->setTypeName(1, parser.argc(1));
			newffangle->setTypeName(2, parser.argc(2));
			newffangle->setAngleStyle(anglestyle);
			for (n=0; n<MAXFFPARAMDATA; n++) if (!parser.isBlank(n+3)) newffangle->params().data[n] = parser.argd(n+3);
			msg(Debug::Verbose,"ANGLE %i : %s-%s-%s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newffangle->typeName(0), newffangle->typeName(1), newffangle->typeName(2), newffangle->params().data[0], newffangle->params().data[1], newffangle->params().data[2], newffangle->params().data[3], newffangle->params().data[4], newffangle->params().data[5]); 
			count ++;
		}
	} while (!done);
	msg(Debug::None,"\t: Read in %i angle definitions (%s)\n", count, AngleFunctions::AngleFunctions[anglestyle].name);
	dbgEnd(Debug::Calls,"Forcefield::readAngles");
	return TRUE;
}

bool Forcefield::readTorsions(ifstream &fffile)
{
	// Read in torsion data
	dbgBegin(Debug::Calls,"Forcefield::readTorsions");
	ForcefieldBound *newfftorsion;
	int count, success, n;
	// Get functional form of torsion potential
	TorsionFunctions::TorsionFunction torsionstyle = TorsionFunctions::torsionFunction(parser.argc(1));
	if (torsionstyle == TorsionFunctions::nTorsionFunctions)
	{
		torsionstyle = TorsionFunctions::None;
		msg(Debug::None,"Torsion twist functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	count = 0;
	bool done = FALSE;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		success = parser.getArgsDelim(&fffile,Parser::SkipBlanks);
		if (success != 0)
		{
			if (success == 1) msg(Debug::None,"File error reading torsion data %i.\n",count+1);
			if (success == -1) msg(Debug::None,"End of file error reading torsion data %i.\n",count+1);
			dbgEnd(Debug::Calls,"Forcefield::readTorsions");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (findType(parser.argc(n)) == NULL))
					msg(Debug::None,"\t... Warning - torsion atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newfftorsion = torsions_.add();
			newfftorsion->setType(ForcefieldBound::TorsionInteraction);
			newfftorsion->setTypeName(0,parser.argc(0));
			newfftorsion->setTypeName(1,parser.argc(1));
			newfftorsion->setTypeName(2,parser.argc(2));
			newfftorsion->setTypeName(3,parser.argc(3));
			newfftorsion->setTorsionStyle(torsionstyle);
			newfftorsion->params().data[0] = parser.argd(4);
			newfftorsion->params().data[1] = parser.argd(5);
			newfftorsion->params().data[2] = parser.argd(6);
			newfftorsion->params().data[3] = parser.argd(7);
			for (n=0; n<MAXFFPARAMDATA-2; n++) if (!parser.isBlank(n+4)) newfftorsion->params().data[n] = parser.argd(n+4);
			newfftorsion->params().data[TF_ESCALE] = escale14;
			newfftorsion->params().data[TF_VSCALE] = vscale14;
			msg(Debug::Verbose,"TORSION %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f %8.4f %8.4f\n", n, newfftorsion->typeName(0), newfftorsion->typeName(1), newfftorsion->typeName(2), newfftorsion->typeName(3), newfftorsion->params().data[0], newfftorsion->params().data[1], newfftorsion->params().data[2], newfftorsion->params().data[3], newfftorsion->params().data[4], newfftorsion->params().data[5]);
			count ++;
		}
	} while (!done);
	msg(Debug::None,"\t: Read in %i torsion definitions (%s)\n", count, TorsionFunctions::TorsionFunctions[torsionstyle].name);
	dbgEnd(Debug::Calls,"Forcefield::readTorsions");
	return TRUE;
}
