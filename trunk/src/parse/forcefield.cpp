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

// Forcefield keywords
const char *FF_dictionary[FFK_NITEMS] = { "_NULL_", "name", "units", "rules", "types", "generator", "convert", "equivalents", "vdw", "bonds", "angles", "torsions", "vscale", "escale" };
ForcefieldKeyword FFK_from_text(const char *s)
	{ return (ForcefieldKeyword) enumSearch("forcefield keyword",FFK_NITEMS,FF_dictionary,s); }

// Local variables
double escale14 = 0.5;
double vscale14 = 0.5;

// Load the specified forcefield
bool Forcefield::load(const char *filename)
{
	dbgBegin(DM_CALLS,"Forcefield::load");
	bool done, okay;
	int success, n, m, count;
	EnergyUnit ffunit = EU_J, newunit;
	ifstream fffile(filename,ios::in);
	if (!fffile.good())
	{
		fffile.close();
		msg(DM_NONE,"Failed to open forcefield file.\n");
		dbgEnd(DM_CALLS,"Forcefield::load");
		return FALSE;
	}
	// Grab the path of the forcefield
	path_.set(filename);
	// Now follows blocks of keywords
	done = FALSE;
	msg(DM_NONE,"Opening forcefield : %s...\n",filename);
	do
	{
		okay = FALSE;
		success = parser.getArgsDelim(&fffile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"EreadVdwor reading FF directive.\n");
			fffile.close();
			dbgEnd(DM_CALLS,"Forcefield::load");
			return FALSE;
		}
		if (success == -1) break;
		// Call subroutines to read in data based on keywords
		switch (FFK_from_text(parser.argc(0)))
		{
			case (FFK_NAME):
				name_.set(parser.argc(1));
				msg(DM_NONE,"\t: '%s'\n",name_.get());
				okay = TRUE;
				break;
			case (FFK_UNITS):
				newunit = EU_from_text(parser.argc(1));
				if (newunit != EU_NITEMS)
				{
					ffunit = newunit;
					msg(DM_NONE,"\t: Energy units are %s\n",text_from_EU(ffunit));
					okay = TRUE;
				}
				break;
			case (FFK_RULES):
				rules_ = FFR_from_text(parser.argc(1));
				msg(DM_NONE,"\t: Rule-set to use is '%s'\n", text_from_FFR(rules_));
				okay = TRUE;
				break;
			case (FFK_TYPES):
				okay = readTypes(fffile);
				break;
			case (FFK_GENERATOR):
				okay = readGenerator(fffile);
				break;
			case (FFK_EQUIVALENTS):
				okay = readEquivalents(fffile);
				break;
			case (FFK_CONVERT):
				// Check that generator data has been initialised
				if (nGenerators_ == 0) msg(DM_NONE, "\t: ERROR - Energetic parameters to convert must be specified *after* 'generator' keyword.\n");
				else for (n=1; n<parser.nArgs(); n++) energyGenerators_[parser.argi(n)-1] = TRUE;
				okay = !(nGenerators_ == 0);
				break;
			case (FFK_VDW):
				okay = readVdw(fffile);
				break;
			case (FFK_BONDS):
				okay = readBonds(fffile);
				break;
			case (FFK_ANGLES):
				okay = readAngles(fffile);
				break;
			case (FFK_TORSIONS):
				okay = readTorsions(fffile);
				break;
			case (FFK_VSCALE):
				vscale14 = parser.argd(1);	// 1-4 VDW scaling
				msg(DM_NONE,"\t: VDW 1-4 scale factor = %6.3f\n", vscale14);
				okay = TRUE;
				break;
			case (FFK_ESCALE):
				escale14 = parser.argd(1);	// 1-4 electrostatic scaling
				msg(DM_NONE,"\t: Electrostatic 1-4 scale factor = %6.3f\n", vscale14);
				okay = TRUE;
				break;
			default:
				msg(DM_NONE,"Unrecognised forcefield keyword '%s'\n.",parser.argc(0));
				break;
		}
		// Check on 'okay'
		if (!okay)
		{
			//msg(DM_NONE,"EreadVdwor reading forcefield file. Aborted.\n");
			dbgEnd(DM_CALLS,"Forcefield::load");
			fffile.close();
			return FALSE;
		}
	} while ( !fffile.eof() );
	fffile.close();
	// Last thing - convert energetic units in the forcefield to the internal units of the program
	convertParameters(ffunit);
	dbgEnd(DM_CALLS,"Forcefield::load");
	return TRUE;
}

// Read in forcefield atom types.
bool Forcefield::readTypes(ifstream &fffile)
{
	dbgBegin(DM_CALLS,"Forcefield::readTypes");
	int success, newffid;
	bool done;
	ForcefieldAtom *ffa, *idsearch;
	done = FALSE;
	// Create _NDEF_ type common to all FFs)
	ffa = types_.add();
	ffa->name_ = "_NDEF_";
	ffa->typeId_ = -1;
	done = FALSE;
	// Format of lines is 'ffid typename element description'
	do
	{
		success = parser.getArgsDelim(&fffile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor while reading atom type description %i.\n", types_.nItems());
			if (success == -1) msg(DM_NONE,"End of file while reading atom type description %i.\n", types_.nItems());
			dbgEnd(DM_CALLS,"Forcefield::readTypes");
			return FALSE;
		}
		else if (strcmp(parser.argc(0),"end") == 0) break;
		// Search for this ID to make sure it hasn't already been used
		newffid = parser.argi(0);
		idsearch = findType(newffid);
		if (idsearch != NULL)
		{
			msg(DM_NONE,"Duplicate forcefield type ID '%i' - already used by type '%s'.\n", newffid, idsearch->name_.get());
			dbgEnd(DM_CALLS,"Forcefield::readTypes");
			return FALSE;
		}
		ffa = types_.add();
		ffa->typeId_ = newffid;
		ffa->name_ = parser.argc(1);
		ffa->equivalent_ = parser.argc(1);
		ffa->atomType_.el = elements.find(parser.argc(2),ZM_ALPHA);
		ffa->description_ = parser.argc(4);
		ffa->atomType_.expand(parser.argc(3),this,ffa);
	} while (!done);
	if (types_.nItems() == 1)
	{
		msg(DM_NONE,"No atom types specified!\n");
		dbgEnd(DM_CALLS,"Forcefield::readTypes");
		return FALSE;
	}
	msg(DM_NONE,"\t: Read in %i type descriptions\n",types_.nItems());
	dbgEnd(DM_CALLS,"Forcefield::readTypes");
	return TRUE;
}

bool Forcefield::readGenerator(ifstream &fffile)
{
	// Read in generator data for atom types in rule-based forcefields
	// We expect there to be the same number of sets of data as there are types...
	// Argument to 'generator' keyword is number of data per atom
	dbgBegin(DM_CALLS,"Forcefield::readGenerator");
	int count, success, n;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	// If we are setting nGenerators_ for the first time, allocate convertgen as well
	if (nGenerators_ == 0)
	{
		energyGenerators_ = new bool[nGenerators_];
		nGenerators_ = parser.argi(1);
	}
	count = 0;
	do
	{
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor while reading generator data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading generator data for atom %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readGenerator");
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
				msg(DM_NONE,"Unrecognised forcefield atom id in generator list: '%s'\n",parser.argc(0));
				dbgEnd(DM_CALLS,"Forcefield::readGenerator");
				return FALSE;
			}
			ffa->generator_ = new double[nGenerators_];
			for (n=0; n<nGenerators_; n++) ffa->generator_[n] = parser.argd(n+2);
			count ++;
		}
	} while (!done);
	if (count != types_.nItems()-1)
	{
		msg(DM_NONE,"Not all (%i) atom types had generator data defined.\n", types_.nItems()-count-1);
		dbgEnd(DM_CALLS,"Forcefield::readGenerator");
		return FALSE;
	}
	msg(DM_NONE,"\t: Read in %i generator data for %i atomtypes.\n", nGenerators_, count);
	dbgEnd(DM_CALLS,"Forcefield::readGenerator");
	return TRUE;
}

bool Forcefield::readEquivalents(ifstream &fffile)
{
	// Read in equivalent atom type names.
	// By default, the 'equiv' name is set to the same as the atomtype name. Here, we search/replace specified
	// definitions and set the equiv names to the first name in the list. The equivname doesn't have to exist in the
	// atomtypes itself since the equivalent names are only used in intramolecular parameter searching.
	dbgBegin(DM_CALLS,"Forcefield::readEquivalents");
	int count, success, argpos;
	ForcefieldAtom *ffa;
	bool done = FALSE;
	count = 0;
	do
	{
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor while reading equivalents data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading equivalents data for atom %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readEquivalents");
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
					if (matchType(ffa->name_.get(),parser.argc(argpos)) < 10) ffa->equivalent_ = parser.argc(0);
			}
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Processed %i atomtype equivalents.\n",count);
	dbgEnd(DM_CALLS,"Forcefield::readEquivalents");
	return TRUE;
}

bool Forcefield::readVdw(ifstream &fffile)
{
	// Format of lines is: 'fftype  charge  data1  data2  ... dataN'
	// Need not specify the data in the same order as for the type data above,
	// so search for the fftype read in...
	dbgBegin(DM_CALLS,"Forcefield::readVdw");
	int success, count;
	ForcefieldAtom *ffa;
	// Get functional form of vdw
	VdwFunction vdwstyle = VF_from_text(parser.argc(1));
	if (vdwstyle == VF_NITEMS)
	{
		vdwstyle = VF_UNSPECIFIED;
		msg(DM_NONE,"VDW functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	// TODO allow 'same' directive?
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'ffid  fftype   charge  data1  data2  ...  dataN'
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor reading VDW data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading VDW data for atom %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readVdw");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			ffa = findType(parser.argi(0));
			if (ffa == NULL)
			{
				msg(DM_NONE,"Unrecognised forcefield atom id in VDW list: '%s'\n",parser.argc(0));
				dbgEnd(DM_CALLS,"Forcefield::readVdw");
				return FALSE;
			}
			ffa->charge_ = parser.argd(2);
			ffa->params_.data[0] = parser.argd(3);
			ffa->params_.data[1] = parser.argd(4);
			ffa->params_.data[2] = parser.argd(5);
			ffa->setVdwForm(vdwstyle);
			msg(DM_VERBOSE,"VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->typeId_, ffa->name_.get(), ffa->params_.data[0], ffa->params_.data[1], ffa->params_.data[2], ffa->charge_);
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i atomic VDW parameters\n",count);
	dbgEnd(DM_CALLS,"Forcefield::readVdw");
	return TRUE;
}

bool Forcefield::readBonds(ifstream &fffile)
{
	// Read in bond specifications
	dbgBegin(DM_CALLS,"Forcefield::readBonds");
	ForcefieldBound *newffbond;
	bool done = FALSE;
	int count, success, n;
	// Get functional form of bond potential
	BondFunction bondstyle = BF_from_text(parser.argc(1));
	if (bondstyle == BF_NITEMS)
	{
		bondstyle = BF_UNSPECIFIED;
		msg(DM_NONE,"Bond stretch functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2   data1  data2  ...  dataN'
		// N.B. If data1 == 'same' then reuse the last data read in.
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor reading bond data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file ereadVdwor reading bond data %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readBonds");
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
					msg(DM_NONE,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n", parser.argc(n));
			}
			// Create new ff_bond structure
			newffbond = bonds_.add();
			newffbond->setType(FFC_BOND);
			newffbond->atomTypes_[0] = parser.argc(0);
			newffbond->atomTypes_[1] = parser.argc(1);
			newffbond->setBondStyle(bondstyle);
			newffbond->params_.data[0] = parser.argd(2);
			newffbond->params_.data[1] = parser.argd(3);
			msg(DM_VERBOSE,"BOND %i : %s  %s  %8.4f %8.4f\n", n, newffbond->atomTypes_[0].get(), newffbond->atomTypes_[1].get() , newffbond->params_.data[0], newffbond->params_.data[1]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i bond definitions (%s)\n",count,text_from_BF(bondstyle));
	dbgEnd(DM_CALLS,"Forcefield::readBonds");
	return TRUE;
}

bool Forcefield::readAngles(ifstream &fffile)
{
	// Read in angle specifications
	dbgBegin(DM_CALLS,"Forcefield::readAngles");
	ForcefieldBound *newffangle;
	int count, success, n;
	// Grab functional form of angle potential
	AngleFunction anglestyle = AF_from_text(parser.argc(1));
	if (anglestyle == AF_NITEMS)
	{
		anglestyle = AF_UNSPECIFIED;
		msg(DM_NONE,"Angle bend functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	bool done = FALSE;
	count = 0;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  data1  data2  ...  dataN'
		// N.B. If data1 == 'same' then reuse the last data read in.
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor reading angle data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file ereadVdwor reading angle data %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readAngles");
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
					msg(DM_NONE,"\t... Warning - angle atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newffangle = angles_.add();
			newffangle->setType(FFC_ANGLE);
			newffangle->atomTypes_[0] = parser.argc(0);
			newffangle->atomTypes_[1] = parser.argc(1);
			newffangle->atomTypes_[2] = parser.argc(2);
			newffangle->setAngleStyle(anglestyle);
			newffangle->params_.data[0] = parser.argd(3);
			newffangle->params_.data[1] = parser.argd(4);
			msg(DM_VERBOSE,"ANGLE %i : %s  %s  %s  %8.4f %8.4f\n", n, newffangle->atomTypes_[0].get(), newffangle->atomTypes_[1].get(), newffangle->atomTypes_[2].get(), newffangle->params_.data[0], newffangle->params_.data[1]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i angle definitions (%s)\n",count,text_from_AF(anglestyle));
	dbgEnd(DM_CALLS,"Forcefield::readAngles");
	return TRUE;
}

bool Forcefield::readTorsions(ifstream &fffile)
{
	// Read in torsion data
	dbgBegin(DM_CALLS,"Forcefield::readTorsions");
	ForcefieldBound *newfftorsion;
	int count, success, n;
	// Get functional form of torsion potential
	TorsionFunction torsionstyle = TF_from_text(parser.argc(1));
	if (torsionstyle == TF_NITEMS)
	{
		torsionstyle = TF_UNSPECIFIED;
		msg(DM_NONE,"Torsion twist functional form not recognised - '%s'\n",parser.argc(1));
		return FALSE;
	}
	count = 0;
	bool done = FALSE;
	do
	{
		// Format of lines is: 'fftype1  fftype2  fftype3  fftype4  data1  data2  ...  dataN'
		// N.B. If data1 == 'same' then reuse the last data read in.
		success = parser.getArgsDelim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File ereadVdwor reading torsion data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file ereadVdwor reading torsion data %i.\n",count+1);
			dbgEnd(DM_CALLS,"Forcefield::readTorsions");
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
					msg(DM_NONE,"\t... Warning - torsion atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newfftorsion = torsions_.add();
			newfftorsion->setType(FFC_TORSION);
			newfftorsion->atomTypes_[0] = parser.argc(0);
			newfftorsion->atomTypes_[1] = parser.argc(1);
			newfftorsion->atomTypes_[2] = parser.argc(2);
			newfftorsion->atomTypes_[3] = parser.argc(3);
			newfftorsion->setTorsionStyle(torsionstyle);
			newfftorsion->params_.data[0] = parser.argd(4);
			newfftorsion->params_.data[1] = parser.argd(5);
			newfftorsion->params_.data[2] = parser.argd(6);
			newfftorsion->params_.data[3] = parser.argd(7);
			newfftorsion->params_.data[TF_ESCALE] = escale14;
			newfftorsion->params_.data[TF_VSCALE] = vscale14;
			msg(DM_VERBOSE,"TORSION %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f\n", n, newfftorsion->atomTypes_[0].get(), newfftorsion->atomTypes_[1].get(), newfftorsion->atomTypes_[2].get(), newfftorsion->atomTypes_[3].get(), newfftorsion->params_.data[0], newfftorsion->params_.data[1], newfftorsion->params_.data[2], newfftorsion->params_.data[3]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i torsion definitions (%s)\n",count,text_from_TF(torsionstyle));
	dbgEnd(DM_CALLS,"Forcefield::readTorsions");
	return TRUE;
}
