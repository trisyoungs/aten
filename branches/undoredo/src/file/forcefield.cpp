/*
	*** Forcefield import
	*** src/file/forcefield.cpp
	Copyright T. Youngs 2007

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

#include <fstream>
#include "classes/forcefield.h"
#include "base/elements.h"
#include "file/parse.h"
#include "base/sysfunc.h"

// Forcefield keywords
const char *FF_dictionary[FFK_NITEMS] = { "_NULL_", "name", "units", "rules", "types", "generator", "convert", "equivalents", "vdw", "bonds", "angles", "torsions", "vscale", "escale" };
ff_dict FFK_from_text(const char *s)
	{ return (ff_dict) enum_search("forcefield keyword",FFK_NITEMS,FF_dictionary,s); }

// Local variables
double escale14 = 0.5;
double vscale14 = 0.5;

// Load the specified forcefield
bool forcefield::load(const char *filename)
{
	dbg_begin(DM_CALLS,"forcefield::load");
	bool done, okay;
	int success, n, m, count;
	energy_unit ffunit = EU_J, newunit;
	ifstream fffile(filename,ios::in);
	if (!fffile.good())
	{
		fffile.close();
		msg(DM_NONE,"Failed to open forcefield file.\n");
		dbg_end(DM_CALLS,"forcefield::load");
		return FALSE;
	}
	// Grab the path of the forcefield
	path.set(filename);
	// Now follows blocks of keywords
	done = FALSE;
	msg(DM_NONE,"Opening forcefield : %s...\n",filename);
	do
	{
		okay = FALSE;
		success = parser.get_args_delim(&fffile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success == 1)
		{
			msg(DM_NONE,"Error reading FF directive.\n");
			fffile.close();
			dbg_end(DM_CALLS,"forcefield::load");
			return FALSE;
		}
		if (success == -1) break;
		// Call subroutines to read in data based on keywords
		switch (FFK_from_text(parser.argc(0)))
		{
			case (FFK_NAME):
				ffname.set(parser.argc(1));
				msg(DM_NONE,"\t: '%s'\n",ffname.get());
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
				rules = FFR_from_text(parser.argc(1));
				msg(DM_NONE,"\t: Rule-set to use is '%s'\n",text_from_FFR(rules));
				okay = TRUE;
				break;
			case (FFK_TYPES):
				okay = read_types(fffile);
				break;
			case (FFK_GENERATOR):
				okay = read_generator(fffile);
				break;
			case (FFK_EQUIVALENTS):
				okay = read_equivalents(fffile);
				break;
			case (FFK_CONVERT):
				// Check that generator data has been initialised
				if (ngendata == 0) msg(DM_NONE, "\t: ERROR - Energetic parameters to convert must be specified *after* 'generator' keyword.\n");
				else for (n=1; n<parser.get_nargs(); n++) convertgen[parser.argi(n)-1] = TRUE;
				okay = !(ngendata == 0);
				break;
			case (FFK_VDW):
				okay = read_vdw(fffile);
				break;
			case (FFK_BONDS):
				okay = read_bonds(fffile);
				break;
			case (FFK_ANGLES):
				okay = read_angles(fffile);
				break;
			case (FFK_TORSIONS):
				okay = read_torsions(fffile);
				break;
			case (FFK_VSCALE):
				vscale14 = parser.argd(1);	// 1-4 VDW scaling
				msg(DM_NONE,"\t: VDW 1-4 scale factor = %6.3f\n",vscale14);
				okay = TRUE;
				break;
			case (FFK_ESCALE):
				escale14 = parser.argd(1);	// 1-4 electrostatic scaling
				msg(DM_NONE,"\t: Electrostatic 1-4 scale factor = %6.3f\n",vscale14);
				okay = TRUE;
				break;
			default:
				msg(DM_NONE,"Unrecognised forcefield keyword '%s'\n.",parser.argc(0));
				break;
		}
		// Check on 'okay'
		if (!okay)
		{
			//msg(DM_NONE,"Error reading forcefield file. Aborted.\n");
			dbg_end(DM_CALLS,"forcefield::load");
			fffile.close();
			return FALSE;
		}
	} while ( !fffile.eof() );
	fffile.close();
	// Last thing - convert energetic units in the forcefield to the internal units of the program
	convert_parameters(ffunit);
	dbg_end(DM_CALLS,"forcefield::load");
	return TRUE;
}

// Read in forcefield atom types.
bool forcefield::read_types(ifstream &fffile)
{
	dbg_begin(DM_CALLS,"forcefield::read_types");
	int success, newffid;
	bool done;
	ffatom *ffa, *idsearch;
	done = FALSE;
	// Create _NDEF_ type common to all FFs)
	ffa = atomtypes.add();
	ffa->name = "_NDEF_";
	ffa->ffid = -1;
	done = FALSE;
	// Format of lines is 'ffid typename element description'
	do
	{
		success = parser.get_args_delim(&fffile,PO_USEQUOTES+PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error while reading atom type description %i.\n", atomtypes.size());
			if (success == -1) msg(DM_NONE,"End of file while reading atom type description %i.\n", atomtypes.size());
			dbg_end(DM_CALLS,"forcefield::read_types");
			return FALSE;
		}
		else if (strcmp(parser.argc(0),"end") == 0) break;
		// Search for this ID to make sure it hasn't already been used
		newffid = parser.argi(0);
		idsearch = find_type(newffid);
		if (idsearch != NULL)
		{
			msg(DM_NONE,"Duplicate forcefield type ID '%i' - already used by type '%s'.\n",newffid,idsearch->name.get());
			dbg_end(DM_CALLS,"forcefield::read_types");
			return FALSE;
		}
		ffa = atomtypes.add();
		ffa->ffid = newffid;
		ffa->name = parser.argc(1);
		ffa->equiv = parser.argc(1);
		ffa->typedesc.el = elements.find(parser.argc(2),ZM_ALPHA);
		ffa->typedesc.expand(parser.argc(3),this,ffa);
		ffa->description = parser.argc(4);
		//printf("ATOMTYPE %i : Description = %s\n",n,parser.argc(3));
	} while (!done);
	if (atomtypes.size() == 1)
	{
		msg(DM_NONE,"No atom types specified!\n");
		dbg_end(DM_CALLS,"forcefield::read_types");
		return FALSE;
	}
	msg(DM_NONE,"\t: Read in %i type descriptions\n",atomtypes.size());
	dbg_end(DM_CALLS,"forcefield::read_types");
	return TRUE;
}

bool forcefield::read_generator(ifstream &fffile)
{
	// Read in generator data for atom types in rule-based forcefields
	// We expect there to be the same number of sets of data as there are types...
	// Argument to 'generator' keyword is number of data per atom
	dbg_begin(DM_CALLS,"forcefield::read_generator");
	int count, success, n;
	ffatom *ffa;
	bool done = FALSE;
	// If we are setting ngendata for the first time, allocate convertgen as well
	if (ngendata == 0)
	{
		ngendata = parser.argi(1);
		convertgen = new bool[ngendata];
	}
	count = 0;
	do
	{
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error while reading generator data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading generator data for atom %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_generator");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Convert type name to internal index and read in generator data...
			// Format of lines is : ffid  typename data1  data2 ...
			// Typename is unused, but is present in the file to aid readability
			ffa = find_type(parser.argi(0));
			if (ffa == NULL)
			{
				msg(DM_NONE,"Unrecognised forcefield atom id in generator list: '%s'\n",parser.argc(0));
				dbg_end(DM_CALLS,"forcefield::read_generator");
				return FALSE;
			}
			ffa->generator = new double[ngendata];
			for (n=0; n<ngendata; n++) ffa->generator[n] = parser.argd(n+2);
			count ++;
		}
	} while (!done);
	if (count != atomtypes.size()-1)
	{
		msg(DM_NONE,"Not all (%i) atom types had generator data defined.\n",atomtypes.size()-count-1);
		dbg_end(DM_CALLS,"forcefield::read_generator");
		return FALSE;
	}
	msg(DM_NONE,"\t: Read in %i generator data for %i atomtypes.\n",ngendata,count);
	dbg_end(DM_CALLS,"forcefield::read_generator");
	return TRUE;
}

bool forcefield::read_equivalents(ifstream &fffile)
{
	// Read in equivalent atom type names.
	// By default, the 'equiv' name is set to the same as the atomtype name. Here, we search/replace specified
	// definitions and set the equiv names to the first name in the list. The equivname doesn't have to exist in the
	// atomtypes itself since the equivalent names are only used in intramolecular parameter searching.
	dbg_begin(DM_CALLS,"forcefield::read_equivalents");
	int count, success, argpos;
	ffatom *ffa;
	bool done = FALSE;
	count = 0;
	do
	{
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error while reading equivalents data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading equivalents data for atom %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_equivalents");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Format of lines is : equivffname  fftype1  fftype2  ... fftypeN
			// Search atom types for typenames given in the list
			for (argpos=1; argpos<parser.get_nargs(); argpos++)
			{
				for (ffa = atomtypes.first(); ffa != NULL; ffa = ffa->next)
					if (match_type(ffa->name.get(),parser.argc(argpos)) < 10) ffa->equiv = parser.argc(0);
			}
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Processed %i atomtype equivalents.\n",count);
	dbg_end(DM_CALLS,"forcefield::read_equivalents");
	return TRUE;
}

bool forcefield::read_vdw(ifstream &fffile)
{
	// Format of lines is: 'fftype  charge  data1  data2  ... dataN'
	// Need not specify the data in the same order as for the type data above,
	// so search for the fftype read in...
	dbg_begin(DM_CALLS,"forcefield::read_vdw");
	int success, count;
	ffatom *ffa;
	// Get functional form of vdw
	vdw_func vdwstyle = VF_from_text(parser.argc(1));
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
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error reading VDW data for atom %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file while reading VDW data for atom %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_vdw");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			ffa = find_type(parser.argi(0));
			if (ffa == NULL)
			{
				msg(DM_NONE,"Unrecognised forcefield atom id in VDW list: '%s'\n",parser.argc(0));
				dbg_end(DM_CALLS,"forcefield::read_vdw");
				return FALSE;
			}
			ffa->q = parser.argd(2);
			ffa->params.data[0] = parser.argd(3);
			ffa->params.data[1] = parser.argd(4);
			ffa->params.data[2] = parser.argd(5);
			ffa->set_style(vdwstyle);
			msg(DM_VERBOSE,"VDW Data %i : %s %8.4f %8.4f %8.4f %8.4f\n", ffa->ffid, ffa->name.get(), ffa->params.data[0], ffa->params.data[1], ffa->params.data[2], ffa->q);
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i atomic VDW parameters\n",count);
	dbg_end(DM_CALLS,"forcefield::read_vdw");
	return TRUE;
}

bool forcefield::read_bonds(ifstream &fffile)
{
	// Read in bond specifications
	dbg_begin(DM_CALLS,"forcefield::read_bonds");
	ffbound *newffbond;
	bool done = FALSE;
	int count, success, n;
	// Get functional form of bond potential
	bond_func bondstyle = BF_from_text(parser.argc(1));
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
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error reading bond data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file error reading bond data %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_bonds");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '?', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<2; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (find_type(parser.argc(n)) == NULL))
					msg(DM_NONE,"\t... Warning - bond atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_bond structure
			newffbond = bonds.add();
			newffbond->set_type(FFC_BOND);
			newffbond->types[0] = parser.argc(0);
			newffbond->types[1] = parser.argc(1);
			newffbond->set_bond_style(bondstyle);
			newffbond->params.data[0] = parser.argd(2);
			newffbond->params.data[1] = parser.argd(3);
			msg(DM_VERBOSE,"BOND %i : %s  %s  %8.4f %8.4f\n",n,newffbond->types[0].get(), newffbond->types[1].get() , newffbond->params.data[0], newffbond->params.data[1]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i bond definitions (%s)\n",count,text_from_BF(bondstyle));
	dbg_end(DM_CALLS,"forcefield::read_bonds");
	return TRUE;
}

bool forcefield::read_angles(ifstream &fffile)
{
	// Read in angle specifications
	dbg_begin(DM_CALLS,"forcefield::read_angles");
	ffbound *newffangle;
	int count, success, n;
	// Grab functional form of angle potential
	angle_func anglestyle = AF_from_text(parser.argc(1));
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
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error reading angle data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file error reading angle data %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_angles");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<3; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (find_type(parser.argc(n)) == NULL))
					msg(DM_NONE,"\t... Warning - angle atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newffangle = angles.add();
			newffangle->set_type(FFC_ANGLE);
			newffangle->types[0] = parser.argc(0);
			newffangle->types[1] = parser.argc(1);
			newffangle->types[2] = parser.argc(2);
			newffangle->set_angle_style(anglestyle);
			newffangle->params.data[0] = parser.argd(3);
			newffangle->params.data[1] = parser.argd(4);
			msg(DM_VERBOSE,"ANGLE %i : %s  %s  %s  %8.4f %8.4f\n",n,newffangle->types[0].get(), newffangle->types[1].get(), newffangle->types[2].get(), newffangle->params.data[0], newffangle->params.data[1]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i angle definitions (%s)\n",count,text_from_AF(anglestyle));
	dbg_end(DM_CALLS,"forcefield::read_angles");
	return TRUE;
}

bool forcefield::read_torsions(ifstream &fffile)
{
	// Read in torsion data
	dbg_begin(DM_CALLS,"forcefield::read_torsions");
	ffbound *newfftorsion;
	int count, success, n;
	// Get functional form of torsion potential
	torsion_func torsionstyle = TF_from_text(parser.argc(1));
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
		success = parser.get_args_delim(&fffile,PO_SKIPBLANKS);
		if (success != 0)
		{
			if (success == 1) msg(DM_NONE,"File error reading torsion data %i.\n",count+1);
			if (success == -1) msg(DM_NONE,"End of file error reading torsion data %i.\n",count+1);
			dbg_end(DM_CALLS,"forcefield::read_torsions");
			return FALSE;
		}
		if (strcmp(parser.argc(0),"end") == 0) done = TRUE;
		else
		{
			// Do the best checking we can on the fftypes. If one contains a wildcard '*', then we must allow it.
			// If not, then check to see that it references an atomname in the atomtypes list
			for (n=0; n<4; n++)
			{
				if ((strchr(parser.argc(n),'*') == NULL) && (find_type(parser.argc(n)) == NULL))
					msg(DM_NONE,"\t... Warning - torsion atom '%s' does not exist in the forcefield!\n",parser.argc(n));
			}
			// Create new ff_angle structure
			newfftorsion = torsions.add();
			newfftorsion->set_type(FFC_TORSION);
			newfftorsion->types[0] = parser.argc(0);
			newfftorsion->types[1] = parser.argc(1);
			newfftorsion->types[2] = parser.argc(2);
			newfftorsion->types[3] = parser.argc(3);
			newfftorsion->set_torsion_style(torsionstyle);
			newfftorsion->params.data[0] = parser.argd(4);
			newfftorsion->params.data[1] = parser.argd(5);
			newfftorsion->params.data[2] = parser.argd(6);
			newfftorsion->params.data[3] = parser.argd(7);
			newfftorsion->params.data[TF_ESCALE] = escale14;
			newfftorsion->params.data[TF_VSCALE] = vscale14;
			msg(DM_VERBOSE,"TORSION %i : %s  %s  %s  %s  %8.4f %8.4f %8.4f %8.4f\n",n, newfftorsion->types[0].get(), newfftorsion->types[1].get(), newfftorsion->types[2].get(), newfftorsion->types[3].get(), newfftorsion->params.data[0], newfftorsion->params.data[1], newfftorsion->params.data[2], newfftorsion->params.data[3]); 
			count ++;
		}
	} while (!done);
	msg(DM_NONE,"\t: Read in %i torsion definitions (%s)\n",count,text_from_TF(torsionstyle));
	dbg_end(DM_CALLS,"forcefield::read_torsions");
	return TRUE;
}
