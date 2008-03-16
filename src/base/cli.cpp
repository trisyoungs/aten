/*
	*** Command-line option parsing
	*** src/base/cli.cpp
	Copyright T. Youngs 2007,2008

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

#include <iostream>
#include "base/cli.h"

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argtext,description)
optiondata clioptions[] = {
	{ CO_BOHR,		'b',"bohr",		0,
		"",		"Converts model atomic positions from Bohr to Angstrom" },
	{ CO_BOND,		'\0',"bond",		0,
		"",		"Force (re)calculation of bonding in the model" },
	{ CO_CACHE,		'\0',"cachelimit",	1,
		"<limit>",	"Set the trajectory cache limit to <limit> kb"},
	{ CO_CENTRE,		'\0',"centre",		0,
		"",		"Force centering of atomic coordinates at zero" },
	{ CO_COMMAND,		'c',"command",		1,
		"<commands>", "Execute supplied commands before main program execution" },
	{ CO_FF,		'\0',"ff",		1,
		"<file>",	"Load the specified forcefield file" },
	{ CO_FOLD,		'\0',"fold",		0,
		"",		"Force folding of atoms in periodic systems" },
	{ CO_FORMAT,		'f',"format",		0,
		"",		"Load models from command-line with specified <format>" },
	{ CO_GRID,		'g',"grid",		1,
		"<file>",	"Load the specified gridded data file" },
	{ CO_HELP,		'h',"help",		0,
		"",		"Print this information" },
	{ CO_INTERACTIVE,	'i',"interactive",	0,
		"",		"Enter interactive mode" },
	{ CO_MAP,		'm',"map",		1,
		"<name=element,...>",	"Map file atomtypes to elements" },
	{ CO_NOBOND,		'\0',"nobond",		0,
		"",		"Prevent (re)calculation of bonding in the model" },
	{ CO_NOCENTRE,		'\0',"nocentre",	0,
		"",		"Prevent centering of atomic coordinates at zero" },
	{ CO_NOFOLD,		'\0',"nofold",		0,
		"",		"Prevent folding of atoms in periodic systems" },
	{ CO_NOPACK,		'\0',"nopack",		0,
		"",		"Prevent generation of symmetry-equivalent atoms from spacegroup information" },
	{ CO_PACK,		'\0',"pack",		0,
		"",		"Force generation of symmetry-equivalent atoms from spacegroup information" },
	{ CO_SCRIPT,		's',"script",		1,
		"<file",	"Load and execute the script file specified" },
	{ CO_UNDO,		'u',"maxundo",		1,
		"<nlevels>",	"Set the maximum number of undo levels per model (-1 = unlimited)" },
	{ CO_ZMAP,		'z',"zmap",		1,
		"<mapstyle>",	"Override filter element mapping style" },
	{ CO_DEBUG,		'd',"debug",		0,
		"",		"Print major subroutine call information" },
	{ CO_DEBUGALL,		'\0',"debugall",	0,
		"",		"Print out all debug information" },
	{ CO_DEBUGFILE,		'\0',"debugfile",	0,
		"",		"Print out verbose information from file filter routines" },
	{ CO_DEBUGMORE,		'\0',"debugmore",	0,
		"",		"Print all subroutine call information" },
	{ CO_DEBUGPARSE,	'\0',"debugparse",	0,
		"",		"Print out verbose information from file parsing routines" },
	{ CO_DEBUGTYPING,	'\0',"debugtyping",	0,
		"",		"Print out verbose information from atom typing routines" },
	{ CO_VERBOSE,		'v',"verbose",		0,
		"",		"Enable verbose program output" }
};

// Parse debug options
void master_data::debug_cli(int argc, char *argv[])
{
	int n, o;
	bool isshort, match;
	char *arg;
	// Cycle over program arguments and available CLI options
	n = 0;
	while (n < argc)
	{
		// If first character is not '-' then continue
		if (argv[n][0] != '-') { n++; continue; }
		// Is this a long or short option?
		isshort = (argv[n][1] != '-');
		arg = (isshort ? &argv[n][1] : &argv[n][2]);
		// Cycle over defined CLI options and search for this one
		match = FALSE;
		for (o=0; o<CO_NITEMS; o++)
		{
			// Check short option character or long option text
			if (isshort) match = (*arg == clioptions[o].shortopt);
			else match = (strcmp(arg,clioptions[o].longopt) == 0 ? TRUE : FALSE);
			if (match) break;
		}
		// If we have a match then 'o' contains the option identifier
		// Only look for debug options here...
		if (match && (o >= CO_DEBUG))
		{
			switch (o)
			{
				// Turn on call debugging
				case (CO_DEBUG):
					add_debuglevel(DM_CALLS);
					break;
				// Turn on debug messages for atom typing
				case (CO_DEBUGTYPING):
					add_debuglevel(DM_TYPING);
					break;
				// Turn on debug messages for atom typing
				case (CO_DEBUGPARSE):
					add_debuglevel(DM_PARSE);
					break;
				// Turn on debug messages for atom typing
				case (CO_DEBUGFILE):
					add_debuglevel(DM_FILTERS);
					break;
				// Turn on debug messages for more calls
				case (CO_DEBUGMORE):
					add_debuglevel(DM_CALLS);
					add_debuglevel(DM_MORECALLS);
					break;
				// Turn on debug messages for all calls
				case (CO_DEBUGALL):
					add_debuglevel(DM_CALLS);
					add_debuglevel(DM_MORECALLS);
					add_debuglevel(DM_VERBOSE);
					add_debuglevel(DM_PARSE);
					add_debuglevel(DM_TYPING);
					break;
				// Turn on verbose messaging
				case (CO_VERBOSE):
					add_debuglevel(DM_VERBOSE);
					break;
			}
		}
		// Next option...
		n++;
	}
}

// Parse all options
int master_data::parse_cli(int argc, char *argv[])
{
	int n, o, ntried;
	bool isshort, match;
	char *arg;
	commandlist *cl;
	zmap_type zm;
	filter *f, *modelfilter = NULL;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary run)
	n = 1;
	while (n < argc)
	{
		match = FALSE;
		// Check for a CLI argument (presence of '-')
		if (argv[n][0] == '-')
		{
			// Is this a long or short option?
			isshort = (argv[n][1] != '-');
			arg = (isshort ? &argv[n][1] : &argv[n][2]);
			// Cycle over defined CLI options and search for this one
			for (o=0; o<CO_NITEMS; o++)
			{
				// Check short option character or long option text
				if (isshort) match = (*arg == clioptions[o].shortopt);
				else match = (strcmp(arg,clioptions[o].longopt) == 0 ? TRUE : FALSE);
				if (match) break;
			}
			// If we have a match then 'o' contains the option identifier. Otherwise try to load the argument as a model.
			if (match)
			{
				// If it's a debug option then we've already dealt with it
				if (o >= CO_DEBUG) break;
				switch (o)
				{
					// Convert coordinates from Bohr to Angstrom
					case (CO_BOHR):
						prefs.set_coords_in_bohr(TRUE);
						break;
					// Force bonding calculation of atoms on load
					case (CO_BOND):
						prefs.set_bond_on_load(PS_YES);
						break;
					// Set trajectory cache limit
					case (CO_CACHE):
						prefs.set_cache_limit(atoi(argv[++n]));
						break;
					// Force model centering on load (for non-periodic systems)
					case (CO_CENTRE):
						prefs.set_centre_on_load(PS_YES);
						break;
					// Read script commands from passed string
					case (CO_COMMAND):
						cl = master.commands.add();
						if (cl->cache_line(argv[++n])) master.set_program_mode(PM_COMMAND);
						else
						{
							master.commands.remove(cl);
							return -1;
						}
						break;
					// Load the specified forcefield
					case (CO_FF):
						master.load_ff(argv[++n]);
						break;
					// Force folding (MIM'ing) of atoms in periodic systems on load
					case (CO_FOLD):
						prefs.set_fold_on_load(PS_YES);
						break;
					// Set forced model load format
					case (CO_FORMAT):
						modelfilter = master.find_filter(FT_MODEL_IMPORT, argv[++n]);
						if (modelfilter == NULL) return -1;
						break;
					// Load surface
					case (CO_GRID):
						f = master.probe_file(argv[++n], FT_GRID_IMPORT);
						if (f != NULL) f->execute(argv[++n]);
						break;
					// Display help
					case (CO_HELP):
						print_usage();
						return -1;
						break;
					// Enter interactive mode
					case (CO_INTERACTIVE):
						master.set_program_mode(PM_INTERACTIVE);
						break;
					// Set type mappings
					case (CO_MAP):
						break;
					// Prohibit bonding calculation of atoms on load
					case (CO_NOBOND):
						prefs.set_bond_on_load(PS_NO);
						break;
					// Prohibit model centering on load (for non-periodic systems)
					case (CO_NOCENTRE):
						prefs.set_centre_on_load(PS_NO);
						break;
					// Prohibit folding (MIM'ing) of atoms in periodic systems on load
					case (CO_NOFOLD):
						prefs.set_fold_on_load(PS_NO);
						break;
					// Force packing (application of symmetry operators) on load
					case (CO_NOPACK):
						prefs.set_pack_on_load(PS_NO);
						break;
					// Prohibit packing (application of symmetry operators) on load
					case (CO_PACK):
						prefs.set_pack_on_load(PS_YES);
						break;
					// Cache a script file
					case (CO_SCRIPT):
						cl = master.scripts.add();
						if (cl->load(argv[++n])) master.set_program_mode(PM_COMMAND);
						else
						{
							master.scripts.remove(cl);
							return -1;
						}
						break;
					// Set maximum number of undolevels per model
					case (CO_UNDO):
						prefs.set_maxundo(atoi(argv[++n]));
						break;
					// Set the type of element (Z) mapping to use in name conversion
					case (CO_ZMAP):
						zm = ZM_from_text(argv[++n]);
						if (zm != ZM_NITEMS) prefs.set_zmapping(zm);
						break;
					default:
						printf("Unrecognised command-line option '%s'.\n",argv[n]);
						dbg_end(DM_CALLS,"cli::parse");
						return -1;
				}
			}
		}
		else
		{
			// Not a CLI switch, so try to load it as a model
			ntried ++;
			if (modelfilter != NULL) f = modelfilter;
			else f = master.probe_file(argv[n], FT_MODEL_IMPORT);
			if (f != NULL) f->execute(argv[n]);
		}
		n++;
	}
	// Check on the number of models that failed to load
	if (ntried == 0) return 0;
	else if (master.get_nmodels() == 0)
	{
		printf("Couldn't open any of the supplied files!\n");
		return -1;
	}
	return master.get_nmodels();
}

// Usage help
void master_data::print_usage()
{
	printf("Usage: aten [options] [<model> ...]\n");
	printf("\nProgram Options:\n");
	for (int n=0; n<CO_NITEMS; n++)
	{
		if (clioptions[n].argument == 0)
		{
			if (clioptions[n].shortopt != '\0') printf("\t-%c, --%s\n", clioptions[n].shortopt, clioptions[n].longopt);
			else printf("\t--%s\n", clioptions[n].longopt);
		}
		else
		{
			if (clioptions[n].shortopt != '\0') printf("\t-%c %s, --%s %s\n", clioptions[n].shortopt, clioptions[n].argtext, clioptions[n].longopt, clioptions[n].argtext);
			else printf("\t--%s %s\n", clioptions[n].longopt, clioptions[n].argtext);
		}
		printf("\t\t%s\n",clioptions[n].description);
	}
}
