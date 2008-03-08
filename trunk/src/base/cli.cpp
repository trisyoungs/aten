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
	{ CO_BOHR,	"bohr",		0,"",		"Converts model atomic positions from Bohr to Angstrom" },
	{ CO_COMMAND,	"command",	1,"<commands>", "Execute supplied commands before main program execution" },
	{ CO_DEBUG,	"debug",	0,"",		"Print major subroutine call information" },
	{ CO_FF,	"ff",		1,"<file>",	"Load the specified forcefield file" },
	{ CO_GRID,	"grid",		1,"<file>",	"Load the specified gridded data file" },
	{ CO_HELP,	"help",		0,"",		"Print this information" },
	{ CO_INTERACTIVE,"interactive",	0,"",		"Enter interactive mode" },
	{ CO_MAP,	"map",		1,"<name=element,...>",	"Map file atomtypes to elements" },
	{ CO_SCRIPT,	"script",	1,"<file",	"Load and execute the script file specified" },
	{ CO_UNDO,	"maxundo",	1,"<nlevels>",	"Set the maximum number of undo levels per model (-1 = unlimited)" },
	{ CO_VERBOSE,	"verbose",	0,"",		"Enable verbose program output" },
	{ CO_ZMAP,	"zmap",		1,"<mapstyle>",	"Override filter element mapping style" },
	{ CO_CACHE,	"cachelimit",	1,"<limit>",	"Set the trajectory cache limit to <limit> kb"},
	{ CO_FOLD,	"fold",		0,"",		"Force folding of atoms in periodic systems" },
	{ CO_NOFOLD,	"nofold",	0,"",		"Prevent folding of atoms in periodic systems" },
	{ CO_BOND,	"bond",		0,"",		"Force (re)calculation of bonding in the model" },
	{ CO_NOBOND,	"nobond",	0,"",		"Prevent (re)calculation of bonding in the model" },
	{ CO_CENTRE,	"centre",	0,"",		"Force centering of atomic coordinates at zero" },
	{ CO_NOCENTRE,	"nocentre",	0,"",		"Prevent centering of atomic coordinates at zero" },
	{ CO_NOPACK,	"nopack",	0,"",		"Prevent generation of symmetry-equivalent atoms from spacegroup information" },
	{ CO_DEBUGTYPING,"debugtyping",	0,"",		"Print out verbose information from atom typing routines" },
	{ CO_DEBUGMORE,	"debugmore",	0,"",		"Print all subroutine call information" },
	{ CO_DEBUGALL,	"debugall",	0,"",		"Print out all debug information" },
	{ CO_DEBUGPARSE,"debugparse",	0,"",		"Print out verbose information from file parsing routines" },
	{ CO_DEBUGFILE,	"debugfile",	0,"",		"Print out verbose information from file filter routines" }
};

// Prepare options list
void master_data::prepare_cli()
{
	// Initialise short options string and build list of command-line options
	shortopts.create_empty(256);
	// Loop over elements defined in optiondata
	nopts = sizeof(clioptions) / sizeof(clioptions[0]);
	longopts = new option[nopts+1];
	for (int n=0; n<nopts; n++)
	{
		// Add long option
		longopts[n].name = clioptions[n].get_keyword();
		longopts[n].has_arg = clioptions[n].get_argument();
		longopts[n].flag = NULL;
		longopts[n].val = clioptions[n].get_id();
		// If this long option has a corresponding short option (anything <= CO_ZMAP), add it to the shortopts list
		if (longopts[n].val <= CO_ZMAP)
		{
			shortopts += char(longopts[n].val+97);
			if (longopts[n].has_arg == required_argument) shortopts += ':';
			else if (longopts[n].has_arg == optional_argument) shortopts.cat("::");
		}
	}
	// Add terminating long option
	longopts[nopts].name = 0;
	longopts[nopts].has_arg = 0;
	longopts[nopts].flag = NULL;
	longopts[nopts].val = 0;
}

// Parse all options
int master_data::parse_cli(int argc, char *argv[])
{
	// Parse program options using getopt_long.
	int index = 1, ntried = 0;
	bool done = FALSE;
	commandlist *cl;
	filter *f;
	zmap_type zm;
	//printf("PROPER_PARSE = %i [%s]\n",argc,shortopts.get());
	while (!done)
	{
		// Parse option from cli arguments
		int result = getopt_long(argc,argv,shortopts.get(),longopts,&index);
		// Convert short option result (> CO_NITEMS) to long option equivalent
		if (result > CO_NITEMS) result -= 97;
		//printf("CLI_PARSE result = %i\n",result);
		if (result == -1) done = TRUE;
		else
		{
			switch (result)
			{
				/*
				// Short options with long equivalents
				*/
				case (CO_A):
					break;
				// Convert coordinates from Bohr to Angstrom
				case (CO_BOHR):
					prefs.set_coords_in_bohr(TRUE);
					break;
				// Read script commands from passed string
				case (CO_COMMAND):
					cl = master.commands.add();
					if (cl->cache_line(optarg)) master.set_program_mode(PM_COMMAND);
					else
					{
						master.commands.remove(cl);
						return -1;
					}
					break;
				// Turn on call debugging
				case (CO_DEBUG):
					add_debuglevel(DM_CALLS);
					break;
				case (CO_E):
					break;
				// Load the specified forcefield
				case (CO_FF):
					master.load_ff(optarg);
					break;
				// Load surface
				case (CO_GRID):
					f = master.probe_file(optarg, FT_GRID_IMPORT);
					if (f != NULL) f->execute(optarg);
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
				case (CO_J):
				case (CO_K):
				case (CO_L):
				case (CO_MAP):
				case (CO_N):
				case (CO_O):
				case (CO_P):
				case (CO_Q):
				case (CO_R):
					break;
				// Cache a script file
				case (CO_SCRIPT):
					cl = master.scripts.add();
					if (cl->load(optarg)) master.set_program_mode(PM_COMMAND);
					else
					{
						master.scripts.remove(cl);
						return -1;
					}
					break;
				case (CO_T):
					break;
				// Set maximum number of undolevels per model
				case (CO_UNDO):
					prefs.set_maxundo(atoi(optarg));
					break;
				// Turn on verbose messaging
				case (CO_VERBOSE):
					add_debuglevel(DM_VERBOSE);
					break;
				case (CO_W):
				case (CO_X):
				case (CO_Y):
					break;
				// Set the type of element (Z) mapping to use in name conversion
				case (CO_ZMAP):
					zm = ZM_from_text(optarg);
					if (zm != ZM_NITEMS) prefs.set_zmapping(zm);
					break;
				/*
				// Long options
				*/
				// Set trajectory cache limit
				case (CO_CACHE):
					prefs.set_cache_limit(atoi(optarg));
					break;
				// Force folding (MIM'ing) of atoms in periodic systems on load
				case (CO_FOLD):
					prefs.set_fold_on_load(PS_YES);
					break;
				// Prohibit folding (MIM'ing) of atoms in periodic systems on load
				case (CO_NOFOLD):
					prefs.set_fold_on_load(PS_NO);
					break;
				// Force bonding calculation of atoms on load
				case (CO_BOND):
					prefs.set_bond_on_load(PS_YES);
					break;
				// Prohibit bonding calculation of atoms on load
				case (CO_NOBOND):
					prefs.set_bond_on_load(PS_NO);
					break;
				// Force model centering on load (for non-periodic systems)
				case (CO_CENTRE):
					prefs.set_centre_on_load(PS_YES);
					break;
				// Prohibit model centering on load (for non-periodic systems)
				case (CO_NOCENTRE):
					prefs.set_centre_on_load(PS_NO);
					break;
				// Prohibit packing (application of symmetry operators) on load
				case (CO_PACK):
					prefs.set_pack_on_load(PS_YES);
					break;
				// Force packing (application of symmetry operators) on load
				case (CO_NOPACK):
					prefs.set_pack_on_load(PS_NO);
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
				default:
					printf("Unrecognised command-line option '%s'.\n",argv[index]);
					dbg_end(DM_CALLS,"cli::parse");
					return -1;
			}
		}
	}
	// Now all that remains in the argv array (argv[n] where n >= index) are molecule files to load
	while (optind < argc)
	{
		ntried ++;
		f = master.probe_file(argv[optind], FT_MODEL_IMPORT);
		if (f != NULL) f->execute(argv[optind]);
		optind++;
	}
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
	int id;
	for (int n=0; n<nopts; n++)
	{
		id = clioptions[n].get_id();
		if (clioptions[n].get_argument() == 0)
		{
			if (id <= CO_ZMAP) printf("\t-%c, --%s\n", char(id+97), clioptions[n].get_keyword());
			else printf("\t--%s\n", clioptions[n].get_keyword());
		}
		else
		{
			if (id <= CO_ZMAP) printf("\t-%c %s, --%s %s\n", char(id+97), clioptions[n].get_argtext(), clioptions[n].get_keyword(), clioptions[n].get_argtext());
			else printf("\t--%s %s\n", clioptions[n].get_keyword(), clioptions[n].get_argtext());
		}
		printf("\t\t%s\n",clioptions[n].get_description());
	}
}
