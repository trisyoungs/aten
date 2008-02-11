/*
	*** Command-line option parsing
	*** src/base/cli.cpp
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

#include <iostream>
#include "base/master.h"
#include <getopt.h>

// Prepare options list
void master_data::prepare_cli()
{
	// Blank all related variables
	for (int n=0; n<CO_NITEMS; n++)
	{
		CO_keyword[n][0] = '\0';
		CO_desc[n][0] = '\0';
		CO_argument[n][0] = '\0';
	}
	// Initialise short options string and build list of command-line options
	shortopts.create_empty(256);
	add_cli_option("bohr",CO_BOHR,"Converts model atomic positions from Bohr to Angstrom");
	add_cli_option("command",CO_COMMAND,"Execute supplied commands before main program execution",required_argument,"<commands>");
	add_cli_option("debug",CO_DEBUG,"Print major subroutine call information");
	add_cli_option("ff",CO_FF,"Load the specified forcefield file",required_argument,"<file>");
	add_cli_option("help",CO_HELP,"Print this information");
	add_cli_option("script",CO_SCRIPT,"Load and execute the script file specified",required_argument,"<file>");
	add_cli_option("maxundo",CO_UNDO,"Set the maximum number of undo levels per model (-1 = unlimited)",required_argument,"<nlevels>");
	add_cli_option("verbose",CO_VERBOSE,"Enable verbose program output");
	add_cli_option("zmap",CO_ZMAP,"Override filter element mapping style",required_argument,"<mapstyle>");
	add_cli_option("cachelimit",CO_CACHE,"Set the trajectory cache limit to <limit> kb",required_argument,"<limit>");
	add_cli_option("fold",CO_FOLD,"Force folding of atoms in periodic systems");
	add_cli_option("nofold",CO_NOFOLD,"Prevent folding of atoms in periodic systems");
	add_cli_option("bond",CO_BOND,"Force (re)calculation of bonding in the model");
	add_cli_option("nobond",CO_NOBOND,"Prevent (re)calculation of bonding in the model");
	add_cli_option("centre",CO_CENTRE,"Force centering of atomic coordinates at zero");
	add_cli_option("nocentre",CO_NOCENTRE,"Prevent centering of atomic coordinates at zero");
	add_cli_option("nopack",CO_NOPACK,"Prevent generation of symmetry-equivalent atoms from spacegroup information");
	add_cli_option("debugtyping",CO_DEBUGTYPING,"Print out verbose information from atom typing routines");
	add_cli_option("debugmore",CO_DEBUGMORE,"Print all subroutine call information");
	add_cli_option("debugall",CO_DEBUGALL,"Print out all debug information");
	add_cli_option("debugparse",CO_DEBUGPARSE,"Print out verbose information from file parsing routines");
	add_cli_option("debugfilters",CO_DEBUGFILTERS,"Print out verbose information from file filter routines");
	add_cli_option("grid",CO_GRID,"Load the specified grided data file",required_argument,"<file>");
	add_cli_option("",CO_LISTENDER,"");
}

// Add CLI options
void master_data::add_cli_option(const char *name, int enumid, const char *argname, int argtype, const char *description)
{
	// Add a new option entry in the opts array
	static int nopts = 0;
	clioptions[nopts].name = name;
	clioptions[nopts].has_arg = argtype;
	clioptions[nopts].flag = NULL;
	clioptions[nopts].val = enumid;
	// Store description data
	//strcpy(CO_keyword[enumid],name);
	//strcpy(CO_desc[enumid],description);
	//strcpy(CO_argument[enumid],argname);
	nopts ++;
	// If this long option has a corresponding short option (anything <= CO_ZMAP), add it to the shortopts list
	if ((enumid > CO_LISTENDER) && (enumid <= CO_ZMAP))
	{
		shortopts += char(enumid+96);
		if (argtype == required_argument) shortopts += ':';
		else if (argtype == optional_argument) shortopts.cat("::");
	}
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
	printf("LKdjflkjfkdlsj\n");
	printf("PROPER_PARSE = %i [%s]\n",argc,shortopts.get());
	while (!done)
	{
		// Parse option from cli arguments
		int result = getopt_long(argc,argv,shortopts.get(),clioptions,&index);
	printf("CLI_PARSE result = %i\n",result);
		if (result == -1) done = TRUE;
		else
		{
			switch (result)
			{
				/*
				// Short options with long equivalents
				*/
				// Turn on call debugging
				case (CO_DEBUG):
					add_debuglevel(DM_CALLS);
					break;
				// Turn on verbose messaging
				case (CO_VERBOSE):
					add_debuglevel(DM_VERBOSE);
					break;
				// Load the specified forcefield
				case (CO_FF):
					master.load_ff(optarg);
					break;
				// Read script commands from passed string
				case (CO_COMMAND):
					cl = master.scripts.add();
					if (cl->cache_line(optarg)) master.set_program_mode(PM_COMMAND);
					else
					{
						master.scripts.remove(cl);
						return -1;
					}
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
				// Set the type of element (Z) mapping to use in name conversion
				case (CO_ZMAP):
					zm = ZM_from_text(optarg);
					if (zm != ZM_NITEMS) prefs.set_zmapping(zm);
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
				// Convert coordinates from Bohr to Angstrom
				case (CO_BOHR):
					prefs.set_coords_in_bohr(TRUE);
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
				case (CO_DEBUGFILTERS):
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
				// Load surface
				case (CO_GRID):
					f = master.probe_file(optarg, FT_GRID_IMPORT);
					if (f != NULL) f->execute(optarg);
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
	for (int n=CO_A; n<CO_NITEMS; n++)
	{
		// Check to see if the option is blank (which may be the case for options < C_ZMAP)
		if (CO_keyword[n][0] == '\0') continue;
		// Print information....
		if (n <= CO_ZMAP) printf("  -%c %s, --%s %s\n", char(n+96), CO_argument[n], CO_keyword[n], CO_argument[n]);
		else printf("  --%s %s\n", CO_keyword[n], CO_argument[n]);
		printf("\t\t%s\n",CO_desc[n]);
	}
}
