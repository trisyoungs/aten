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

enum short_opt { SO_A=97, SO_BOHR, SO_COMMAND, SO_DEBUG, SO_E,
		SO_FF, SO_G, SO_HELP, SO_INTERACTIVE, SO_J,
		SO_K, SO_L, SO_M, SO_N, SO_O,
		SO_P, SO_Q, SO_R, SO_SCRIPT, SO_T,
		SO_U, SO_VERBOSE, SO_W, SO_X, SO_Y, SO_ZMAP,
		SO_LASTITEM };
enum long_opt { LO_FOLD, LO_NOFOLD, LO_BOND, LO_NOBOND, LO_CENTRE, LO_NOCENTRE, LO_PACK, LO_NOPACK, LO_DEBUGTYPING, LO_DEBUGPARSE, LO_DEBUGMORE, LO_DEBUGALL, LO_DEBUGFILTERS, LO_SURFACE, LO_NITEMS };

// Prepare options list
void master_data::prepare_cli()
{
	// Create the option structures for getopt_long to use
	nopts = 0;
	shortopts.create_empty(256);
	// Long/short options
	add_cli_option("bohr",no_argument,SO_BOHR,TRUE);
	add_cli_option("command",required_argument,SO_COMMAND,TRUE);
	add_cli_option("debug",no_argument,SO_DEBUG,TRUE);
	add_cli_option("ff",required_argument,SO_FF,TRUE);
	add_cli_option("script",required_argument,SO_SCRIPT,TRUE);
	add_cli_option("verbose",no_argument,SO_VERBOSE,TRUE);
	add_cli_option("zmap",required_argument,SO_ZMAP,TRUE);
	add_cli_option("help",no_argument,SO_HELP,TRUE);
	// Long options
	add_cli_option("fold",no_argument,LO_FOLD,FALSE);
	add_cli_option("nofold",no_argument,LO_NOFOLD,FALSE);
	add_cli_option("bond",no_argument,LO_BOND,FALSE);
	add_cli_option("nobond",no_argument,LO_NOBOND,FALSE);
	add_cli_option("centre",no_argument,LO_CENTRE,FALSE);
	add_cli_option("nocentre",no_argument,LO_NOCENTRE,FALSE);
	add_cli_option("nopack",no_argument,LO_NOPACK,FALSE);
	add_cli_option("debugtyping",no_argument,LO_DEBUGTYPING,FALSE);
	add_cli_option("debugmore",no_argument,LO_DEBUGMORE,FALSE);
	add_cli_option("debugall",no_argument,LO_DEBUGALL,FALSE);
	add_cli_option("debugparse",no_argument,LO_DEBUGPARSE,FALSE);
	add_cli_option("debugfilters",no_argument,LO_DEBUGFILTERS,FALSE);
	add_cli_option("surface",required_argument,LO_SURFACE,FALSE);
	add_cli_option("0",0,0,0);
}

// Add CLI options
void master_data::add_cli_option(const char *name, int has_arg, int enumid, bool has_shortopt)
{
	// Add a new option entry in the opts array
	if (nopts == MAXCLIOPTS)
	{
		printf("Increase MAXOPTS in cli.h!\n");
		return;
	}
	longopts[nopts].name = name;
	longopts[nopts].has_arg = has_arg;
	longopts[nopts].flag = NULL;
	longopts[nopts].val = enumid;
	nopts ++;
	// If this long option has a corresponding short option, add it to the shortopts list
	if (has_shortopt)
	{
		shortopts += char(enumid);
		if (has_arg == required_argument) shortopts += ':';
		else if (has_arg == optional_argument) shortopts.cat("::");
	}
}

// Parse all options
int master_data::parse_cli(int argc, char *argv[])
{
	// Parse program options using getopt_long.
	int index = 1, ntried = 0;
	bool done = FALSE;
	model *m;
	script *s;
	filter *f;
	zmap_type zm;
	//printf("PROPER_PARSE = %i [%s]\n",argc,shortopts.c_str());
	while (!done)
	{
		// Parse option from cli arguments
		int result = getopt_long(argc,argv,shortopts.get(),longopts,&index);
	//printf("CLI_PARSE result = %i\n",result);
		if (result == -1) done = TRUE;
		else
		{
			switch (result)
			{
				/*
				// Short options with long equivalents
				*/
				// Turn on call debugging
				case (SO_DEBUG):
					add_debuglevel(DM_CALLS);
					break;
				// Turn on verbose messaging
				case (SO_VERBOSE):
					add_debuglevel(DM_VERBOSE);
					break;
				// Load the specified forcefield
				case (SO_FF):
					master.load_ff(optarg);
					break;
				// Read script commands from passed string
				case (SO_COMMAND):
					s = master.scripts.add();
					if (s->cache_line(optarg)) master.set_program_mode(PM_SCRIPT);
					else
					{
						master.scripts.remove(s);
						return -1;
					}
					break;
				// Cache a script file
				case (SO_SCRIPT):
					s = master.scripts.add();
					if (s->load(optarg)) master.set_program_mode(PM_SCRIPT);
					else
					{
						master.scripts.remove(s);
						return -1;
					}
					break;
				// Set the type of element (Z) mapping to use in name conversion
				case (SO_ZMAP):
					zm = ZM_from_text(optarg);
					if (zm != ZM_NITEMS) prefs.set_zmapping(zm);
					break;
				// Display help
				case (SO_HELP):
					print_usage();
					return -1;
					break;
				// Enter interactive mode
				case (SO_INTERACTIVE):
					master.set_program_mode(PM_INTERACTIVE);
					break;
				// Convert coordinates from Bohr to Angstrom
				case (SO_BOHR):
					prefs.set_coords_in_bohr(TRUE);
					break;
				/*
				// Long options
				*/
				// Force folding (MIM'ing) of atoms in periodic systems on load
				case (LO_FOLD):
					prefs.set_fold_on_load(PS_YES);
					break;
				// Prohibit folding (MIM'ing) of atoms in periodic systems on load
				case (LO_NOFOLD):
					prefs.set_fold_on_load(PS_NO);
					break;
				// Force bonding calculation of atoms on load
				case (LO_BOND):
					prefs.set_bond_on_load(PS_YES);
					break;
				// Prohibit bonding calculation of atoms on load
				case (LO_NOBOND):
					prefs.set_bond_on_load(PS_NO);
					break;
				// Force model centering on load (for non-periodic systems)
				case (LO_CENTRE):
					prefs.set_centre_on_load(PS_YES);
					break;
				// Prohibit model centering on load (for non-periodic systems)
				case (LO_NOCENTRE):
					prefs.set_centre_on_load(PS_NO);
					break;
				// Prohibit packing (application of symmetry operators) on load
				case (LO_PACK):
					prefs.set_pack_on_load(PS_YES);
					break;
				// Force packing (application of symmetry operators) on load
				case (LO_NOPACK):
					prefs.set_pack_on_load(PS_NO);
					break;
				// Turn on debug messages for atom typing
				case (LO_DEBUGTYPING):
					add_debuglevel(DM_TYPING);
					break;
				// Turn on debug messages for atom typing
				case (LO_DEBUGPARSE):
					add_debuglevel(DM_PARSE);
					break;
				// Turn on debug messages for atom typing
				case (LO_DEBUGFILTERS):
					add_debuglevel(DM_FILTERS);
					break;
				// Turn on debug messages for more calls
				case (LO_DEBUGMORE):
					add_debuglevel(DM_CALLS);
					add_debuglevel(DM_MORECALLS);
					break;
				// Turn on debug messages for all calls
				case (LO_DEBUGALL):
					add_debuglevel(DM_CALLS);
					add_debuglevel(DM_MORECALLS);
					add_debuglevel(DM_VERBOSE);
					add_debuglevel(DM_PARSE);
					add_debuglevel(DM_TYPING);
					break;
				// Load surface
				case (LO_SURFACE):
					f = master.probe_file(optarg, FT_GRID_IMPORT);
					if (f != NULL) f->import_grid(optarg);
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
		if (f != NULL) m = f->import_model(argv[optind]);
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
	printf("Usage: aten [options] [<model> [<model> [...]]\n");
	printf("\nProgram Options:\n");
	printf("-h, --help\n   Displays this information\n");
	printf("-s <file>\n   Loads and executes the script file specified\n");
	printf("-i, --interactive\n   Enters interactive mode\n");
	printf("-f, --ff <file>\n   Loads the specified forcefield file\n");
	printf("\nModel Load Options:\n");
	printf("-z, --zmap <alpha|numeric|ff|first>\n   Sets conversion method of atom names\n");
	printf("--[no]fold\n   Force or prevent folding of atoms in periodic systems\n");
	printf("--nopack\n   Prevent generation of symmetry-equivalent atoms from spacegroup information\n");
	printf("--[no]bond\n   Force or prevent (re)calculation of bonding in the model\n");
	printf("--nocentre\n   Don't centre non-periodic models.\n");
	printf("-b, --bohr\n   Converts model atomic positions from Bohr to Angstrom\n");
	printf("\nDebug Options:\n");
	printf("-d, --debug\n   Print major subroutine call information\n");
	printf("--debugmore\n   Print all subroutine call information\n");
	printf("--debugtyping\n   Print out verbose information from atom typing routines\n");
	printf("--debugparse\n   Print out verbose information from file parsing routines\n");
	printf("--debugfilters\n   Print out verbose information from file filter routines\n");
	printf("--debugall\n   Print out all debug information\n");
	printf("--verbose\n   Enable verbose program output\n");
}
