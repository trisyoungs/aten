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
#include "base/prefs.h"
#include "base/master.h"
#include "base/elements.h"

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argtext,description)
Cli cliSwitches[] = {
	{ Cli::BohrSwitch,		'b',"bohr",		0,
		"",		"Converts model/grid atomic positions from Bohr to Angstrom" },
	{ Cli::BondSwitch,		'\0',"bond",		0,
		"",		"Force (re)calculation of bonding in the model" },
	{ Cli::CacheSwitch,		'\0',"cachelimit",	1,
		"<limit>",	"Set the trajectory cache limit to <limit> kb"},
	{ Cli::CentreSwitch,		'\0',"centre",		0,
		"",		"Force centering of atomic coordinates at zero" },
	{ Cli::CommandSwitch,		'c',"command",		1,
		"<commands>", "Execute supplied commands before main program execution" },
	{ Cli::ForcefieldSwitch,	'\0',"ff",		1,
		"<file>",	"Load the specified forcefield file" },
	{ Cli::FoldSwitch,		'\0',"fold",		0,
		"",		"Force folding of atoms in periodic systems" },
	{ Cli::FormatSwitch,		'f',"format",		0,
		"",		"Load models from command-line with specified <format>" },
	{ Cli::GridSwitch,		'g',"grid",		1,
		"<file>",	"Load the specified gridded data file" },
	{ Cli::HelpSwitch,		'h',"help",		0,
		"",		"Print this information" },
	{ Cli::InteractiveSwitch,	'i',"interactive",	0,
		"",		"Enter interactive mode" },
	{ Cli::MapSwitch,		'm',"map",		1,
		"<name=element,...>",	"Map file atomtypes to elements" },
	{ Cli::NoBondSwitch,		'\0',"nobond",		0,
		"",		"Prevent (re)calculation of bonding in the model" },
	{ Cli::NoCentreSwitch,		'\0',"nocentre",	0,
		"",		"Prevent centering of atomic coordinates at zero" },
	{ Cli::NoFoldSwitch,		'\0',"nofold",		0,
		"",		"Prevent folding of atoms in periodic systems" },
	{ Cli::NoPackSwitch,		'\0',"nopack",		0,
		"",		"Prevent generation of symmetry-equivalent atoms from spacegroup information" },
	{ Cli::PackSwitch,		'\0',"pack",		0,
		"",		"Force generation of symmetry-equivalent atoms from spacegroup information" },
	{ Cli::ScriptSwitch,		's',"script",		1,
		"<file",	"Load and execute the script file specified" },
	{ Cli::UndoLevelSwitch,		'u',"maxundo",		1,
		"<nlevels>",	"Set the maximum number of undo levels per model (-1 = unlimited)" },
	{ Cli::ZmapSwitch,		'z',"zmap",		1,
		"<mapstyle>",	"Override filter element mapping style" },
	{ Cli::DebugSwitch,		'd',"debug",		0,
		"",		"Print major subroutine call information" },
	{ Cli::DebugAllSwitch,		'\0',"debugall",	0,
		"",		"Print out all debug information" },
	{ Cli::DebugFileSwitch,		'\0',"debugfile",	0,
		"",		"Print out verbose information from file filter routines" },
	{ Cli::DebugMoreSwitch,		'\0',"debugmore",	0,
		"",		"Print all subroutine call information" },
	{ Cli::DebugParseSwitch,	'\0',"debugparse",	0,
		"",		"Print out verbose information from file parsing routines" },
	{ Cli::DebugTypingSwitch,	'\0',"debugtyping",	0,
		"",		"Print out verbose information from atom typing routines" },
	{ Cli::VerboseSwitch,		'v',"verbose",		0,
		"",		"Enable verbose program output" }
};

// Parse debug options
void Master::debugCli(int argc, char *argv[])
{
	int n, o;
	bool isShort, match;
	char *arg;
	// Cycle over program arguments and available CLI options
	n = 0;
	while (n < argc)
	{
		// If first character is not '-' then continue
		if (argv[n][0] != '-') { n++; continue; }
		// Is this a long or short option?
		isShort = (argv[n][1] != '-');
		arg = (isShort ? &argv[n][1] : &argv[n][2]);
		// Cycle over defined CLI options and search for this one
		match = FALSE;
		for (o=0; o<Cli::nSwitchItems; o++)
		{
			// Check short option character or long option text
			if (isShort) match = (*arg == cliSwitches[o].shortOpt);
			else match = (strcmp(arg,cliSwitches[o].longOpt) == 0 ? TRUE : FALSE);
			if (match) break;
		}
		// If we have a match then 'o' contains the option identifier
		// Only look for debug options here...
		if (match && (o >= Cli::DebugSwitch))
		{
			switch (o)
			{
				// Turn on call debugging
				case (Cli::DebugSwitch):
					Debug::addDebug(Debug::Calls);
					break;
				// Turn on debug messages for atom typing
				case (Cli::DebugTypingSwitch):
					Debug::addDebug(Debug::Typing);
					break;
				// Turn on debug messages for atom typing
				case (Cli::DebugParseSwitch):
					Debug::addDebug(Debug::Parse);
					break;
				// Turn on debug messages for atom typing
				case (Cli::DebugFileSwitch):
					Debug::addDebug(Debug::Filters);
					break;
				// Turn on debug messages for more calls
				case (Cli::DebugMoreSwitch):
					Debug::addDebug(Debug::Calls);
					Debug::addDebug(Debug::MoreCalls);
					break;
				// Turn on debug messages for all calls
				case (Cli::DebugAllSwitch):
					Debug::addDebug(Debug::Calls);
					Debug::addDebug(Debug::MoreCalls);
					Debug::addDebug(Debug::Verbose);
					Debug::addDebug(Debug::Parse);
					Debug::addDebug(Debug::Typing);
					break;
				// Turn on verbose messaging
				case (Cli::VerboseSwitch):
					Debug::addDebug(Debug::Verbose);
					break;
			}
		}
		// Next option...
		n++;
	}
}

// Parse all options
int Master::parseCli(int argc, char *argv[])
{
	int argn, opt, ntried = 0, n, el;
	bool isShort, match;
	char *arg;
	CommandList *cl;
	Prefs::ZmapType zm;
	Namemap<int> *nm;
	Filter *f, *modelfilter = NULL;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary run)
	argn = 1;
	while (argn < argc)
	{
		match = FALSE;
		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			arg = (isShort ? &argv[argn][1] : &argv[argn][2]);
			// Cycle over defined CLI options and search for this one
			for (opt=0; opt<Cli::nSwitchItems; opt++)
			{
				// Check short option character or long option text
				if (isShort) match = (*arg == cliSwitches[opt].shortOpt);
				else match = (strcmp(arg,cliSwitches[opt].longOpt) == 0 ? TRUE : FALSE);
				if (match) break;
			}
			// If we have a match then 'o' contains the option identifier. Otherwise try to load the argument as a model.
			if (match && (opt < Cli::DebugSwitch))
			{
				// If it's a debug option then we've already dealt with it
				switch (opt)
				{
					// Convert coordinates from Bohr to Angstrom
					case (Cli::BohrSwitch):
						prefs.setCoordsInBohr(TRUE);
						break;
					// Force bonding calculation of atoms on load
					case (Cli::BondSwitch):
						prefs.setBondOnLoad(Prefs::SwitchOn);
						break;
					// Set trajectory cache limit
					case (Cli::CacheSwitch):
						prefs.setCacheLimit(atoi(argv[++argn]));
						break;
					// Force model centering on load (for non-periodic systems)
					case (Cli::CentreSwitch):
						prefs.setCentreOnLoad(Prefs::SwitchOn);
						break;
					// Read script commands from passed string
					case (Cli::CommandSwitch):
						cl = master.commands.add();
						if (cl->cacheLine(argv[++argn])) master.setProgramMode(Master::CommandMode);
						else
						{
							master.commands.remove(cl);
							return -1;
						}
						break;
					// Load the specified forcefield
					case (Cli::ForcefieldSwitch):
						master.loadForcefield(argv[++argn]);
						break;
					// Force folding (MIM'ing) of atoms in periodic systems on load
					case (Cli::FoldSwitch):
						prefs.setFoldOnLoad(Prefs::SwitchOn);
						break;
					// Set forced model load format
					case (Cli::FormatSwitch):
						modelfilter = master.findFilter(FT_MODEL_IMPORT, argv[++argn]);
						if (modelfilter == NULL) return -1;
						break;
					// Load surface
					case (Cli::GridSwitch):
						argn++;
						f = master.probeFile(argv[argn], FT_GRID_IMPORT);
						if (f != NULL) f->execute(argv[argn]);
						break;
					// Display help
					case (Cli::HelpSwitch):
						printUsage();
						return -1;
						break;
					// Enter interactive mode
					case (Cli::InteractiveSwitch):
						master.setProgramMode(Master::InteractiveMode);
						break;
					// Set type mappings
					case (Cli::MapSwitch):
						// Get the argument and parse it internally
						parser.getArgsDelim(argv[++argn], Parser::Defaults);
						for (n=0; n<parser.nArgs(); n++)
						{
							el = elements.find(afterChar(parser.argc(n), '='));
							if (el == 0) msg(Debug::None,"Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
							else
							{
								nm = typeMap.add();
								nm->set(beforeChar(parser.argc(n),'='), el);
							}
						}
						break;
					// Prohibit bonding calculation of atoms on load
					case (Cli::NoBondSwitch):
						prefs.setBondOnLoad(Prefs::SwitchOff);
						break;
					// Prohibit model centering on load (for non-periodic systems)
					case (Cli::NoCentreSwitch):
						prefs.setCentreOnLoad(Prefs::SwitchOff);
						break;
					// Prohibit folding (MIM'ing) of atoms in periodic systems on load
					case (Cli::NoFoldSwitch):
						prefs.setFoldOnLoad(Prefs::SwitchOff);
						break;
					// Force packing (application of symmetry operators) on load
					case (Cli::NoPackSwitch):
						prefs.setPackOnLoad(Prefs::SwitchOff);
						break;
					// Prohibit packing (application of symmetry operators) on load
					case (Cli::PackSwitch):
						prefs.setPackOnLoad(Prefs::SwitchOn);
						break;
					// Cache a script file
					case (Cli::ScriptSwitch):
						cl = master.scripts.add();
						if (cl->load(argv[++argn])) master.setProgramMode(Master::CommandMode);
						else
						{
							master.scripts.remove(cl);
							return -1;
						}
						break;
					// Set maximum number of undolevels per model
					case (Cli::UndoLevelSwitch):
						prefs.setMaxUndoLevels(atoi(argv[++argn]));
						break;
					// Set the type of element (Z) mapping to use in name conversion
					case (Cli::ZmapSwitch):
						zm = Prefs::zmapType(argv[++argn]);
						if (zm != Prefs::nZmapTypes) prefs.setZmapType(zm);
						break;
					default:
						printf("Unrecognised command-line option '%s'.\n",argv[argn]);
						return -1;
				}
			}
		}
		else
		{
			// Not a CLI switch, so try to load it as a model
			ntried ++;
			if (modelfilter != NULL) f = modelfilter;
			else f = master.probeFile(argv[argn], FT_MODEL_IMPORT);
			if (f != NULL) f->execute(argv[argn]);
		}
		argn++;
	}
	// Check on the number of models that failed to load
	if (ntried == 0) return 0;
	else if (master.nModels() == 0)
	{
		printf("Couldn't open any of the supplied files!\n");
		return -1;
	}
	return master.nModels();
}

// Usage help
void Master::printUsage() const
{
	printf("Usage: aten [options] [<model> ...]\n");
	printf("\nProgram Options:\n");
	for (int n=0; n<Cli::nSwitchItems; n++)
	{
		if (cliSwitches[n].argument == 0)
		{
			if (cliSwitches[n].shortOpt != '\0') printf("\t-%c, --%s\n", cliSwitches[n].shortOpt, cliSwitches[n].longOpt);
			else printf("\t--%s\n", cliSwitches[n].longOpt);
		}
		else
		{
			if (cliSwitches[n].shortOpt != '\0') printf("\t-%c %s, --%s %s\n", cliSwitches[n].shortOpt, cliSwitches[n].argText, cliSwitches[n].longOpt, cliSwitches[n].argText);
			else printf("\t--%s %s\n", cliSwitches[n].longOpt, cliSwitches[n].argText);
		}
		printf("\t\t%s\n",cliSwitches[n].description);
	}
}
