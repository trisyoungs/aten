/*
	*** Command-line option parsing
	*** src/aten/cli.cpp
	Copyright T. Youngs 2007-2009

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


#include "main/cli.h"
#include "main/aten.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argtext,description)
Cli cliSwitches[] = {
	{ Cli::BatchSwitch,		'\0',"batch",		0,
		"",		"Run any commands supplied with -c or --command on all models and save" },
	{ Cli::BohrSwitch,		'b',"bohr",		0,
		"",		"Converts model/grid atomic positions from Bohr to Angstrom" },
	{ Cli::BondSwitch,		'\0',"bond",		0,
		"",		"Force (re)calculation of bonding in the model" },
	{ Cli::CacheSwitch,		'\0',"cachelimit",	1,
		"<limit>",	"Set the trajectory cache limit to <limit> kb"},
	{ Cli::CentreSwitch,		'\0',"centre",		0,
		"",		"Force centering of atomic coordinates at zero" },
	{ Cli::CommandSwitch,		'c',"command",		1,
		"<commands>",	"Execute supplied commands before main program execution" },
	{ Cli::DebugSwitch,		'd',"debug",		2,
		"[output]",	"Print out call debug information, or specific information if output type is supplied" },
	{ Cli::ExportSwitch,		'e',"export",		1,
		"<nickname>",	"Export all loaded models in the nicknamed format" },
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
	{ Cli::KeepNamesSwitch,		'\0',"keepnames",	0,
		"",		"Store atom (type)names given in files in a forcefield created for the model" },
	{ Cli::KeepViewSwitch,		'k',"keepview",		0,
		"",		"Keep (don't reset) view when GUI starts" },
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
	{ Cli::QuietSwitch,		'q',"quiet",		0,
		"",		"Run silently, reporting only errors that stop the program" },
	{ Cli::ScriptSwitch,		's',"script",		1,
		"<file>",	"Load and execute the script file specified" },
	{ Cli::TrajectorySwitch,	't',"trajectory",	1,
		"<file>",	"Associate a trajectory with the last loaded model" },
	{ Cli::UndoLevelSwitch,		'u',"undolevels",	1,
		"<nlevels>",	"Set the maximum number of undo levels per model (-1 = unlimited)" },
	{ Cli::VerboseSwitch,		'v',"verbose",		0,
		"",		"Enable verbose program output" },
	{ Cli::VersionSwitch,		'\0',"version",		0,
		"",		"Print program version and exit" },
	{ Cli::ZmapSwitch,		'z',"zmap",		1,
		"<mapstyle>",	"Override filter element mapping style" }
};

// Parse CLI options *before* filters / prefs have been loaded
bool Aten::parseCliEarly(int argc, char *argv[])
{
	int argn, opt;
	bool isShort, match, nextArgIsSwitch, hasNextArg;
	Messenger::OutputType ot;
	char *arg;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		argn++;
		// Check for null argument
		if (argv[argn][0] == '\0') continue;
		match = FALSE;
		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			arg = (isShort ? &argv[argn][1] : &argv[argn][2]);
			// Check what the next CLI argument is
			if (argn == (argc-1)) hasNextArg = FALSE;
			else
			{
				hasNextArg = TRUE;
				nextArgIsSwitch = (argv[argn+1][0] == '-');
			}
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn],"-psn",4) == 0)
			{
				printf("Found (and ignored) OSX-added '%s'.\n",argv[argn]);
				continue;
			} 
			// Cycle over defined CLI options and search for this one
			for (opt=0; opt<Cli::nSwitchItems; opt++)
			{
				// Check short option character or long option text
				if (isShort) match = (*arg == cliSwitches[opt].shortOpt);
				else match = (strcmp(arg,cliSwitches[opt].longOpt) == 0 ? TRUE : FALSE);
				if (match) break;
			}
			// Check to see if we matched any of the known CLI switches
			if (!match)
			{
				printf("Unrecognised command-line option '%s'.\n",argv[argn]);
				return FALSE;
			}
			// If this option needs an argument, check that we have one
			switch (cliSwitches[opt].argument)
			{
				// No argument required
				case (0):
					break;
				// Required argument
				case (1):
					if ((!hasNextArg) || nextArgIsSwitch)
					{
						if (isShort) msg.print(" '-%c' requires an argument.\n", cliSwitches[opt].shortOpt);
						else msg.print(" '--%s' requires an argument.\n", cliSwitches[opt].longOpt);
						return FALSE;
					}
					break;
				// Optional argument
				case (2):
					break;
			}
			// Ready to perform switch action!
			// We recognise only a specific selection of switches here, mostly to do with debugging / versioning etc.
			switch (opt)
			{
				// Turn on debug messages for calls (or specified output)
				case (Cli::DebugSwitch):
					if ((!hasNextArg) || nextArgIsSwitch) msg.addOutputType(Messenger::Calls);
					else
					{
						ot = Messenger::outputType(argv[++argn]);
						if (ot != Messenger::nOutputTypes) msg.addOutputType(ot);
						else return FALSE;
						argv[argn][0] = '\0';
					}
					break;
				// Display help
				case (Cli::HelpSwitch):
					printUsage();
					return FALSE;
					break;
				// Run in silent mode (no CLI output)
				case (Cli::QuietSwitch):
					msg.setQuiet(TRUE);
					break;
				// Turn on verbose messaging
				case (Cli::VerboseSwitch):
					msg.addOutputType(Messenger::Verbose);
					break;
				// Print version and exit
				case (Cli::VersionSwitch):
					printf("Aten version %s, built from %s@%s.\n", ATENVERSION, ATENURL, ATENREVISION);
					return FALSE;
					break;
			}
		}
	}
	return TRUE;
}

// Parse CLI options, after filters / prefs have been loaded
int Aten::parseCli(int argc, char *argv[])
{
	int argn, opt, ntried = 0, n, el;
	bool isShort, match, nextArgIsSwitch, hasNextArg;
	char *arg, *line, prompt[32];
	Forcefield *ff;
	ElementMap::ZmapType zm;
	Namemap<int> *nm;
	CommandList cl, *script;
	Filter *f, *modelfilter = NULL;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		argn++;
		// Check for null argument
		if (argv[argn][0] == '\0') continue;
		match = FALSE;
		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			arg = (isShort ? &argv[argn][1] : &argv[argn][2]);
			// Check what the next CLI argument is
			if (argn == (argc-1)) hasNextArg = FALSE;
			else
			{
				hasNextArg = TRUE;
				nextArgIsSwitch = (argv[argn+1][0] == '-');
			}
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn],"-psn",4) == 0)
			{
				printf("Found (and ignored) OSX-added '%s'.\n",argv[argn]);
				continue;
			} 
			// Cycle over defined CLI options and search for this one
			for (opt=0; opt<Cli::nSwitchItems; opt++)
			{
				// Check short option character or long option text
				if (isShort) match = (*arg == cliSwitches[opt].shortOpt);
				else match = (strcmp(arg,cliSwitches[opt].longOpt) == 0 ? TRUE : FALSE);
				if (match) break;
			}
			// Check to see if we matched any of the known CLI switches
			if (!match)
			{
				printf("Unrecognised command-line option '%s'.\n",argv[argn]);
				return -1;
			}
			// If this option needs an argument, check that we have one
			switch (cliSwitches[opt].argument)
			{
				// No argument required
				case (0):
					break;
				// Required argument
				case (1):
					if ((!hasNextArg) || nextArgIsSwitch)
					{
						if (isShort) msg.print(" '-%c' requires an argument.\n", cliSwitches[opt].shortOpt);
						else msg.print(" '--%s' requires an argument.\n", cliSwitches[opt].longOpt);
						return -1;
					}
					break;
				// Optional argument
				case (2):
					break;
			}
			// Ready to perform switch action!
			switch (opt)
			{
				// All of the following switches were dealt with in parseCliEarly()
				case (Cli::DebugSwitch):
				case (Cli::HelpSwitch):
				case (Cli::QuietSwitch):
				case (Cli::VerboseSwitch):
				case (Cli::VersionSwitch):
					break;
				// Enable batch processing mode
				case (Cli::BatchSwitch):
					if (aten.programMode() == Aten::BatchExportMode) aten.setProgramMode(Aten::ProcessAndExportMode);
					else aten.setProgramMode(Aten::BatchProcessMode);
					break;
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
				// Read commands from passed string and execute them
				case (Cli::CommandSwitch):
					if ((aten.programMode() == Aten::BatchProcessMode) || (aten.programMode() == Aten::ProcessAndExportMode))
					{
						script = aten.addBatchCommand();
						if (!script->cacheLine(argv[++argn])) return -1;
					}
					else
					{
						cl.clear();
						if (cl.cacheLine(argv[++argn]))
						{
							if (!cl.execute()) return -1;
						}
						else return -1;
					}
					break;
				// Export all models in nicknamed format (single-shot mode)
				case (Cli::ExportSwitch):
					f = aten.findFilter(Filter::ModelExport, argv[++argn]);
					if (f == NULL) return -1;
					aten.setExportFilter(f);
					if (aten.programMode() == Aten::BatchProcessMode) aten.setProgramMode(Aten::ProcessAndExportMode);
					else aten.setProgramMode(Aten::BatchExportMode);
					break;
				// Force folding (MIM'ing) of atoms in periodic systems on load
				case (Cli::FoldSwitch):
					prefs.setFoldOnLoad(Prefs::SwitchOn);
					break;
				// Load the specified forcefield
				case (Cli::ForcefieldSwitch):
					ff = aten.loadForcefield(argv[++argn]);
					if (ff == NULL) return -1;
					break;
				// Set forced model load format
				case (Cli::FormatSwitch):
					modelfilter = aten.findFilter(Filter::ModelImport, argv[++argn]);
					if (modelfilter == NULL) return -1;
					break;
				// Load surface
				case (Cli::GridSwitch):
					argn++;
					f = aten.probeFile(argv[argn], Filter::GridImport);
					if (f == NULL) return -1;
					else if (!f->execute(argv[argn])) return -1;
					break;
				// Enter interactive mode
				case (Cli::InteractiveSwitch):
					sprintf(prompt,"Aten %s > ",ATENVERSION);
					printf("Entering interactive mode...\n");
					aten.setProgramMode(Aten::InteractiveMode);
					do
					{
						// Get string from user
						line = readline(prompt);
						aten.interactiveScript.clear();
						if (aten.interactiveScript.cacheLine(line)) aten.interactiveScript.execute();
						// Add the command to the history and delete it 
						add_history(line);
						delete line;
					} while (aten.programMode() == Aten::InteractiveMode);
					//aten.set_program_mode(PM_NONE);
					break;
				// Keep atom names in file
				case (Cli::KeepNamesSwitch):
					prefs.setKeepNames(TRUE);
					break;
				// Keep (don't reset) view when GUI starts
				case (Cli::KeepViewSwitch):
					prefs.setKeepView(TRUE);
					break;
				// Set type mappings
				case (Cli::MapSwitch):
					// Get the argument and parse it internally
					parser.getArgsDelim(argv[++argn], Parser::Defaults);
					for (n=0; n<parser.nArgs(); n++)
					{
						el = elements().findAlpha(afterChar(parser.argc(n), '='));
						if (el == 0) printf("Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
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
				// Load and run a script file
				case (Cli::ScriptSwitch):
					script = aten.scripts.add();
					if (script->load(argv[++argn]))
					{
						aten.setProgramMode(Aten::CommandMode);
						if (!script->execute()) aten.setProgramMode(Aten::NoMode);
						// Need to check program mode after each script since it can be changed
						if (aten.programMode() == Aten::CommandMode) aten.setProgramMode(Aten::GuiMode);
					}
					else
					{
						aten.scripts.remove(script);
						return -1;
					}
					break;
				// Associate trajectory with last loaded model
				case (Cli::TrajectorySwitch):
					// Check for a current model
					if (current.m == NULL) printf("There is no current model to associate a trajectory to.\n");
					else
					{
						Filter *f = probeFile(argv[++argn], Filter::TrajectoryImport);
						if (f == NULL) return -1;
						if (!current.m->initialiseTrajectory(argv[argn],f)) return -1;
					}
					break;
				// Set maximum number of undolevels per model
				case (Cli::UndoLevelSwitch):
					prefs.setMaxUndoLevels(atoi(argv[++argn]));
					break;
				// Set the type of element (Z) mapping to use in name conversion
				case (Cli::ZmapSwitch):
					zm = ElementMap::zmapType(argv[++argn]);
					if (zm != ElementMap::nZmapTypes) prefs.setZmapType(zm);
					break;
			}
		}
		else
		{
			// Not a CLI switch, so try to load it as a model
			ntried ++;
			if (modelfilter != NULL) f = modelfilter;
			else f = aten.probeFile(argv[argn], Filter::ModelImport);
			if (f != NULL) f->execute(argv[argn]);
			else return -1;
		}
	}
	return aten.nModels();
}

// Usage help
void Aten::printUsage() const
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
