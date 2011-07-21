/*
	*** Command-line option parsing
	*** src/main/cli.cpp
	Copyright T. Youngs 2007-2011

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
#include "main/version.h"
#include "model/model.h"
#include "classes/prefs.h"
#include "parser/tree.h"
#include "base/sysfunc.h"
#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include <parser/double.h>
#include <parser/integer.h>
#include <parser/character.h>

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argtext,description)
Cli cliSwitches[] = {
	{ Cli::AtenDataSwitch,		'\0',"atendata",	1,
		"<dir>",
		"Set the data location to the supplied directory (and don't read $ATENDATA)" },
	{ Cli::BatchSwitch,		'\0',"batch",		0,
		"",
		"Run any commands supplied with -c or --command on all models and save in the original format" },
	{ Cli::BohrSwitch,		'b',"bohr",		0,
		"",
		"Converts model/grid atomic positions from Bohr to Angstrom" },
	{ Cli::BondSwitch,		'\0',"bond",		0,
		"",
		"Force (re)calculation of bonding in the model" },
	{ Cli::CacheSwitch,		'\0',"cachelimit",	1,
		"<limit>",
		"Set the trajectory cache limit to <limit> kb"},
	{ Cli::CentreSwitch,		'\0',"centre",		0,
		"",
		"Force centering of atomic coordinates at zero" },
	{ Cli::CommandSwitch,		'c',"command",		1,
		"<commands>",
		"Execute supplied commands before main program execution" },
	{ Cli::DebugSwitch,		'd',"debug",		2,
		"[output]",
		"Print out call debug information, or specific information if output type is supplied" },
	{ Cli::DoubleSwitch,		'\0',"double",		1,
		"<var>=<value>",
		"Pass a floating point <value> into Aten with variable name <var>" },
	{ Cli::ExportSwitch,		'e',"export",		1,
		"<nickname>",
		"Export all loaded models in the nicknamed format" },
	{ Cli::ExportMapSwitch,		'\0',"exportmap",	1,
		"<name=element,...>",
		"Map forcefield atomtypes to names supplied (for export)" },
	{ Cli::ExpressionSwitch,	'\0',"expression",	1,
		"<filename>",
		"Load the specified forcefield expression file" },
	{ Cli::FilterSwitch,		'\0',"filter",		1,
		"<filename>",
		"Load additional filter data from specified filename" },
	{ Cli::ForcefieldSwitch,	'\0',"ff",		1,
		"<file>",
		"Load the specified forcefield file" },
	{ Cli::FoldSwitch,		'\0',"fold",		0,
		"",
		"Force folding of atoms in periodic systems" },
	{ Cli::FormatSwitch,		'f',"format",		1,
		"",
		"Load models from command-line with specified <format>" },
	{ Cli::GridSwitch,		'g',"grid",		1,
		"<file>",
		"Load the specified gridded data file" },
	{ Cli::HelpSwitch,		'h',"help",		0,
		"",
		"Print this information" },
	{ Cli::IntSwitch,		'\0',"int",		1,
		"<var>=<value>",
		"Pass an integer <value> into Aten with variable name <var>" },
	{ Cli::InteractiveSwitch,	'i',"interactive",	0,
		"",
		"Enter interactive mode" },
	{ Cli::KeepNamesSwitch,		'\0',"keepnames",	0,
		"",
		"Store atom (type)names given in files in a forcefield created for the model" },
	{ Cli::KeepTypesSwitch,		'\0',"keeptypes",	0,
		"",
		"Assign and fix corresponding atom types to atoms whose names have been converted from forcefield zmapping" },
	{ Cli::KeepViewSwitch,		'k',"keepview",		0,
		"",
		"Keep (don't reset) view when GUI starts" },
	{ Cli::LoadFromListSwitch,	'\0',"loadfromlist",	1,
		"<file>",
		"Assume that <file> is a textfile containing a list of filenames to be loaded as models" },
	{ Cli::MapSwitch,		'm',"map",		1,
		"<name=element,...>",
		"Map file atomtypes to elements" },
	{ Cli::NewModelSwitch,		'n',"new",		0,
		"",
		"Creates a new, empty model" },
	{ Cli::NoBondSwitch,		'\0',"nobond",		0,
		"",
		"Prevent (re)calculation of bonding in the model" },
	{ Cli::NoCentreSwitch,		'\0',"nocentre",	0,
		"",
		"Prevent centering of atomic coordinates at zero" },
	{ Cli::NoFiltersSwitch,	'\0',"nofilters",	0,
		"",
		"Prevent loading of filters from standard locations on startup" },
	{ Cli::NoFoldSwitch,		'\0',"nofold",		0,
		"",
		"Prevent folding of atoms in periodic systems" },
	{ Cli::NoFragmentsSwitch,	'\0',"nofragments",	0,
		"",
		"Prevent loading of fragments on startup" },
	{ Cli::NoFragmentIconsSwitch,	'\0',"nofragmenticons",	0,
		"",
		"Prevent generation of fragment icons" },
	{ Cli::NoIncludesSwitch,	'\0',"noincludes",	0,
		"",
		"Prevent loading of includes on startup" },
	{ Cli::NoListsSwitch,		'\0',"nolists",		0,
		"",
		"Disable use of OpenGL display lists" },
	{ Cli::NoPackSwitch,		'\0',"nopack",		0,
		"",
		"Prevent generation of symmetry-equivalent atoms from spacegroup information" },
	{ Cli::NoPartitionsSwitch,	'\0',"nopartitions",	0,
		"",
		"Prevent loading of partitions on startup" },
	{ Cli::NoQtSettingsSwitch,	'\0',"noqtsettings",	0,
		"",
		"Don't load in Qt window/toolbar settings on startup" },
	{ Cli::PackSwitch,		'\0',"pack",		0,
		"",
		"Force generation of symmetry-equivalent atoms from spacegroup information" },
	{ Cli::PipeSwitch,		'p',"pipe",		0,
		"",
		"Read and execute commands from piped input" },
	{ Cli::ProcessSwitch,		'\0',"process",		0,
		"",
		"Run any commands supplied with -c or --command on all models (but don't save)" },
	{ Cli::QuietSwitch,		'q',"quiet",		0,
		"",
		"Run silently, reporting only errors that stop the program" },
	{ Cli::ScriptSwitch,		's',"script",		1,
		"<file>",
		"Load and execute the script file specified" },
	{ Cli::StringSwitch,		'\0',"string",		1,
		"<var>=<value>",
		"Pass a string <value> into Aten with variable name <var>" },
	{ Cli::TrajectorySwitch,	't',"trajectory",	1,
		"<file>",
		"Associate a trajectory with the last loaded model" },
	{ Cli::UndoLevelSwitch,		'u',"undolevels",	1,
		"<nlevels>",
		"Set the maximum number of undo levels per model (-1 = unlimited)" },
	{ Cli::VBOSwitch,		'\0',"vbo",		0,
		"",
		"Use OpenGL Vertex Buffer Objects for rendering" },
	{ Cli::VerboseSwitch,		'v',"verbose",		0,
		"",
		"Enable verbose program output" },
	{ Cli::VersionSwitch,		'\0',"version",		0,
		"",
		"Print program version and exit" },
	{ Cli::ZmapSwitch,		'z',"zmap",		1,
		"<mapstyle>",	"Override filter element mapping style" }
};

/*
// Member functions
*/
// Search for short option
Cli::CliSwitch Cli::cliSwitch(char c)
{
	int n;
	for (n=0; n<Cli::nSwitchItems; ++n)
	{
		if (cliSwitches[n].shortOpt == '\0') continue;
		if (c == cliSwitches[n].shortOpt) break;
	}
	return (Cli::CliSwitch) n;
}

// Search for long option
Cli::CliSwitch Cli::cliSwitch(const char *s)
{
	int n;
	for (n=0; n<Cli::nSwitchItems; ++n)
	{
		if (cliSwitches[n].longOpt[0] == '\0') continue;
		if (strcmp(s, cliSwitches[n].longOpt) == 0) break;
	}
	return (Cli::CliSwitch) n;
}

// Parse CLI options *before* filters / prefs have been loaded
bool Aten::parseCliEarly(int argc, char *argv[])
{
	int argn, opt;
	bool isShort, hasArg;
	Messenger::OutputType ot;
	Dnchar arg;
	Dnchar argtext;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		argn++;
		// Check for null argument
		if (argv[argn][0] == '\0') continue;
		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn],"-psn",4) == 0)
			{
				printf("Found (and ignored) OSX-added '%s'.\n",argv[argn]);
				continue;
			}
			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			if (isShort)
			{
				arg = &argv[argn][1];
				argtext.clear();
				if ((argv[argn][1] != '\0') && (argv[argn][2] != '\0'))
				{
					isShort = FALSE;
					arg = beforeChar(&argv[argn][1], '=');
					argtext = afterChar(&argv[argn][1], '=');
				}
			}
			else
			{
				arg = beforeChar(&argv[argn][2], '=');
				argtext = afterChar(&argv[argn][2], '=');
			}
			// Search for option...
			opt = (isShort ? Cli::cliSwitch(arg[0]) : Cli::cliSwitch(arg.get()));
			// Check to see if we matched any of the known CLI switches
			if (opt == Cli::nSwitchItems)
			{
				printf("Unrecognised command-line option '%s%s'.\n", isShort ? "-" : "--", arg.get());
				return FALSE;
			}
			// Check if an argument to the switch has been supplied...
			if (argtext != "") hasArg = TRUE;
			else if ((argn < (argc-1)) && (argv[argn+1][0] != '-') && (cliSwitches[opt].argument != 0))
			{
				hasArg = TRUE;
				argtext = argv[++argn];
			}
			else hasArg = FALSE;
			// ...and whether it expects one
			switch (cliSwitches[opt].argument)
			{
				// No argument required
				case (0):
					break;
				// Required argument
				case (1):
					if (!hasArg)
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
				case (Cli::AtenDataSwitch):
					aten.setDataDir(argtext.get());
					msg.print("Will search for filters in '%s'.\n", argtext.get());
					break;
				// Turn on debug messages for calls (or specified output)
				case (Cli::DebugSwitch):
					if (!hasArg) msg.addOutputType(Messenger::Calls);
					else
					{
						ot = Messenger::outputType(argtext.get(), TRUE);
						if (ot != Messenger::nOutputTypes) msg.addOutputType(ot);
						else return FALSE;
					}
					break;
				// Restrict filter loading on startup
				case (Cli::NoFiltersSwitch):
					prefs.setLoadFilters(FALSE);
					break;
				// Restrict fragment loading on startup
				case (Cli::NoFragmentsSwitch):
					prefs.setLoadFragments(FALSE);
					break;
				// Restrict fragment icon generation
				case (Cli::NoFragmentIconsSwitch):
					prefs.setGenerateFragmentIcons(FALSE);
					break;
				// Restrict include loading on startup
				case (Cli::NoIncludesSwitch):
					prefs.setLoadIncludes(FALSE);
					break;
				// Restrict partition loading on startup
				case (Cli::NoPartitionsSwitch):
					prefs.setLoadPartitions(FALSE);
					break;
				// Turn off display list usage
				case (Cli::NoListsSwitch):
					msg.print("OpenGL display lists will not be used.\n");
					prefs.setInstanceType(PrimitiveInstance::NoInstance);
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
				// Turn on VBO usage
				case (Cli::VBOSwitch):
					msg.print("OpenGL Vertex Buffer Objects will be used.\n");
					prefs.setInstanceType(PrimitiveInstance::VBOInstance);
					break;
				// Turn on verbose messaging
				case (Cli::VerboseSwitch):
					msg.print("Verbosity enabled.\n");
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
	int argn, opt, ntried = 0, n, el, i;
	bool isShort, hasArg;
	char *line;
	Dnchar arg, argtext, varname, varvalue, prompt;
	Forcefield *ff;
	LineParser parser;
	ElementMap::ZMapType zm;
	NameMap<int> *nmi;
	Model *m;
	Program *script, tempforest;
	ReturnValue rv;
	Tree *f, *modelfilter = NULL;
	Program interactiveScript;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		argn++;
		// Check for null argument
		if (argv[argn][0] == '\0') continue;
		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn],"-psn",4) == 0)
			{
				msg.print("Found (and ignored) OSX-added '%s'.\n",argv[argn]);
				continue;
			}
			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			if (isShort)
			{
				arg = &argv[argn][1];
				argtext.clear();
				if ((argv[argn][1] != '\0') && (argv[argn][2] != '\0'))
				{
					isShort = FALSE;
					arg = beforeChar(&argv[argn][1], '=');
					argtext = afterChar(&argv[argn][1], '=');
				}
			}
			else
			{
				arg = beforeChar(&argv[argn][2], '=');
				argtext = afterChar(&argv[argn][2], '=');
			}
			// Search for option...
			opt = (isShort ? Cli::cliSwitch(arg[0]) : Cli::cliSwitch(arg.get()));
			// Check to see if we matched any of the known CLI switches
			if (opt == Cli::nSwitchItems)
			{
				printf("Unrecognised command-line option '%s%s'.\n", isShort ? "-" : "--", arg.get());
				return -1;
			}
			// Check if an argument to the switch has been supplied...
			if (argtext != "") hasArg = TRUE;
			else if ((argn < (argc-1)) && (argv[argn+1][0] != '-') && (cliSwitches[opt].argument != 0))
			{
				hasArg = TRUE;
				argtext = argv[++argn];
			}
			else hasArg = FALSE;
			// ...and whether it expects one
			switch (cliSwitches[opt].argument)
			{
				// No argument required
				case (0):
					break;
				// Required argument
				case (1):
					if (!hasArg)
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
				// All of the following switches were dealt with in parseCliEarly(), so ignore them
				case (Cli::AtenDataSwitch):
				case (Cli::DebugSwitch):
				case (Cli::HelpSwitch):
				case (Cli::NoFiltersSwitch):
				case (Cli::NoFragmentsSwitch):
				case (Cli::NoFragmentIconsSwitch):
				case (Cli::NoIncludesSwitch):
				case (Cli::NoListsSwitch):
				case (Cli::NoPartitionsSwitch):
				case (Cli::QuietSwitch):
				case (Cli::VBOSwitch):
				case (Cli::VerboseSwitch):
				case (Cli::VersionSwitch):
					break;
				// Enable batch mode
				case (Cli::BatchSwitch):
					if (aten.programMode() == Aten::ProcessMode)
					{
						printf("Error: --batch and --process options are mutually exclusive.\n");
						return -1;
					}
					else if (aten.programMode() == Aten::ExportMode) aten.setProgramMode(Aten::BatchExportMode);
					else aten.setProgramMode(Aten::BatchMode);
					break;
				// Convert coordinates from Bohr to Angstrom
				case (Cli::BohrSwitch):
					prefs.setCoordsInBohr(TRUE);
					break;
				// Force bonding calculation of atoms on load
				case (Cli::BondSwitch):
					prefs.setBondOnLoad(TRUE);
					break;
				// Set trajectory cache limit
				case (Cli::CacheSwitch):
					prefs.setCacheLimit(argtext.asInteger());
					break;
				// Force model centering on load (for non-periodic systems)
				case (Cli::CentreSwitch):
					prefs.setCentreOnLoad(TRUE);
					break;
				// Read commands from passed string and execute them
				case (Cli::CommandSwitch):
					if ((aten.programMode() == Aten::BatchMode) || (aten.programMode() == Aten::ProcessMode) || (aten.programMode() == Aten::BatchExportMode))
					{
						script = aten.addBatchCommand();
						if (!script->generateFromString(argtext.get(), "batchcommand")) return -1;
					}
					else
					{
						tempforest.clear();
						if (tempforest.generateFromString(argtext.get(), "CLI command"))
						{
							if (!tempforest.execute(rv)) return -1;
						}
						else return -1;
					}
					break;
				// Export all models in nicknamed format (single-shot mode)
				case (Cli::ExportSwitch):
					if (aten.programMode() == Aten::ProcessMode)
					{
						printf("Error: --export and --process options are mutually exclusive.\n");
						return -1;
					}

					// Parse the first option so we can get the filter nickname and any filter options
					parser.getArgsDelim(LineParser::UseQuotes, argtext.get());
					
					// First part of argument is nickname
					f = aten.findFilter(FilterData::ModelExport, parser.argc(0));
					// Check that a suitable format was found
					if (f == NULL)
					{
						// Print list of valid filter nicknames
						aten.printValidNicknames(FilterData::ModelExport);
						return -1;
					}

					// Loop over remaining arguments to set filter options
					for (i = 1; i < parser.nArgs(); ++i)
					{
						if (!f->setVariable(beforeStr(parser.argc(i),"="), afterStr(parser.argc(i),"="))) return -1;
					}
					
					aten.setExportFilter(f);
					if (aten.programMode() == Aten::BatchMode) aten.setProgramMode(Aten::BatchExportMode);
					else aten.setProgramMode(Aten::ExportMode);
					break;
				// Set export type remappings
				case (Cli::ExportMapSwitch):
					// Get the argument and parse it internally
					parser.getArgsDelim(0, argtext.get());
					if (strchr(parser.argc(n),'=') == NULL)
					{
						printf("Mangled exportmap value found (i.e. it contains no '='): '%s'.\n", parser.argc(n));
						return -1;
					}
					for (n=0; n<parser.nArgs(); n++) typeExportMap.add(beforeChar(parser.argc(n),'='), afterChar(parser.argc(n), '='));
					break;
				// Load expression
				case (Cli::ExpressionSwitch):
					f = aten.probeFile(argtext.get(), FilterData::ExpressionImport);
					if (f == NULL) return -1;
					else if (!f->executeRead(argtext.get())) return -1;
					break;
				// Load additional filter data from specified filename
				case (Cli::FilterSwitch):
					if (!aten.openFilter(argtext.get())) return -1;
					break;
				// Force folding (MIM'ing) of atoms in periodic systems on load
				case (Cli::FoldSwitch):
					prefs.setFoldOnLoad(TRUE);
					break;
				// Load the specified forcefield
				case (Cli::ForcefieldSwitch):
					ff = aten.loadForcefield(argtext.get());
					if (ff == NULL) return -1;
					break;
				// Set forced model load format
				case (Cli::FormatSwitch):
					modelfilter = aten.findFilter(FilterData::ModelImport, argtext.get());
					if (modelfilter == NULL)
					{
						// Print list of valid filter nicknames
						aten.printValidNicknames(FilterData::ModelImport);
						return -1;
					}
					break;
				// Load surface
				case (Cli::GridSwitch):
					f = aten.probeFile(argtext.get(), FilterData::GridImport);
					if (f == NULL) return -1;
					else if (!f->executeRead(argtext.get())) return -1;
					break;
				// Pass value
				case (Cli::DoubleSwitch):
				case (Cli::IntSwitch):
				case (Cli::StringSwitch):
					// Split argument into name and value
					varname = beforeChar(argtext.get(), '=');
					varvalue = afterChar(argtext.get(), '=');
					if (passedValues_.find(varname.get()) != NULL)
					{
						printf("Error: Passed variable named '%s' has already been declared.\n", varname.get());
						return -1;
					}
					else if (opt == Cli::IntSwitch) addPassedValue(VTypes::IntegerData, varname.get(), varvalue.get());
					else if (opt == Cli::DoubleSwitch) addPassedValue(VTypes::DoubleData, varname.get(), varvalue.get());
					else if (opt == Cli::StringSwitch) addPassedValue(VTypes::StringData, varname.get(), varvalue.get());
					break;
				// Enter interactive mode
				case (Cli::InteractiveSwitch):
					prompt.sprintf("Aten %s > ",ATENVERSION);
					printf("Entering interactive mode...\n");
					aten.setProgramMode(Aten::InteractiveMode);
					do
					{
						// Get string from user
						line = readline(prompt);
						interactiveScript.mainProgram()->reset(FALSE);
						if (interactiveScript.generateFromString(line,"InteractiveCommand",FALSE,FALSE)) interactiveScript.execute(rv);
						// Add the command to the history and delete it 
						add_history(line);
						free(line);
					} while (aten.programMode() == Aten::InteractiveMode);
					break;
				// Keep atom names in file
				case (Cli::KeepNamesSwitch):
					// Mutually exclusive with keeptypes
					if  (prefs.keepTypes())
					{
						printf("Error: --keepnames and --keeptypes are mutually exclusive.\n");
						return -1;
					}
					prefs.setKeepNames(TRUE);
					break;
				// Keep atom type names in file
				case (Cli::KeepTypesSwitch):
					// Mutually exclusive with keepnames
					if  (prefs.keepNames())
					{
						printf("Error: --keepnames and --keeptypes are mutually exclusive.\n");
						return -1;
					}
					prefs.setKeepTypes(TRUE);
					break;
				// Keep (don't reset) view when GUI starts
				case (Cli::KeepViewSwitch):
					prefs.setKeepView(TRUE);
					break;
				// Load models from list in file
				case (Cli::LoadFromListSwitch):
					if (!parser.openFile(argtext.get())) return -1;
					while (!parser.eofOrBlank())
					{
						parser.readNextLine(LineParser::StripComments);
						ntried ++;
						if (modelfilter != NULL) f = modelfilter;
						else f = aten.probeFile(parser.line(), FilterData::ModelImport);
						if (f != NULL) f->executeRead(parser.line());
						else return -1;
					}
					break;
				// Set type mappings
				case (Cli::MapSwitch):
					// Get the argument and parse it internally
					parser.getArgsDelim(0, argtext.get());
					for (n=0; n<parser.nArgs(); n++)
					{
						if (strchr(parser.argc(n),'=') == NULL)
						{
							printf("Mangled map value found (i.e. it contains no '='): '%s'.\n", parser.argc(n));
							return -1;
						}
						el = elements().find(afterChar(parser.argc(n), '='), ElementMap::AlphaZMap);
						if (el == 0)
						{
							printf("Unrecognised element '%s' in type map.\n",afterChar(parser.argc(n),'='));
							return -1;
						}
						else
						{
							nmi = typeImportMap.add();
							nmi->set(beforeChar(parser.argc(n),'='), el);
						}
					}
					break;
				// Create a new model
				case (Cli::NewModelSwitch):
					m = aten.addModel();
					m->enableUndoRedo();
					break;
				// Prohibit bonding calculation of atoms on load
				case (Cli::NoBondSwitch):
					prefs.setBondOnLoad(FALSE);
					break;
				// Prohibit model centering on load (for non-periodic systems)
				case (Cli::NoCentreSwitch):
					prefs.setCentreOnLoad(FALSE);
					break;
				// Prohibit folding (MIM'ing) of atoms in periodic systems on load
				case (Cli::NoFoldSwitch):
					prefs.setFoldOnLoad(FALSE);
					break;
				// Force packing (application of symmetry operators) on load
				case (Cli::NoPackSwitch):
					prefs.setPackOnLoad(FALSE);
					break;
				// Don't load Qt window/toolbar settings on startup
				case (Cli::NoQtSettingsSwitch):
					prefs.setLoadQtSettings(FALSE);
					break;
				// Prohibit packing (application of symmetry operators) on load
				case (Cli::PackSwitch):
					prefs.setPackOnLoad(TRUE);
					break;
				// Read and execute commads from pipe
				case (Cli::PipeSwitch):
					prefs.setReadPipe(TRUE);
					break;
				// Enable processing mode
				case (Cli::ProcessSwitch):
					if ((aten.programMode() == Aten::ExportMode) || (aten.programMode() == Aten::BatchExportMode) || (aten.programMode() == Aten::BatchMode))
					{
						printf("Error: --process and --batch / --export options are mutually exclusive.\n");
						return -1;
					}
					else aten.setProgramMode(Aten::ProcessMode);
					break;
				// Load and run a script file
				case (Cli::ScriptSwitch):
					script = aten.addScript();
					if (script->generateFromFile(argtext.get(), "CliScript"))
					{
						msg.print("Successfully generated script.\n");
						aten.setProgramMode(Aten::CommandMode);
						if (!script->execute(rv))
						{
							msg.print("Script execution failed.\n");
							return -1;
						}
						// Need to check program mode after each script since it can be changed
						if (aten.programMode() == Aten::CommandMode) aten.setProgramMode(Aten::GuiMode);
					}
					else
					{
						aten.removeScript(script);
						return -1;
					}
					break;
				// Associate trajectory with last loaded model
				case (Cli::TrajectorySwitch):
					// Check for a current model
					if (current.m == NULL)
					{
						printf("There is no current model to associate a trajectory to.\n");
						return -1;
					}
					else
					{
						Tree *f = probeFile(argtext.get(), FilterData::TrajectoryImport);
						if (f == NULL) return -1;
						if (!current.m->initialiseTrajectory(argtext.get(),f)) return -1;
					}
					break;
				// Set maximum number of undolevels per model
				case (Cli::UndoLevelSwitch):
					prefs.setMaxUndoLevels(argtext.asInteger());
					break;
				// Set the type of element (Z) mapping to use in name conversion
				case (Cli::ZmapSwitch):
					zm = ElementMap::zMapType(argtext.get(), TRUE);
					if (zm != ElementMap::nZMapTypes) prefs.setZMapType(zm, TRUE);
					else return -1;
					break;
				// Undefined option
				default:
					printf("Shoddy programming alert - CLI option has not been implemented.\n");
					return -1;
			}
		}
		else
		{
			// Not a CLI switch, so try to load it as a model
			ntried ++;
			if (modelfilter != NULL) f = modelfilter;
			else f = aten.probeFile(argv[argn], FilterData::ModelImport);
			if (f != NULL) f->executeRead(argv[argn]);
			else return -1;
		}
	}

	// Anything redirected to stdin (or forcibly piped)?
	bool readcin = FALSE;
	if (prefs.readPipe()) readcin = TRUE;
	else
	{
		cin.seekg(0, ios::end);
		streampos endpos = cin.tellg();
		if ((endpos != streampos(-1)) && (endpos != streampos(0))) readcin = TRUE;
		cin.seekg(0, ios::beg);
		
	}
	if (readcin)
	{
		// Grab all lines from cin to a temporary stringlist....
		List<Dnchar> commands;
		char line[8096];
		while (cin.good() && (!cin.eof()))
		{
			cin.getline(line, 8095);
			// Remove any commented part of line
			removeComments(line);
			if (isEmpty(line)) continue;
			Dnchar *s = commands.add();
			s->set(line);
		}
		// Create and execute commands
		if ((aten.programMode() == Aten::BatchMode) || (aten.programMode() == Aten::ProcessMode) || (aten.programMode() == Aten::BatchExportMode))
		{
			script = aten.addBatchCommand();
			if (!script->generateFromStringList(commands.first(), "batchcommand")) return -1;
		}
		else
		{
			tempforest.clear();
			if (tempforest.generateFromStringList(commands.first(), "CLI command (cin)"))
			{
				if (!tempforest.execute(rv)) return -1;
			}
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

// Add passed value
bool Aten::addPassedValue(VTypes::DataType type, const char *name, const char *value)
{
	// Search for existing passed value of this name...
	if (passedValues_.find(name)) return FALSE;
	Variable *var = NULL;
	if (type == VTypes::IntegerData) var = new IntegerVariable(atoi(value), TRUE);
	else if (type == VTypes::DoubleData) var = new DoubleVariable(atof(value), TRUE);
	else if (type == VTypes::StringData) var = new StringVariable(value, TRUE);
	else printf("Internal Error: Don't know how to create a passed value of type '%s'.\n", VTypes::dataType(type));
	var->setName(name);
	passedValues_.take(var, TRUE);
	return TRUE;
}

// Find passed value
Variable *Aten::findPassedValue(const char *name) const
{
	return passedValues_.find(name);
}
