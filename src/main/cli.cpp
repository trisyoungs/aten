/*
	*** Command-line option parsing
	*** src/main/cli.cpp
	Copyright T. Youngs 2007-2016

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
#include "base/prefs.h"
#include "render/primitiveinstance.h"
#include "base/sysfunc.h"
#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include "parser/double.h"
#include "parser/integer.h"
#include "parser/character.h"

ATEN_USING_NAMESPACE

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argText,description)
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
	{ Cli::DialogsSwitch,		'\0',"dialogs",		0,
		"",
		"Permit script/filter dialogs to be raised even if the main GUI doesn't exist" },
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
		"<format>",
		"Load models from command-line assuming specified <format>" },
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
	{ Cli::ListsSwitch,		'\0',"lists",		0,
		"",
		"Enable use of OpenGL display lists (in preference to VBOs)" },
	{ Cli::LoadFromListSwitch,	'\0',"loadfromlist",	1,
		"<file>",
		"Assume that <file> is a textfile containing a list of filenames to be loaded as models" },
	{ Cli::MapSwitch,		'm',"map",		1,
		"<name=element,...>",
		"Map file atomtypes to elements" },
	{ Cli::NewModelSwitch,		'n',"new",		0,
		"",
		"Creates a new, empty model" },
	{ Cli::NicknamesSwitch,		'\0',"nicknames",	0,
		"",
		"Show all available filter nicknames and quit" },
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
	{ Cli::NoInstancesSwitch,		'\0',"noinstances",		0,
		"",
		"Disable use of both OpenGL display lists and VBOs" },
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
	{ Cli::TrajectoryFormatSwitch,	'\0',"tf",	1,
		"<format>",
		"Load trajectories from command-line assuming specified <format>" },
	{ Cli::UndoLevelSwitch,		'u',"undolevels",	1,
		"<nlevels>",
		"Set the maximum number of undo levels per model (-1 = unlimited)" },
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
 * Member functions
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
Cli::CliSwitch Cli::cliSwitch(QString s)
{
	int n;
	for (n=0; n<Cli::nSwitchItems; ++n)
	{
		if (cliSwitches[n].longOpt[0] == '\0') continue;
		if (s == cliSwitches[n].longOpt) break;
	}
	return (Cli::CliSwitch) n;
}

// Parse CLI options *before* filters / prefs have been loaded
bool Aten::parseCliEarly(int argc, char *argv[])
{
	int argn, opt;
	bool isShort, hasArg;
	Messenger::OutputType ot;
	QString arg, argText;

	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		++argn;

		// Check for null argument
		if (argv[argn][0] == '\0') continue;

		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn], "-psn", 4) == 0)
			{
				printf("Found (and ignored) OSX-added '%s'.\n", argv[argn]);
				continue;
			}

			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			if (isShort)
			{
				arg = &argv[argn][1];
				argText.clear();
				if ((argv[argn][1] != '\0') && (argv[argn][2] != '\0'))
				{
					isShort = false;
					QStringList items = QString(&argv[argn][1]).split('=');
					arg = items.at(0);
					if (items.count() == 2) argText = items.at(1);
				}
			}
			else
			{
				QStringList items = QString(&argv[argn][2]).split('=');
				arg = items.at(0);
				if (items.count() == 2) argText = items.at(1);
			}

			// Search for option...
			opt = (isShort ? Cli::cliSwitch(arg.at(0).toLatin1()) : Cli::cliSwitch(arg));

			// Check to see if we matched any of the known CLI switches
			if (opt == Cli::nSwitchItems)
			{
				printf("Unrecognised command-line option '%s%s'.\n", isShort ? "-" : "--", qPrintable(arg));
				return false;
			}

			// Check if an argument to the switch has been supplied...
			if (!argText.isEmpty()) hasArg = true;
			else if ((argn < (argc-1)) && (argv[argn+1][0] != '-') && (cliSwitches[opt].argument != 0))
			{
				hasArg = true;
				argText = argv[++argn];
			}
			else hasArg = false;

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
						if (isShort) Messenger::print("Usage Error: '-%c' requires an argument.", cliSwitches[opt].shortOpt);
						else Messenger::print("Usage Error: '--%s' requires an argument.", cliSwitches[opt].longOpt);
						return false;
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
				// Set data directory location
				case (Cli::AtenDataSwitch):
					dataDir_ = argText;
					Messenger::print("Will search for filters in '%s'.", qPrintable(argText));
					break;
				// Turn on debug messages for calls (or specified output)
				case (Cli::DebugSwitch):
					if (!hasArg) Messenger::addOutputType(Messenger::Calls);
					else
					{
						ot = Messenger::outputType(argText, true);
						if (ot != Messenger::nOutputTypes) Messenger::addOutputType(ot);
						else return false;
					}
					break;
				// Display help
				case (Cli::HelpSwitch):
					printUsage();
					return false;
					break;
				// Turn on display list usage (in preference to VBOs)
				case (Cli::ListsSwitch):
					Messenger::print("OpenGL display lists will be used for rendering instead of VBOs.");
					PrimitiveInstance::setInstanceType(PrimitiveInstance::ListInstance);
					break;
				// Restrict filter loading on startup
				case (Cli::NoFiltersSwitch):
					prefs.setLoadFilters(false);
					break;
				// Restrict fragment loading on startup
				case (Cli::NoFragmentsSwitch):
					prefs.setLoadFragments(false);
					break;
				// Restrict fragment icon generation
				case (Cli::NoFragmentIconsSwitch):
					prefs.setGenerateFragmentIcons(false);
					break;
				// Restrict include loading on startup
				case (Cli::NoIncludesSwitch):
					prefs.setLoadIncludes(false);
					break;
				// Turn off VBO and display list usage
				case (Cli::NoInstancesSwitch):
					Messenger::print("VBO and display lists will not be used for rendering.");
					PrimitiveInstance::setInstanceType(PrimitiveInstance::NoInstances);
					break;
				// Restrict partition loading on startup
				case (Cli::NoPartitionsSwitch):
					prefs.setLoadPartitions(false);
					break;
				// Run in silent mode (no CLI output)
				case (Cli::QuietSwitch):
					Messenger::setQuiet(true);
					break;
				// Turn on verbose messaging
				case (Cli::VerboseSwitch):
					Messenger::print("Verbosity enabled.");
					Messenger::addOutputType(Messenger::Verbose);
					break;
				// Print version and exit
				case (Cli::VersionSwitch):
					printf("Aten version %s.\n", ATENVERSION);
					return false;
					break;
			}
		}
	}
	return true;
}

// Parse CLI options, after filters / prefs have been loaded
int Aten::parseCli(int argc, char *argv[])
{
	int argn, opt, nTried = 0, n, el, i;
	bool isShort, hasArg;
	char* line;
	QString arg, argText, prompt;
	Forcefield* ff;
	LineParser parser;
	ElementMap::ZMapType zm;
	NameMap<int>* nmi;
	Model* model;
	Program* script, tempProgram;
	ReturnValue rv;
	Tree* filter, *modelFilter = NULL, *trajectoryFilter = NULL;
	Program interactiveScript;
	QStringList items;

	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		++argn;

		// Check for null argument
		if (argv[argn][0] == '\0') continue;

		// Check for a CLI argument (presence of '-')
		if (argv[argn][0] == '-')
		{
			// Manually-exclude some specific (and extremely annoying) extraneous command line options
			if (strncmp(argv[argn],"-psn",4) == 0)
			{
				Messenger::print("Found (and ignored) OSX-added '%s'.", argv[argn]);
				continue;
			}

			// Is this a long or short option?
			isShort = (argv[argn][1] != '-');
			argText.clear();
			if (isShort)
			{
				arg = &argv[argn][1];
				if ((argv[argn][1] != '\0') && (argv[argn][2] != '\0'))
				{
					isShort = false;
					int equalsPos = QString(&argv[argn][2]).indexOf("=");
					if (equalsPos != -1)
					{
						arg = QString(&argv[argn][2]).left(equalsPos);
						argText = QString(&argv[argn][2]).remove(0, equalsPos+1);
					}
					else arg = &argv[argn][2];
				}
			}
			else
			{
				int equalsPos = QString(&argv[argn][2]).indexOf("=");
				if (equalsPos != -1)
				{
					arg = QString(&argv[argn][2]).left(equalsPos);
					argText = QString(&argv[argn][2]).remove(0, equalsPos+1);
				}
				else arg = &argv[argn][2];
			}

			// Search for option...
			opt = (isShort ? Cli::cliSwitch(arg.at(0).toLatin1()) : Cli::cliSwitch(arg));

			// Check to see if we matched any of the known CLI switches
			if (opt == Cli::nSwitchItems)
			{
				Messenger::print("Unrecognised command-line option '%s%s'.", isShort ? "-" : "--", qPrintable(arg));
				return -1;
			}

			// Check if an argument to the switch has been supplied...
			if (!argText.isEmpty()) hasArg = true;
			else if ((argn < (argc-1)) && (argv[argn+1][0] != '-') && (cliSwitches[opt].argument != 0))
			{
				hasArg = true;
				argText = argv[++argn];
			}
			else hasArg = false;

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
						if (isShort) Messenger::print("Usage Error: '-%c' requires an argument.", cliSwitches[opt].shortOpt);
						else Messenger::print("Usage Error: '--%s' requires an argument.", cliSwitches[opt].longOpt);
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
				case (Cli::ListsSwitch):
				case (Cli::NoFiltersSwitch):
				case (Cli::NoFragmentsSwitch):
				case (Cli::NoFragmentIconsSwitch):
				case (Cli::NoIncludesSwitch):
				case (Cli::NoInstancesSwitch):
				case (Cli::NoPartitionsSwitch):
				case (Cli::QuietSwitch):
				case (Cli::VerboseSwitch):
				case (Cli::VersionSwitch):
					break;
				// Enable batch mode
				case (Cli::BatchSwitch):
					if (programMode_ == Aten::ProcessMode)
					{
						Messenger::print("Error: --batch and --process options are mutually exclusive.");
						return -1;
					}
					else if (programMode_ == Aten::ExportMode) programMode_ = Aten::BatchExportMode;
					else programMode_ = Aten::BatchMode;
					break;
				// Convert coordinates from Bohr to Angstrom
				case (Cli::BohrSwitch):
					prefs.setCoordsInBohr(true);
					break;
				// Force bonding calculation of atoms on load
				case (Cli::BondSwitch):
					prefs.setBondOnLoad(Choice::Yes);
					break;
				// Set trajectory cache limit
				case (Cli::CacheSwitch):
					prefs.setCacheLimit(argText.toInt());
					break;
				// Force model centering on load (for non-periodic systems)
				case (Cli::CentreSwitch):
					prefs.setCentreOnLoad(Choice::Yes);
					break;
				// Read commands from passed string and execute them
				case (Cli::CommandSwitch):
					if ((programMode_ == Aten::BatchMode) || (programMode_ == Aten::ProcessMode) || (programMode_ == Aten::BatchExportMode))
					{
						script = addBatchCommand();
						if (!script->generateFromString(argText, "BatchCommand", "Batch Command")) return -1;
					}
					else
					{
						tempProgram.clear();
						if (tempProgram.generateFromString(argText, "CLI Command", "CLI Command"))
						{
							if (!tempProgram.execute(rv)) return -1;
							else if (tempProgram.mainProgram()->acceptedFail() == Commands::Quit) return 0;
						}
						else return -1;
					}
					break;
				// Allow script/filter dialogs to be raised
				case (Cli::DialogsSwitch):
					prefs.setAllowDialogs(true);
					break;
				// Export all models in nicknamed format (single-shot mode)
				case (Cli::ExportSwitch):
					if (programMode_ == Aten::ProcessMode)
					{
						Messenger::print("Error: --export and --process options are mutually exclusive.");
						return -1;
					}

					// Parse the first option so we can get the filter nickname and any filter options
					parser.getArgsDelim(LineParser::UseQuotes, argText);
					
					// First part of argument is nickname
					filter = findFilter(FilterData::ModelExport, parser.argc(0));
					// Check that a suitable format was found
					if (filter == NULL)
					{
						// Print list of valid filter nicknames
						printValidNicknames(FilterData::ModelExport);
						return -1;
					}

					// Loop over remaining arguments which are widget/global variable assignments
					for (i = 1; i < parser.nArgs(); ++i)
					{
						items = parser.argc(i).split('=');
						if (!filter->setAccessibleVariable(items.at(0), items.at(1))) return -1;
					}

					setExportFilter(filter);
					if (programMode_ == Aten::BatchMode) programMode_ = Aten::BatchExportMode;
					else programMode_ = Aten::ExportMode;
					break;
				// Set export type remappings
				case (Cli::ExportMapSwitch):
					// Get the argument and parse it internally
					parser.getArgsDelim(0, argText);
					for (n=0; n<parser.nArgs(); n++) 
					{
						QStringList items = parser.argc(n).split('=');
						if (items.count() != 2)
						{
							Messenger::print("Mangled exportmap value found: '%s'.", qPrintable(parser.argc(n)));
							return -1;
						}
						typeExportMap.add(items.at(0), items.at(1));
					}
					break;
				// Load expression
				case (Cli::ExpressionSwitch):
					filter = probeFile(argText, FilterData::ExpressionImport);
					if (filter == NULL) return -1;
					else if (!filter->executeRead(argText)) return -1;
					break;
				// Load additional filter data from specified filename
				case (Cli::FilterSwitch):
					if (!openFilter(argText)) return -1;
					break;
				// Force folding (MIM'ing) of atoms in periodic systems on load
				case (Cli::FoldSwitch):
					prefs.setFoldOnLoad(Choice::Yes);
					break;
				// Load the specified forcefield
				case (Cli::ForcefieldSwitch):
					ff = loadForcefield(argText);
					if (ff == NULL) return -1;
					break;
				// Set forced model load format
				case (Cli::FormatSwitch):
					modelFilter = findFilter(FilterData::ModelImport, argText);
					if (modelFilter == NULL)
					{
						// Print list of valid filter nicknames
						printValidNicknames(FilterData::ModelImport);
						return -1;
					}
					break;
				// Load surface
				case (Cli::GridSwitch):
					filter = probeFile(argText, FilterData::GridImport);
					if (filter == NULL) return -1;
					else if (!filter->executeRead(argText)) return -1;
					break;
				// Pass value
				case (Cli::DoubleSwitch):
				case (Cli::IntSwitch):
				case (Cli::StringSwitch):
					// Split argument into name and value
					items = argText.split('=');
					if (passedValues_.find(items.at(0)) != NULL)
					{
						Messenger::print("Error: Passed variable named '%s' has already been declared.", qPrintable(items.at(0)));
						return -1;
					}
					else if (opt == Cli::IntSwitch) addPassedValue(VTypes::IntegerData, qPrintable(items.at(0)), qPrintable(items.at(1)));
					else if (opt == Cli::DoubleSwitch) addPassedValue(VTypes::DoubleData, qPrintable(items.at(0)), qPrintable(items.at(1)));
					else if (opt == Cli::StringSwitch) addPassedValue(VTypes::StringData, qPrintable(items.at(0)), qPrintable(items.at(1)));
					break;
				// Enter interactive mode
				case (Cli::InteractiveSwitch):
					prompt.sprintf("Aten %s > ", ATENVERSION);
					Messenger::print("Entering interactive mode...");
					programMode_ = Aten::InteractiveMode;
					do
					{
						// Get string from user
						line = readline(qPrintable(prompt));
						interactiveScript.mainProgram()->reset(false);
						if (interactiveScript.generateFromString(line, "InteractiveCommand", "InteractiveMode", false, false)) interactiveScript.execute(rv);
						// Add the command to the history and delete it 
						add_history(line);
						free(line);
					} while (programMode_ == Aten::InteractiveMode);
					break;
				// Keep atom names in file
				case (Cli::KeepNamesSwitch):
					// Mutually exclusive with keeptypes
					if  (prefs.keepTypes())
					{
						Messenger::print("Error: --keepnames and --keeptypes are mutually exclusive.");
						return -1;
					}
					prefs.setKeepNames(true);
					break;
				// Keep atom type names in file
				case (Cli::KeepTypesSwitch):
					// Mutually exclusive with keepnames
					if  (prefs.keepNames())
					{
						Messenger::print("Error: --keepnames and --keeptypes are mutually exclusive.");
						return -1;
					}
					prefs.setKeepTypes(true);
					break;
				// Keep (don't reset) view when GUI starts
				case (Cli::KeepViewSwitch):
					prefs.setKeepView(true);
					break;
				// Load models from list in file
				case (Cli::LoadFromListSwitch):
					if (!parser.openInput(argText)) return -1;
					while (!parser.eofOrBlank())
					{
						parser.readNextLine(LineParser::StripComments);
						nTried ++;
						if (modelFilter != NULL) filter = modelFilter;
						else filter = probeFile(parser.line(), FilterData::ModelImport);
						if (filter != NULL) filter->executeRead(parser.line());
						else return -1;
					}
					break;
				// Set type mappings
				case (Cli::MapSwitch):
					// Get the argument and parse it internally
					parser.getArgsDelim(0, argText);
					for (n=0; n<parser.nArgs(); n++)
					{
						items = parser.argc(n).split('=');
						if (items.count() != 2)
						{
							Messenger::print("Mangled map value found: '%s'.", qPrintable(parser.argc(n)));
							return -1;
						}
						el = Elements().z(items.at(1));
						if (el == 0)
						{
							Messenger::print("Unrecognised element '%s' in type map.", qPrintable(items.at(1)));
							return -1;
						}
						else Elements().addMapping(el, items.at(0));
					}
					break;
				// Create a new model
				case (Cli::NewModelSwitch):
					model = addModel();
					model->enableUndoRedo();
					break;
				// Display filter nicknames and quit
				case (Cli::NicknamesSwitch):
					for (int ftype=0; ftype<FilterData::nFilterTypes; ++ftype)
					{
						printValidNicknames( (FilterData::FilterType) ftype );
					}
					return -1;
					break;
				// Prohibit bonding calculation of atoms on load
				case (Cli::NoBondSwitch):
					prefs.setBondOnLoad(Choice::No);
					break;
				// Prohibit model centering on load (for non-periodic systems)
				case (Cli::NoCentreSwitch):
					prefs.setCentreOnLoad(Choice::No);
					break;
				// Prohibit folding (MIM'ing) of atoms in periodic systems on load
				case (Cli::NoFoldSwitch):
					prefs.setFoldOnLoad(Choice::No);
					break;
				// Force packing (application of symmetry operators) on load
				case (Cli::NoPackSwitch):
					prefs.setPackOnLoad(Choice::No);
					break;
				// Don't load Qt window/toolbar settings on startup
				case (Cli::NoQtSettingsSwitch):
					prefs.setLoadQtSettings(false);
					break;
				// Prohibit packing (application of symmetry operators) on load
				case (Cli::PackSwitch):
					prefs.setPackOnLoad(Choice::Yes);
					break;
				// Read and execute commads from pipe
				case (Cli::PipeSwitch):
					prefs.setReadPipe(true);
					break;
				// Enable processing mode
				case (Cli::ProcessSwitch):
					if ((programMode_ == Aten::ExportMode) || (programMode_ == Aten::BatchExportMode) || (programMode_ == Aten::BatchMode))
					{
						printf("Error: --process and --batch / --export options are mutually exclusive.");
						return -1;
					}
					else programMode_ = Aten::ProcessMode;
					break;
				// Load and run a script file
				case (Cli::ScriptSwitch):
					script = addScript();
					if (script->generateFromFile(argText, argText))
					{
						Messenger::print("Successfully loaded script.");
						programMode_ = Aten::CommandMode;
						if (!script->execute(rv))
						{
							Messenger::print("Script execution failed.");
							return -1;
						}
						// Need to check program mode after each script since it can be changed
						if (programMode_ == Aten::CommandMode) programMode_ = Aten::GuiMode;
					}
					else
					{
						removeScript(script);
						return -1;
					}
					break;
				// Associate trajectory with last loaded model
				case (Cli::TrajectorySwitch):
					// Check for a current model
					if (current_.m == NULL)
					{
						Messenger::print("There is no current model to associate a trajectory to.");
						return -1;
					}
					else
					{
						Tree* filter = (trajectoryFilter ? trajectoryFilter : probeFile(argText, FilterData::TrajectoryImport));
						if (filter == NULL) return -1;
						if (!current_.m->initialiseTrajectory(argText,filter)) return -1;
					}
					break;
				// Set forced trajectory load format
				case (Cli::TrajectoryFormatSwitch):
					trajectoryFilter = findFilter(FilterData::TrajectoryImport, argText);
					if (trajectoryFilter == NULL)
					{
						// Print list of valid filter nicknames
						printValidNicknames(FilterData::TrajectoryImport);
						return -1;
					}
					break;
				// Set maximum number of undolevels per model
				case (Cli::UndoLevelSwitch):
					prefs.setMaxUndoLevels(argText.toInt());
					break;
				// Set the type of element (Z) mapping to use in name conversion
				case (Cli::ZmapSwitch):
					zm = ElementMap::zMapType(argText, true);
					if (zm != ElementMap::nZMapTypes) prefs.setZMapType(zm);
					else return -1;
					break;
				// Undefined option
				default:
					Messenger::print("Shoddy programming alert - CLI option has not been implemented.");
					return -1;
			}
		}
		else
		{
			// Not a CLI switch, so try to load it as a model
			++nTried;
			if (!loadModel(argv[argn], modelFilter)) return -1;
		}
	}

	// Anything redirected to stdin (or forcibly piped)?
	bool readcin = false;
	if (prefs.readPipe()) readcin = true;
	else
	{
		std::cin.seekg(0, std::ios::end);
		std::streampos endpos = std::cin.tellg();
		if ((endpos != std::streampos(-1)) && (endpos != std::streampos(0))) readcin = true;
		std::cin.seekg(0, std::ios::beg);
		
	}
	if (readcin)
	{
		// Grab all lines from cin to a temporary stringlist....
		QStringList commands;
		char line[8096];
		QString s;
		while (std::cin.good() && (!std::cin.eof()))
		{
			std::cin.getline(line, 8095);
			s = line;
			// Remove any commented part of line
// 			removeComments(line); // ATEN2 TODO Check to see if this works when commented out
			if (s.isEmpty()) continue;
			commands << s;
		}
		// Create and execute commands
		if ((programMode_ == Aten::BatchMode) || (programMode_ == Aten::ProcessMode) || (programMode_ == Aten::BatchExportMode))
		{
			script = addBatchCommand();
			if (!script->generateFromStringList(commands, "BatchCommand", "Batch Command")) return -1;
		}
		else
		{
			tempProgram.clear();
			if (tempProgram.generateFromStringList(commands, "CLI Command (cin)", "CLI Command from STDIN"))
			{
				if (!tempProgram.execute(rv)) return -1;
			}
			else return -1;
		}
	}

	return 1;
}

// Usage help
void Aten::printUsage() const
{
	Messenger::print("Usage: aten [options] [<model> ...]");
	Messenger::print("Program Options:");
	for (int n=0; n<Cli::nSwitchItems; n++)
	{
		if (cliSwitches[n].argument == 0)
		{
			if (cliSwitches[n].shortOpt != '\0') Messenger::print("\t-%c, --%s", cliSwitches[n].shortOpt, cliSwitches[n].longOpt);
			else Messenger::print("\t--%s", cliSwitches[n].longOpt);
		}
		else
		{
			if (cliSwitches[n].shortOpt != '\0') Messenger::print("\t-%c %s, --%s %s", cliSwitches[n].shortOpt, cliSwitches[n].argText, cliSwitches[n].longOpt, cliSwitches[n].argText);
			else Messenger::print("\t--%s %s", cliSwitches[n].longOpt, cliSwitches[n].argText);
		}
		Messenger::print("\t\t%s",cliSwitches[n].description);
	}
}

// Add passed value
bool Aten::addPassedValue(VTypes::DataType type, QString name, QString value)
{
	// Search for existing passed value of this name...
	if (passedValues_.find(name)) return false;
	Variable* var = NULL;
	if (type == VTypes::IntegerData) var = new IntegerVariable(value.toInt(), true);
	else if (type == VTypes::DoubleData) var = new DoubleVariable(value.toDouble(), true);
	else if (type == VTypes::StringData) var = new StringVariable(value, true);
	else Messenger::error("Internal Error: Don't know how to create a passed value of type '%s'.", VTypes::dataType(type));
	var->setName(name);
	passedValues_.take(var, true);
	return true;
}

// Find passed value
Variable* Aten::findPassedValue(QString name) const
{
	return passedValues_.find(name);
}
