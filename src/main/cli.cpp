/*
	*** Command-line option parsing
	*** src/main/cli.cpp
	Copyright T. Youngs 2007-2010

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
#include "main/version.h"
#include "base/sysfunc.h"
#include "base/dnchar.h"
#include "base/constants.h"
#include <string.h>
#include <ctype.h>
#include <iostream>
#include <readline/readline.h>
#include <readline/history.h>
#include "parser/tree.h"

// Definitions of possible CLI options (id,keyword,arg(0=none,1=req,2=opt),argtext,description)
Cli cliSwitches[] = {
	{ Cli::DummySwitch,		'd'," _dummy",		2,
		"[output]",
		"Print out call debug information, or specific information if output type is supplied" },
	{ Cli::HelpSwitch,		'h',"help",		0,
		"",
		"Print this help" }
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

// Parse CLI options, after filters / prefs have been loaded
int Cli::parse(int argc, char *argv[])
{
	int argn, opt, ntried = 0, n, el, linelen;
	bool isShort, hasArg;
	char *line, s[4096];
	Dnchar arg, argtext, varname, varvalue, prompt;
	// Cycle over program arguments and available CLI options (skip [0] which is the binary name)
	argn = 0;
	while (argn < (argc-1))
	{
		argn++;
		// Check for null argument
		if (argv[argn][0] == '\0') continue;
		// Check for a CLI argument (presence of '-')
		// It's possible that this is the start of the expression (with a negative number) rather than a CLI switch....
		if ((argv[argn][0] == '-') && (!isdigit(argv[argn][1])) && (argv[argn][1] != '.'))
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
						if (isShort) printf(" '-%c' requires an argument.\n", cliSwitches[opt].shortOpt);
						else printf(" '--%s' requires an argument.\n", cliSwitches[opt].longOpt);
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
				// Turn on debug messages for calls (or specified output)
				case (Cli::DummySwitch):
// 					if (!hasArg) msg.addOutputType(Messenger::Calls);
// 					else
// 					{
// 						ot = Messenger::outputType(argtext.get(), TRUE);
// 						if (ot != Messenger::nOutputTypes) msg.addOutputType(ot);
// 						else return FALSE;
// 					}
					break;
				// Print help
				case (Cli::HelpSwitch):
					printf("Usage: ac [options] <expr>\n");
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
					break;
				// Undefined option
				default:
					printf("Shoddy programming alert - CLI option has not been implemented.\n");
					return -1;
			}
		}
		else
		{
// 			// Not a CLI switch, so might be the expression!
			Tree expression("_EXPRESSION", argv[argn]);
			ReturnValue rv;
			expression.execute(rv);
			if (rv.type() != VTypes::NoData) printf("%s\n", rv.asString());
 			else return -1;
		}
	}
	return 1;
}
