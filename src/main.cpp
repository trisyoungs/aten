/*
	*** Aten Main
	*** src/main.cpp
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

#include <time.h>
#include <ctime>
#include <string>
#include <iostream>
#include "parse/parser.h"
#include "model/model.h"
#include "command/commandlist.h"
#include "base/master.h"
#include "gui/gui.h"

int main(int argc, char *argv[])
{
	// Print GPL license information
	printf("Aten version %s, Copyright (C) 2007,2008  T. Youngs\n", PACKAGE_VERSION);
	printf("Aten comes with ABSOLUTELY NO WARRANTY.\n");
	printf("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	printf("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	// Prepare command line to act on debug options
	master.debugCli(argc, argv);

	srand( (unsigned)time( NULL ) );
	//printf("Atom Type is currently %lu bytes.\n",sizeof(atom));

	// Get environment variables
	master.homeDir = getenv("HOME");
	master.workDir = getenv("PWD");
	master.dataDir = getenv("ATENDATA");
	if (master.dataDir.empty())
	{
		printf("$ATENDATA has not been set.\n");
		printf("It should point to the (installed) location of the 'data' directory.\n");
		printf("e.g. (in bash) 'export ATENDATA=/usr/local/share/aten/'.\n");
		return 1;
	}
	printf("Home directory is %s, working directory is %s.\n", master.homeDir.get(), master.workDir.get());

	char filename[256];
	// Read default filters from data directory (pass directory)
	sprintf(filename,"%s%s",master.dataDir.get(),"/filters/");
	if (!master.openFilters(filename,TRUE)) return 1;

	// Read user filters from home directory (pass directory)
	sprintf(filename,"%s%s",master.homeDir.get(),"/.aten/filters/");
	master.openFilters(filename,FALSE);

	// Load in user preferences
	sprintf(filename,"%s%s",master.homeDir.get(),"/.aten/prefs.dat");
	prefs.load(filename);

	// Parse program arguments - return value is how many models were loaded, or -1 for some kind of failure
	if (master.parseCli(argc,argv) == -1) return -1;

	// Do various things depending on the program mode that has been set
	// Execute scripts / commands if they were provided
	if (master.programMode() == Master::CommandMode)
	{
		// Commands first
		for (CommandList *cl = master.commands.first(); cl != NULL; cl = cl->next)
		{
			if (!cl->execute(NULL)) master.setProgramMode(Master::NoMode);
			// Need to check program mode after each script since it can be changed
			if (master.programMode() != Master::CommandMode) break;
		}
		// Now scripts
		for (CommandList *cl = master.scripts.first(); cl != NULL; cl = cl->next)
		{
			if (!cl->execute(NULL)) master.setProgramMode(Master::NoMode);
			// Need to check program mode after each script since it can be changed
			if (master.programMode() != Master::CommandMode) break;
		}
		// All scripts done - set program mode to PM_GUI if it is still PM_COMMAND
		if (master.programMode() == Master::CommandMode) master.setProgramMode(Master::GuiMode);
	}
	// Enter interactive mode once any commands/scripts have been executed
	if (master.programMode() == Master::InteractiveMode)
	{
		std::string cmd;
		printf("Entering interactive mode...\n");
		do
		{
			// Get string from user
			printf(">>> ");
			getline(cin,cmd);
			master.interactiveScript.clear();
			master.interactiveScript.cacheLine(cmd.c_str());
			master.interactiveScript.execute();
		} while (master.programMode() == Master::InteractiveMode);
		//master.set_program_mode(PM_NONE);
	}
	// Enter full GUI 
	if (master.programMode() == Master::GuiMode)
	{
		// Add empty model if none were specified on the command line
		if (master.nModels() == 0) Model *m = master.addModel();
		gui.run(argc,argv);
	}

	// Done.
	return 0;
}

