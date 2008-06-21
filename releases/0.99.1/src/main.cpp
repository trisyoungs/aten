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
#include <iostream>
#include "parse/parser.h"
#include "model/model.h"
#include "command/commandlist.h"
#include "base/master.h"
#include "gui/gui.h"

int main(int argc, char *argv[])
{
	// Print GPL license information
	printf("Aten version %s, Copyright (C) 2007,2008  T. Youngs.\n", ATENVERSION);
	printf("Built from %s@%s.\n", ATENURL, ATENREVISION);
	printf("Aten comes with ABSOLUTELY NO WARRANTY.\n");
	printf("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	printf("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	srand( (unsigned)time( NULL ) );
	//printf("Atom Type is currently %lu bytes.\n",sizeof(atom));

	// Get environment variables
	master.setHomeDir(getenv("HOME"));
	master.setWorkDir(getenv("PWD"));
	printf("Home directory is %s, working directory is %s.\n", master.homeDir(), master.workDir());
	master.setDataDir(getenv("ATENDATA"));

	// Initialise QApplication
	gui.initialise(argc, argv);

	// Read in file filters
	if (!master.openFilters()) return 1;

	// Load in user preferences
	char filename[256];
	sprintf(filename,"%s%s",master.homeDir(),"/.aten/prefs.dat");
	prefs.load(filename);

	// Parse program arguments - return value is how many models were loaded, or -1 for some kind of failure
	if (master.parseCli(argc,argv) == -1) return -1;

	// Enter full GUI 
	if (master.programMode() == Master::GuiMode)
	{
		// Add empty model if none were specified on the command line
		if (master.nModels() == 0) Model *m = master.addModel();
		gui.run();
	}

	// Done.
	return 0;
}

