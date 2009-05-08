/*
	*** Aten Main
	*** src/main.cpp
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

#include "main/aten.h"
#include "gui/gui.h"
#include "base/messenger.h"
#include <time.h>
#include <ctime>
#include <iostream>

int main(int argc, char *argv[])
{
	// Parse early command-line options
	if (!aten.parseCliEarly(argc, argv)) return -1;

	// Print GPL license information
	msg.print(Messenger::Verbose, "Aten version %s, Copyright (C) 2007-2009  T. Youngs.\n", ATENVERSION);
	msg.print(Messenger::Verbose, "Built from %s@%s.\n", ATENURL, ATENREVISION);
	msg.print(Messenger::Verbose, "Aten comes with ABSOLUTELY NO WARRANTY.\n");
	msg.print(Messenger::Verbose, "This is free software, and you are welcome to redistribute it under certain conditions.\n");
	msg.print(Messenger::Verbose, "For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	srand( (unsigned)time( NULL ) );
	//printf("Atom Type is currently %lu bytes.\n",sizeof(atom));

	// Get environment variables
	aten.setHomeDir( getenv("HOME") == '\0' ? getenv("HOMEPATH") : getenv("HOME") );
	aten.setWorkDir(getenv("PWD"));
	if (!aten.dataDirSet()) aten.setDataDir(getenv("ATENDATA"));
	msg.print(Messenger::Verbose, "Home directory is %s, working directory is %s, data directory is %s.\n", aten.homeDir(), aten.workDir(), aten.dataDir());

	// Initialise QApplication
	gui.initialise(argc, argv);

	// Read in file filters (if unsuccessful, a messagebox will be raised in the GUI)
	aten.openFilters();

	// Load in user preferences
	char filename[512];
	sprintf(filename, "%s%s", aten.homeDir(), "/.aten/prefs.dat");
	if (!prefs.load(filename)) return -1;

	// Parse program arguments - return value is how many models were loaded, or -1 for some kind of failure
	if (aten.parseCli(argc,argv) == -1) return -1;

	// Enter the correct program mode
	switch (aten.programMode())
	{
		case (Aten::GuiMode):
			gui.run();
			break;
		case (Aten::BatchExportMode):
			aten.exportModels();
			break;
		case (Aten::BatchProcessMode):
			aten.processModels();
			aten.saveModels();
			break;
		case (Aten::ProcessAndExportMode):
			aten.processModels();
			aten.exportModels();
			break;
	}
	// Done.
	return 0;
}

