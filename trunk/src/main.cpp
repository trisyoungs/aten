/*
	*** Aten Main
	*** src/main.cpp
	Copyright T. Youngs 2007-2015

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
#include "main/version.h"
#include "gui/mainwindow.h"
#include "base/messenger.h"
#include "math/mathfunc.h"
#include <time.h>
#include <ctime>
#include <iostream>

int main(int argc, char *argv[])
{
	// Create main Aten object
	Aten aten;

	/* Parse early command-line options */
	if (!aten.parseCliEarly(argc, argv)) return -1;

	/* Print GPL license information */
	msg.print(Messenger::Verbose, "Aten version %s, Copyright (C) 2007-2010  T. Youngs.\n", ATENVERSION);
	msg.print(Messenger::Verbose, "SVN repository is %s.\n", ATENURL);
	msg.print(Messenger::Verbose, "Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	msg.print(Messenger::Verbose, "Aten comes with ABSOLUTELY NO WARRANTY.\n");
	msg.print(Messenger::Verbose, "This is free software, and you are welcome to redistribute it under certain conditions.\n");
	msg.print(Messenger::Verbose, "For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	/* Set random seed */
	srand( (unsigned)time( NULL ) );

	/* Get environment variables */
	if (getenv("HOME") != '\0') aten.setHomeDir(getenv("HOME"));
	else aten.setHomeDir( getenv("USERPROFILE") );
	aten.setWorkDir(getenv("PWD"));
	if (!aten.dataDirSet()) aten.setDataDir(getenv("ATENDATA"));
	msg.print(Messenger::Verbose, "Home directory is %s, working directory is %s, data directory is %s.\n", aten.homeDir(), aten.workDir(), aten.dataDir());

	/* Create the main QApplication */
	QApplication app(argc, argv, QApplication::GuiClient);
	QCoreApplication::setOrganizationName("ProjectAten");
	QCoreApplication::setOrganizationDomain("www.projectaten.net");
	QCoreApplication::setApplicationName("Aten");

	#if QT_VERSION >= 0x040600
	QGL::setPreferredPaintEngine(QPaintEngine::OpenGL);
	#endif

	/* Tweak the default QGLFormat */
	QGLFormat::defaultFormat().setSampleBuffers(true);

	/* Create the main window */
	AtenForm mainWindow;

	/* Reconstruct combination rule functions */
	Combine::regenerateEquations();

	/* Read in includes (if unsuccessful, a messagebox will be raised in the GUI) */
	if (prefs.loadIncludes()) aten.openIncludes();

	/* Read in file filters (if unsuccessful, a messagebox will be raised in the GUI) */
	/* This will also set dataDir_ to a valid value (provided one could be found in a default search location) */
	if (prefs.loadFilters()) aten.openFilters();
	
	/* Load in fragments */
	if (prefs.loadFragments()) aten.openFragments();
	
	/* Load in partitions */
	if (prefs.loadPartitions()) aten.openPartitions();
	
	/* Load in program and user preferences */
	if (!prefs.load()) return -1;
	
	/* Parse program arguments - return value is how many models were loaded, or -1 for some kind of failure */
	if (aten.parseCli(argc,argv) == -1) return -1;

	/* Enter the correct program mode */
	int result = 0;
	switch (aten.programMode())
	{
		case (Aten::GuiMode):
			/* Show the main window */
			mainWindow.show();

			/* Enter Qt's main events loop */
			result =  app.exec();
			break;
		case (Aten::BatchMode):
			msg.print("Batch mode in effect - models will be processed and saved.\n");
			aten.processModels();
			aten.saveModels();
			break;
		case (Aten::ExportMode):
			msg.print("Export mode in effect - models will be exported to the specified format.\n");
			aten.exportModels();
			break;
		case (Aten::ProcessMode):
			msg.print("Process mode in effect - models will be processed and loaded in the GUI.\n");
			aten.processModels();
			gui.run();
			break;
		case (Aten::BatchExportMode):
			msg.print("BatchExport mode in effect - models will be processed and exported to the specified format.\n");
			aten.processModels();
			aten.exportModels();
			break;
		default:
			break;
	}

	/* Done. */
	return 0;
}

