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
#include <QtGui/QApplication>

ATEN_USING_NAMESPACE

// External Object Declarationsi
int main(int argc, char* argv[])
{
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

	/* Create main Aten object before anything else, since this sets pointers in other dependent static objects */
	Aten MrAten;

	/* Parse early command-line options */
	if (!MrAten.parseCliEarly(argc, argv)) return -1;

	/* Print GPL license information */
	Messenger::print("Aten version %s, Copyright (C) 2007-2015 T. Youngs.\n", ATENVERSION);
	Messenger::print("SVN repository is %s.\n", ATENURL);
	Messenger::print("Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	Messenger::print("Aten comes with ABSOLUTELY NO WARRANTY.\n");
	Messenger::print("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	Messenger::print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	/* Set random seed */
	srand( (unsigned) time(NULL) );

	/* Get environment variables */
	if (getenv("HOME") != '\0') MrAten.setHomeDir(getenv("HOME"));
	else MrAten.setHomeDir( getenv("USERPROFILE") );
	MrAten.setWorkDir(getenv("PWD"));
	if (!MrAten.dataDirSet()) MrAten.setDataDir(getenv("ATENDATA"));
	Messenger::print(Messenger::Verbose, "Home directory is %s, working directory is %s, data directory is %s.\n", MrAten.homeDir(), MrAten.workDir(), MrAten.dataDir());

	/* Read in includes (if unsuccessful, a messagebox will be raised in the GUI) */
	if (prefs.loadIncludes()) MrAten.openIncludes();

	/* Read in file filters (if unsuccessful, a messagebox will be raised in the GUI) */
	/* This will also set dataDir_ to a valid value (provided one could be found in a default search location) */
	if (prefs.loadFilters()) MrAten.openFilters();
	
	/* Load in fragments */
	if (prefs.loadFragments()) MrAten.openFragments();
	
	/* Load in partitions */
	if (prefs.loadPartitions()) MrAten.openPartitions();

	/* Create the main window */
	AtenWindow mainWindow(MrAten);

	/* Load in program and user preferences */
	if (!MrAten.loadPrefs()) return -1;

	/* Reconstruct combination rule functions */
	MrAten.combinationRules().regenerateEquations();
	
	/* Parse program arguments - return value is how many models were loaded, or -1 for some kind of failure */
	if (MrAten.parseCli(argc,argv) == -1) return -1;
	
	/* Enter the correct program mode */
	int result = 0;
	switch (MrAten.programMode())
	{
		case (Aten::GuiMode):
			/* Show the main window */
			mainWindow.show();

			/* Enter Qt's main events loop */
			result =  app.exec();
			break;
		case (Aten::BatchMode):
			Messenger::print("Batch mode in effect - models will be processed and saved.\n");
			MrAten.processModels();
			MrAten.saveModels();
			break;
		case (Aten::ExportMode):
			Messenger::print("Export mode in effect - models will be exported to the specified format.\n");
			MrAten.exportModels();
			break;
		case (Aten::ProcessMode):
			Messenger::print("Process mode in effect - models will be processed and loaded in the GUI.\n");
			MrAten.processModels();
			/* Show the main window */
			mainWindow.show();

			/* Enter Qt's main events loop */
			result =  app.exec();
			break;
		case (Aten::BatchExportMode):
			Messenger::print("BatchExport mode in effect - models will be processed and exported to the specified format.\n");
			MrAten.processModels();
			MrAten.exportModels();
			break;
		default:
			break;
	}

	/* Done. */
	return 0;
}

