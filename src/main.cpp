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
#include "render/fontinstance.h"
#include <QApplication>
#include <QMessageBox>
#include <time.h>

ATEN_USING_NAMESPACE

int main(int argc, char* argv[])
{
	/* Create the main QApplication */
	QApplication app(argc, argv);
	QCoreApplication::setOrganizationName("ProjectAten");
	QCoreApplication::setOrganizationDomain("www.projectaten.net");
	QCoreApplication::setApplicationName("Aten");

	/* Tweak the default QSurfaceFormat */
	QSurfaceFormat surfaceFormat;
	surfaceFormat.setSamples(2);
	QSurfaceFormat::setDefaultFormat(surfaceFormat);

	/* Create main Aten object before anything else, since this sets pointers in other dependent static objects */
	Aten MrAten;

	/* Parse early command-line options */
	if (!MrAten.parseCliEarly(argc, argv)) return -1;

	/* Print GPL license information */
	Messenger::print("Aten version %s, Copyright (C) 2007-2015 T. Youngs.", ATENVERSION);
	Messenger::print("Git repository is <https://github.com/trisyoungs/aten.git>.");
	Messenger::print("Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.");
	Messenger::print("Aten comes with ABSOLUTELY NO WARRANTY.");
	Messenger::print("This is free software, and you are welcome to redistribute it under certain conditions.");
	Messenger::print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.");

	/* Set random seed */
	srand( (unsigned) time(NULL) );

	/* Create the main window */
	AtenWindow mainWindow(MrAten);

	/* Set AtenWindow pointer in MrAten */
	MrAten.setAtenWindow(&mainWindow);

	/* Find/set directory locations */
	MrAten.setDirectories();
	Messenger::print(Messenger::Verbose, "Home directory is %s, working directory is %s, data directory is %s.", qPrintable(MrAten.homeDir().path()), qPrintable(MrAten.workDir().path()), qPrintable(MrAten.dataDir().path()));

	/* Read in includes (if unsuccessful, a messagebox will be raised in the GUI) */
	if (prefs.loadIncludes()) MrAten.openIncludes();

	/* Read in file filters (if unsuccessful, a messagebox will be raised in the GUI) */
	if (prefs.loadFilters()) MrAten.openFilters();

	/* Load in fragments */
	if (prefs.loadFragments()) MrAten.openFragments();
	
	/* Load in partitions */
	if (prefs.loadPartitions()) MrAten.openPartitions();

	/* Load in program and user preferences */
	if (!MrAten.loadPrefs()) return -1;

	/* Reconstruct combination rule functions */
	MrAten.combinationRules().regenerateEquations();
	
	/* Parse program arguments - returns -1 for failure, 0 for quit, or 1 for success */
	if (MrAten.parseCli(argc,argv) != 1) return -1;

	/* Load font */
	QString fontFile;
	if (QDir::isRelativePath(prefs.viewerFontFileName())) fontFile = MrAten.dataDirectoryFile(prefs.viewerFontFileName());
	else fontFile = prefs.viewerFontFileName();
	if (!QFile::exists(fontFile)) QMessageBox::warning(0, "Font Error", "The specified font file '" + fontFile + "' does not exist.");
	else if (!FontInstance::setupFont(fontFile)) QMessageBox::warning(0, "Font Error", "Failed to create a font from the specified font file '" + fontFile +"'.");

	/* Enter the correct program mode */
	int result = 0;
	switch (MrAten.programMode())
	{
		case (Aten::GuiMode):
			// If no model loaded, add one
			if (MrAten.nModels() == 0)
			{
				Model* m = MrAten.addModel();
				m->enableUndoRedo();
			}

			/* Show the main window */
			mainWindow.initialUpdateAndShow();

			// Tell Messenger to stop printing to stdout
			Messenger::setPrintToConsole(false);

			/* Enter Qt's main events loop */
			result =  app.exec();
			break;
		case (Aten::BatchMode):
			Messenger::print("Batch mode in effect - models will be processed and saved.");
			MrAten.processModels();
			MrAten.saveModels();
			break;
		case (Aten::ExportMode):
			Messenger::print("Export mode in effect - models will be exported to the specified format.");
			MrAten.exportModels();
			break;
		case (Aten::ProcessMode):
			Messenger::print("Process mode in effect - models will be processed and loaded in the GUI.");
			MrAten.processModels();

			/* Show the main window */
			mainWindow.initialUpdateAndShow();

			// Tell Messenger to stop printing to stdout
			Messenger::setPrintToConsole(false);

			/* Enter Qt's main events loop */
			result =  app.exec();
			break;
		case (Aten::BatchExportMode):
			Messenger::print("BatchExport mode in effect - models will be processed and exported to the specified format.");
			MrAten.processModels();
			MrAten.exportModels();
			break;
		default:
			break;
	}

	/* Done. */
	return 0;
}

