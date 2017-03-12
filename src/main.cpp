/*
	*** Aten Main
	*** src/main.cpp
	Copyright T. Youngs 2007-2017

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
#include <QApplication>
#include <QMessageBox>
#include <QResource>
#include <time.h>
#include <QtGlobal>

ATEN_USING_NAMESPACE

int main(int argc, char* argv[])
{
	/* Set some Qt environment variables */
	// -- Correct rendering on systems that use scaled displays
#if QT_VERSION <= QT_VERSION_CHECK(5,5,0)
	qputenv("QT_DEVICE_PIXEL_RATIO", "auto"); // Now defunct in 5.6
#endif

	/* Create the main QApplication */
	QApplication app(argc, argv);
	QCoreApplication::setOrganizationName("ProjectAten");
	QCoreApplication::setOrganizationDomain("www.projectaten.com");
	QCoreApplication::setApplicationName("Aten");
	// -- Set native siblings attribute to prevent odd rendering artefacts on some systems
	app.setAttribute(Qt::AA_DontCreateNativeWidgetSiblings);
	// -- Set high DPI pixmaps
	app.setAttribute(Qt::AA_UseHighDpiPixmaps, true);

	/* Ensure that the C locale is set, otherwise printf() and friends may not use dot for the radix point */
	setlocale(LC_NUMERIC,"C");

	/* Tweak the default QSurfaceFormat */
	QSurfaceFormat surfaceFormat;
	surfaceFormat.setSamples(2);
	QSurfaceFormat::setDefaultFormat(surfaceFormat);

	/* Create main Aten object before anything else, since this sets pointers in other dependent static objects */
	Aten MrAten;

	/* Parse early command-line options */
	if (!MrAten.parseCliEarly(argc, argv)) return -1;

	/* Print GPL license information */
	Messenger::print("Aten version %s, Copyright (C) 2007-2016 T. Youngs.", ATENVERSION);
	Messenger::print("Git repository is <https://github.com/trisyoungs/aten.git>.");
	Messenger::print("Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.");
	Messenger::print("Aten comes with ABSOLUTELY NO WARRANTY.");
	Messenger::print("This is free software, and you are welcome to redistribute it under certain conditions.");
	Messenger::print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.");

	/* Set random seed */
	srand( (unsigned) time(NULL) );

	/* Find/set directory locations */
	MrAten.setDirectories();
	Messenger::print(Messenger::Verbose, "Home directory is %s, working directory is %s, data directory is %s.", qPrintable(MrAten.homeDir().path()), qPrintable(MrAten.workDir().path()), qPrintable(MrAten.dataDir().path()));
	
	/* Create the main window */
	AtenWindow mainWindow(MrAten);

	/* Set AtenWindow pointer in MrAten */
	MrAten.setAtenWindow(&mainWindow);

	/* Load plugins */
	if (prefs.loadPlugins()) MrAten.loadPlugins();
	
	/* Load includes */
	if (prefs.loadIncludes()) MrAten.loadIncludes();

	/* Load fragments */
	if (prefs.loadFragments()) MrAten.loadFragments();
	
	/* Load partitions */
	if (prefs.loadPartitions()) MrAten.loadPartitions();

	/* Load encoder definitions */
	MrAten.loadEncoderDefinitions();

	/* Load in program and user preferences */
	if (!MrAten.loadPrefs()) return -1;

	/* Parse program arguments - returns -1 for failure, 0 for quit, or 1 for success */
	if (MrAten.parseCli(argc,argv) != 1) return -1;

	/* Enter the correct program mode */
	int result = 0;
	switch (MrAten.programMode())
	{
		case (Aten::GuiMode):
			/* If no model is loaded, add one */
			if (MrAten.nModels() == 0)
			{
				Model* m = MrAten.addModel();
				m->enableUndoRedo();
			}

			/* Turn on filter dialogs */
			prefs.setAllowDialogs(true);

			/* Show the main window */
			mainWindow.initialUpdateAndShow();

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

