/*
	*** GUI Settings Load/Save
	*** src/gui/settings.cpp
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

#include "base/sysfunc.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include <QSettings>
#include <QFileInfo>

// Load Qt Settings
void AtenWindow::loadSettings()
{
	QString key;
	QFileInfo fi1, fi2;
	QString filename;
	QStringList commandHistory, selectHistory;
	Program* prog, *loadedscript;
	int n;
	bool collapsed;
	QSettings settings;

	// Window geometry / position
	if (prefs.loadQtSettings())
	{
		if (settings.contains("MainWinPositions")) restoreState( settings.value("MainWinPositions").toByteArray());
		if (settings.contains("MainWinGeometries")) restoreGeometry( settings.value("MainWinGeometries").toByteArray());
		if (settings.contains("MainWinSize")) resize(settings.value("mainwin_size", QSize(400, 400)).toSize());
		if (settings.contains("MainWinPosition")) move(settings.value("mainwin_pos", QPoint(200, 200)).toPoint());

		// Model list and atom table collapsed status
		collapsed = (settings.contains("AtomsTableCollapsed") ? settings.value("AtomsTableCollapsed").toBool() : true);
		ui.AtomsTableToggleButton->setChecked(!collapsed);
		ui.AtomsTableWidget->setVisible(!collapsed);
		collapsed = (settings.contains("ModelsListCollapsed") ? settings.value("ModelsListCollapsed").toBool() : true);
		ui.ModelsListToggleButton->setChecked(!collapsed);
		ui.ModelsListWidget->setVisible(!collapsed);

		// Recent files
		ReturnValue maxRecentFiles, recentFile;
		// -- Models
		if (ui.HomeFileOpenButton->callPopupMethod("maxRecentFiles", maxRecentFiles))
		{
			settings.beginGroup("RecentModels");
			for (n = 0; n < maxRecentFiles.asInteger(); ++n)
			{
				if (settings.contains(QString::number(n)))
				{
					recentFile = settings.value(QString::number(n)).toString();
					ui.HomeFileOpenButton->callPopupMethod("addRecentFile", recentFile);
				}
			}
			settings.endGroup();
		}
		// -- Forcefields
		if (ui.ForcefieldsManageOpenButton->callPopupMethod("maxRecentFiles", maxRecentFiles))
		{
			settings.beginGroup("RecentForcefields");
			for (n = 0; n < maxRecentFiles.asInteger(); ++n)
			{
				if (settings.contains(QString::number(n)))
				{
					recentFile = settings.value(QString::number(n)).toString();
					ui.HomeFileOpenButton->callPopupMethod("addRecentFile", recentFile);
				}
			}
			settings.endGroup();
		}
		// -- Grids
		if (ui.GridsManageOpenButton->callPopupMethod("maxRecentFiles", maxRecentFiles))
		{
			settings.beginGroup("RecentGrids");
			for (n = 0; n < maxRecentFiles.asInteger(); ++n)
			{
				if (settings.contains(QString::number(n)))
				{
					recentFile = settings.value(QString::number(n)).toString();
					ui.HomeFileOpenButton->callPopupMethod("addRecentFile", recentFile);
				}
			}
			settings.endGroup();
		}
		// -- Scripts
		if (ui.ToolsScriptsOpenButton->callPopupMethod("maxRecentFiles", maxRecentFiles))
		{
			settings.beginGroup("RecentScripts");
			for (n = 0; n < maxRecentFiles.asInteger(); ++n)
			{
				if (settings.contains(QString::number(n)))
				{
					recentFile = settings.value(QString::number(n)).toString();
					ui.HomeFileOpenButton->callPopupMethod("addRecentFile", recentFile);
				}
			}
			settings.endGroup();
		}
		// -- Favourite Places
		FileSelectorWidget::clearFavourites();
		if (settings.contains("FavouritePlaces"))
		{
			settings.beginGroup("FavouritePlaces");
			n = 0;
			while (settings.contains(QString::number(n)))
			{
				FileSelectorWidget::addFavourite(settings.value(QString::number(n++)).toString());
			}
			settings.endGroup();
		}
		else
		{
			// Add on default favourites
			FileSelectorWidget::addFavourite(QDir::rootPath());
			FileSelectorWidget::addFavourite(QDir::homePath());
			FileSelectorWidget::addFavourite(aten_.dataDir().absolutePath());
		}
	}

	// Check for old command history information, read it, and then remove it
	n = 0;
	do
	{
		key = "CommandHistory";
		key += QString::number(n);
		if (settings.contains(key))
		{
			commandHistory << settings.value(key).toString();
			settings.remove(key);
		}
		++n;
	} while (settings.contains(key));
	settings.sync();

	// Probe user history file...
	filename = aten_.atenDirectoryFile("history.txt");
	Messenger::print("Looking for program history file '%s'...", qPrintable(filename));
	QFileInfo fileInfo(filename);
	if (fileInfo.exists())
	{
		Messenger::print("Program history file found in '%s'", qPrintable(filename));
		LineParser parser;
		QString arg, data;
		if (!parser.openInput(filename))
		{
			Messenger::print("Failed to open program history file.");
			return;
		}
		while (!parser.eofOrBlank())
		{
			// Get first part of line - the identifier for the information which follows it
			if (parser.readNextLine(Parser::SkipBlanks) == 1) break;
			if (!parser.getArgDelim(Parser::SkipBlanks, arg)) break;

			// Get remainder of line, which is the data to store
			if (!parser.getRestDelim(data)) break;

			// Determine history type
			Prefs::HistoryType ht = Prefs::historyType(arg, true);
			if (ht == Prefs::nHistoryTypes) continue;
			switch (ht)
			{
				case (Prefs::CommandHistory):
					commandHistory << data;
					break;
				case (Prefs::ScriptHistory):
					// Has this script already been loaded?
					// Use a couple of QFileInfos to find out...
					fi1.setFile(data);
					for (loadedscript = aten_.scripts(); loadedscript != NULL; loadedscript = loadedscript->next)
					{
						fi2.setFile(loadedscript->filename());
						if (fi1.canonicalFilePath() == fi2.canonicalFilePath()) break;
					}
					if (loadedscript != NULL)
					{
						printf("Script '%s' appears to have been loaded already - will not load it a second time.\n", qPrintable(data));
					}
					else
					{
						prog = aten_.addScript();
						if (prog->generateFromFile(data, data)) Messenger::print("Successfully loaded script file '%s'.", qPrintable(data));
						else
						{
							aten_.removeScript(prog);
							Messenger::print("Failed to load script file '%s'.", qPrintable(data));
						}
					}
					break;
				case (Prefs::SelectHistory):
					selectHistory << data;
					break;
				default:
					break;
			}
		}
		parser.closeFiles();
	}
	else Messenger::print("Program history file not found.");
	
	// Update GUI controls
	ui.QuickCommandCombo->addItems(commandHistory);
	ui.QuickCommandCombo->setCurrentIndex(-1);
	ui.SelectIntelligentTargetCombo->addItems(selectHistory);
	ui.SelectIntelligentTargetCombo->setCurrentIndex(-1);
}

// Save Qt settings
void AtenWindow::saveSettings()
{
	// Open new file for writing...
	QString filename = aten_.atenDirectoryFile("history.txt");
	Messenger::print("Saving program history file '%s'...", qPrintable(filename));

	// GUI Settings
	ReturnValue nItems, recentFile;
	QSettings settings;
	// -- Recent Models
	settings.remove("RecentModels");
	if (ui.HomeFileOpenButton->callPopupMethod("nRecentFiles", nItems))
	{
		int count = 0;
		settings.beginGroup("RecentModels");
		for (int n = 0; n < nItems.asInteger(); ++n)
		{
			if (!ui.HomeFileOpenButton->callPopupMethod("recentFile", recentFile = n)) continue;
			settings.setValue(QString::number(count++), recentFile.asString());
		}
		settings.endGroup();
	}
	// -- Recent Forcefields
	settings.remove("RecentForcefields");
	if (ui.ForcefieldsManageOpenButton->callPopupMethod("nRecentFiles", nItems))
	{
		int count = 0;
		settings.beginGroup("RecentForcefields");
		for (int n = 0; n < nItems.asInteger(); ++n)
		{
			if (!ui.ForcefieldsManageOpenButton->callPopupMethod("recentFile", recentFile = n)) continue;
			settings.setValue(QString::number(count++), recentFile.asString());
		}
		settings.endGroup();
	}
	// -- Recent Grids
	settings.remove("RecentGrids");
	if (ui.GridsManageOpenButton->callPopupMethod("nRecentFiles", nItems))
	{
		int count = 0;
		settings.beginGroup("RecentGrids");
		for (int n = 0; n < nItems.asInteger(); ++n)
		{
			if (!ui.GridsManageOpenButton->callPopupMethod("recentFile", recentFile = n)) continue;
			settings.setValue(QString::number(count++), recentFile.asString());
		}
		settings.endGroup();
	}
	// -- Recent Scripts
	settings.remove("RecentScripts");
	if (ui.ToolsScriptsOpenButton->callPopupMethod("nRecentFiles", nItems))
	{
		int count = 0;
		settings.beginGroup("RecentScripts");
		for (int n = 0; n < nItems.asInteger(); ++n)
		{
			if (!ui.ToolsScriptsOpenButton->callPopupMethod("recentFile", recentFile = n)) continue;
			settings.setValue(QString::number(count++), recentFile.asString());
		}
		settings.endGroup();
	}
	// -- FileSelectorWidget Favourite Places
	settings.remove("FavouritePlaces");
	settings.beginGroup("FavouritePlaces");
	for (int n = 0; n < FileSelectorWidget::favourites().count(); ++n)
	{
		settings.setValue(QString::number(n), FileSelectorWidget::favourites().at(n));
	}
	settings.endGroup();

	// History file
	LineParser historyFile;
	historyFile.openOutput(filename, true);
	if (historyFile.isFileGoodForWriting())
	{
		QString line;
		int n;

		// Scripts
		for (Program* prog = aten_.scripts(); prog != NULL; prog = prog->next)
		{
			line.sprintf("%s  %s", Prefs::historyType(Prefs::ScriptHistory), qPrintable(prog->filename()));
			historyFile.writeLine(line);
		}

		// Quick command history
		for (n=0; n < ui.QuickCommandCombo->count(); ++n)
		{
			printf("Writing Line: [%s]\n", qPrintable(ui.QuickCommandCombo->itemText(n)));
			line.sprintf("%s  %s", Prefs::historyType(Prefs::CommandHistory), qPrintable(ui.QuickCommandCombo->itemText(n)));
			historyFile.writeLine(line);
		}
		
		// Intelligent Selection history
		for (n=0; n < ui.SelectIntelligentTargetCombo->count(); ++n)
		{
			line.sprintf("%s  %s", Prefs::historyType(Prefs::SelectHistory), qPrintable(ui.SelectIntelligentTargetCombo->itemText(n)));
			historyFile.writeLine(line);
		}
	}
	historyFile.closeFiles();

	Messenger::print("Done.");
}
