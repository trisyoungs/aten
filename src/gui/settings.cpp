/*
	*** Qt Settings Load/Save
	*** src/gui/settings.cpp
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

#include <QtCore/QFileInfo>
#include "base/sysfunc.h"
#include "gui/mainwindow.h"
#include "gui/command.h"
#include "gui/select.h"
#include "main/aten.h"
#include <QtCore/QSettings>

// Load Qt Settings
void AtenWindow::loadSettings()
{
	QString key;
	QFileInfo fi1, fi2;
	Dnchar filename;
	QStringList commandHistory, selectHistory, selectForHistory, selectNetaHistory;
	Program* prog, *loadedscript;
	int n;

	// Toolbar visibility / position
	if (prefs.loadQtSettings())
	{
		if (settings_.contains("MainWinPositions")) restoreState( settings_.value("MainWinPositions").toByteArray());
		if (settings_.contains("MainWinGeometries")) restoreGeometry( settings_.value("MainWinGeometries").toByteArray());
		if (settings_.contains("MainWinSize")) resize(settings_.value("mainwin_size", QSize(400, 400)).toSize());
		if (settings_.contains("MainWinPosition")) move(settings_.value("mainwin_pos", QPoint(200, 200)).toPoint());
	}

	// Check for old command history information, read it, and then remove it
	n = 0;
	do
	{
		key = "CommandHistory";
		key += itoa(n);
		if (settings_.contains(key))
		{
			commandHistory << settings_.value(key).toString();
			settings_.remove(key);
		}
		++n;
	} while (settings_.contains(key));
	settings_.sync();

	// Probe user history file...
	filename.sprintf("%s%c%s%chistory.txt", aten_.homeDir(), PATHSEP, aten_.atenDir(), PATHSEP);
	Messenger::print("Looking for program history file '%s'...\n", filename.get());
	if (fileExists(filename))
	{
		Messenger::print("Program history file found in '%s'\n", filename.get());
		LineParser parser;
		Dnchar arg, data;
		if (!parser.openInput(filename))
		{
			Messenger::print("Failed to open program history file.\n");
			return;
		}
		while (!parser.eofOrBlank())
		{
			// Get first part of line - the identifier for the information which follows it
			if (parser.readNextLine(LineParser::SkipBlanks) == 1) break;
			if (!parser.getArgDelim(LineParser::SkipBlanks, &arg)) break;
			// Get remainder of line, which is the data to store
			if (!parser.getRestDelim(&data)) break;
			// Determine history type
			Prefs::HistoryType ht = Prefs::historyType(arg, TRUE);
			if (ht == Prefs::nHistoryTypes) continue;
			switch (ht)
			{
				case (Prefs::CommandHistory):
					commandHistory << data.get();
					break;
				case (Prefs::RecentFileHistory):
					addRecent(data);
					break;
				case (Prefs::ScriptHistory):
					// Has this script already been loaded?
					// Use a couple of QFileInfo's to find out...
					fi1.setFile(data.get());
					for (loadedscript = aten_.scripts(); loadedscript != NULL; loadedscript = loadedscript->next)
					{
						fi2.setFile(loadedscript->filename());
						if (fi1.canonicalFilePath() == fi2.canonicalFilePath()) break;
					}
					if (loadedscript != NULL)
					{
						printf("Script '%s' appears to have been loaded already - will not load it a second time.\n", data.get());
					}
					else
					{
						prog = aten_.addScript();
						if (prog->generateFromFile(data, data)) Messenger::print("Successfully loaded script file '%s'.\n", data.get());
						else
						{
							aten_.removeScript(prog);
							Messenger::print("Failed to load script file '%s'.\n", data.get());
						}
					}
					break;
				case (Prefs::SelectForHistory):
					selectForHistory << data.get();
					break;
				case (Prefs::SelectHistory):
					selectHistory << data.get();
					break;
				case (Prefs::SelectNetaHistory):
					selectNetaHistory << data.get();
					break;
			}
		}
		parser.closeFiles();
	}
	else Messenger::print("Program history file not found.\n");
	
	// Update GUI controls
	commandWidget->setCommandList(commandHistory);
	selectWidget->setHistories(selectHistory, selectForHistory, selectNetaHistory);
}

// Save Qt settings
void AtenWindow::saveSettings()
{
	// Open new file for writing...
	Dnchar filename;
	filename.sprintf("%s%c%s%chistory.txt", aten_.homeDir(), PATHSEP, aten_.atenDir(), PATHSEP);
	Messenger::print("Savung program history file '%s'...", filename.get());
	LineParser historyFile;
	historyFile.openOutput(filename, TRUE);
	
	if (historyFile.isFileGoodForWriting())
	{
		Dnchar line;
		int n;
		QStringList history;
		
		// Recent file entries
		for (n=0; n<MAXRECENTFILES; n++)
		{
			// Check file entry contains data (is visible in the GUI)
			if (!actionRecentFile[n]->isVisible()) continue;
			line.sprintf("RecentFile  %s\n", qPrintable(actionRecentFile[n]->data().toString()));
			historyFile.writeLine(line);
		}
		
		// Scripts
		for (Program* prog = aten_.scripts(); prog != NULL; prog = prog->next)
		{
			line.sprintf("Script  %s\n", prog->filename());
			historyFile.writeLine(line);
		}

		// Command toolbar history
		history = commandWidget->commandList();
		for (n=0; n < history.count(); ++n)
		{
			line.sprintf("Command  %s\n", qPrintable(history[n]));
			historyFile.writeLine(line);
		}
		
		// Select combo history
		for (n=0; n < selectWidget->ui.SelectCombo->count(); ++n)
		{
			line.sprintf("Select  %s\n", qPrintable(selectWidget->ui.SelectCombo->itemText(n)));
			historyFile.writeLine(line);
		}
		
		// SelectFor combo history
		for (n=0; n < selectWidget->ui.SelectForCombo->count(); ++n)
		{
			line.sprintf("SelectFor  %s\n", qPrintable(selectWidget->ui.SelectForCombo->itemText(n)));
			historyFile.writeLine(line);
		}
		
		// SelectNeta combo history
		for (n=0; n < selectWidget->ui.SelectNetaCombo->count(); ++n)
		{
			line.sprintf("SelectNeta  %s\n", qPrintable(selectWidget->ui.SelectNetaCombo->itemText(n)));
			historyFile.writeLine(line);
		}
		
	}
	
	historyFile.closeFiles();
	Messenger::print("Done.\n");
}


// Save default window state
void AtenWindow::on_actionStoreDefaultWindowState_triggered(bool checked)
{
	// Toolbar visibility / position
	settings_.setValue("MainWinPositions", saveState() );
	settings_.setValue("MainWinGeometries", saveGeometry() );
	settings_.value("MainWinSize", size());
	settings_.value("MainWinPosition", pos());

	// Synchronise (i.e. save) changes to settings
	settings_.sync();
}
