/*
	*** Qt GUI: Qt Settings
	*** src/gui/settings.cpp
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

#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/command.h"
#include "main/aten.h"
#include <QtCore/QSettings>

// Load Qt Settings
void AtenForm::loadSettings()
{
	QString key;
	Dnchar filename;
	int n;
	// Recent file entries
	for (n=0; n<MAXRECENTFILES; n++)
	{
		// Construct settings value to search for
		key = "RecentFile";
		key += itoa(n);
		if (settings_.contains(key)) addRecent(qPrintable(settings_.value(key).toString()));
	}
	// Toolbar visibility / position
	if (prefs.loadQtSettings() && settings_.contains("MainWinPositions")) gui.mainWindow->restoreState( settings_.value("MainWinPositions").toByteArray());
	// Command toolbar history
	QStringList history;
	n = 0;
	do
	{
		key = "CommandHistory";
		key += itoa(n);
		if (settings_.contains(key)) history << settings_.value(key).toString();
		++n;
	} while (settings_.contains(key));
	gui.commandWindow->setCommandList(history);
	// Scripts
	n = 0;
	do
	{
		key = "Script";
		key += itoa(n);
		if (settings_.contains(key))
		{
			Forest *f = aten.addScript();
			filename = qPrintable(settings_.value(key).toString());
			if (f->generateFromFile(filename.get(), filename.get())) msg.print("Successfully loaded script file '%s'.\n", filename.get());
			else
			{
				aten.removeScript(f);
				msg.print("Failed to load script file '%s'.\n", filename.get());
				settings_.remove(key);
			}
		}
		++n;
	} while (settings_.contains(key));
}

// Save Qt settings
void AtenForm::saveSettings()
{
	QString key;
	int n;
	// Save the recent file entries
	for (n=0; n<MAXRECENTFILES; n++)
	{
		// Create name tag
		key = "RecentFile";
		key += itoa(n);
		if (actionRecentFile[n]->isVisible()) settings_.setValue(key,actionRecentFile[n]->data().toString());
		else settings_.remove(key);
	}
	// Toolbar visibility / position
	settings_.setValue("MainWinPositions", gui.mainWindow->saveState() );
	// Command toolbar history
	QStringList history = gui.commandWindow->commandList();
	for (n=0; n < history.count(); ++n)
	{
		key = "CommandHistory";
		key += itoa(n);
		settings_.setValue(key, history[n]);
	}
	// Scripts
	n = 0;
	for (Forest *f = aten.scripts(); f != NULL; f = f->next)
	{
		key = "Script";
		key += itoa(n);
		settings_.setValue(key, f->filename());
		++n;
	}
	// Synchronise (i.e. save) changes to settings
	settings_.sync();
}
