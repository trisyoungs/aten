/*
	*** Qt GUI: Qt Settings
	*** src/gui/settings.cpp
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

#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include <QtCore/QSettings>

// Load Qt Settings
void AtenForm::loadSettings()
{
	QString key;
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
	if (settings_.contains("MainWinPositions")) gui.mainWindow->restoreState( settings_.value("MainWinPositions").toByteArray());
	// Command toolbar history
	QStringList history;
	for (n=0; n < prefs.commandHistoryLimit(); ++n)
	{
		key = "CommandHistory";
		key += itoa(n);
		if (settings_.contains(key)) history << settings_.value(key).toString();
	}
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
	QStringList history = commandEditModel_->stringList();
	for (n=0; n < prefs.commandHistoryLimit(); ++n)
	{
		key = "CommandHistory";
		key += itoa(n);
		if (n < history.count()) settings_.setValue(key, history[n]);
	}
}
