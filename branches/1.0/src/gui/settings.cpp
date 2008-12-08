/*
	*** Qt GUI: Qt Settings
	*** src/gui/settings.cpp
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

#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include <QtCore/QSettings>

// Load Qt Settings
void AtenForm::loadSettings()
{
	char temp[128];
	// Recent file entries
	for (int n=0; n<MAXRECENTFILES; n++)
	{
		// Construct settings value to search for
		strcpy(temp,"RecentFile");
		strcat(temp,itoa(n));
		if (settings_.contains(temp)) addRecent(qPrintable(settings_.value(temp).toString()));
	}
	// Toolbar visibility / position
	if (settings_.contains("MainWinPositions")) gui.mainWindow->restoreState( settings_.value("MainWinPositions").toByteArray());
}

// Save Qt settings
void AtenForm::saveSettings()
{
	char temp[128];
	// Save the recent file entries
	for (int i=0; i<MAXRECENTFILES; i++)
	{
		// Create name tag
		strcpy(temp,"RecentFile");
		strcat(temp,itoa(i));
		//if (actionRecentFile[i]->isVisible()) printf("action %i is visible\n",i);
		if (actionRecentFile[i]->isVisible()) settings_.setValue(temp,actionRecentFile[i]->data().toString());
		else settings_.remove(temp);
	}
	// Toolbar visibility / position
	settings_.setValue("MainWinPositions", gui.mainWindow->saveState() );
}
