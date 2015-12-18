/*
	*** Popup Widget - Scripts Open
	*** src/gui/popupscriptsopen_funcs.cpp
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

#include "gui/popupscriptsopen.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
ScriptsOpenPopup::ScriptsOpenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Connect signals in TRecentFiles widget to our slots here
	connect(ui.RecentFiles, SIGNAL(fileSelected(QString)), this, SLOT(loadScript(QString)));
}

// Update controls (before show()) (virtual)
void ScriptsOpenPopup::updateControls()
{
	refreshing_ = true;

	ui.RecentFiles->updateControls();

	refreshing_ = false;
}

// Call named method associated to popup
bool ScriptsOpenPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "addRecentFile")
	{
		ui.RecentFiles->addFile(rv.asString());
	}
	else if (methodName == "maxRecentFiles")
	{
		rv = ui.RecentFiles->maxFiles();
	}
	else if (methodName == "nRecentFiles")
	{
		rv = ui.RecentFiles->nFiles();
	}
	else if (methodName == "recentFile")
	{
		rv = ui.RecentFiles->file(rv.asInteger());
	}
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

void ScriptsOpenPopup::loadScript(QString filename)
{
	Program* script = parent_.aten().addScript();
	if (script->generateFromFile(filename, filename))
	{
		Messenger::print("Successfully loaded script.");

		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::ToolsPanelTarget);
	}
	else parent_.aten().removeScript(script);

	done();
}
