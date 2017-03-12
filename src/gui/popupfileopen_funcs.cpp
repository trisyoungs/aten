/*
	*** Popup Widget - File Open
	*** src/gui/popupfileopen_funcs.cpp
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

#include "gui/popupfileopen.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
FileOpenPopup::FileOpenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Connect signals in TRecentFiles widget to our slots here
	connect(ui.RecentFiles, SIGNAL(fileSelected(QString)), this, SLOT(loadFile(QString)));
}

// Update controls (before show()) (virtual)
void FileOpenPopup::updateControls()
{
	refreshing_ = true;

	ui.RecentFiles->updateControls();

	refreshing_ = false;
}

// Call named method associated to popup
bool FileOpenPopup::callMethod(QString methodName, ReturnValue& rv)
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

void FileOpenPopup::loadFile(QString fileName)
{
	// Load model
	if (parent_.aten().importModel(fileName)) parent_.aten().setSingleModelVisible(parent_.aten().currentModel());

	// Update main window
	parent_.updateWidgets(AtenWindow::AllTargets);

	done();
}
