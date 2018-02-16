/*
	*** Popup Widget - Grids Open
	*** src/gui/popupgridsopen_funcs.cpp
	Copyright T. Youngs 2007-2018

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

#include "gui/popupgridsopen.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridsOpenPopup::GridsOpenPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);

	// Connect signals in TRecentFiles widget to our slots here
	connect(ui.RecentFiles, SIGNAL(fileSelected(QString)), this, SLOT(loadGrid(QString)));
}

// Update controls (before show()) (virtual)
void GridsOpenPopup::updateControls()
{
	refreshing_ = true;

	ui.RecentFiles->updateControls();

	refreshing_ = false;
}

// Call named method associated to popup
bool GridsOpenPopup::callMethod(QString methodName, ReturnValue& rv)
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

void GridsOpenPopup::loadGrid(QString filename)
{
	Model* targetModel = parent_.aten().currentModelOrFrame();
	if (!targetModel) return;

	parent_.aten().importGrid(targetModel, filename);

	parent_.updateWidgets(AtenWindow::GridsPanelTarget);

	done();
}
