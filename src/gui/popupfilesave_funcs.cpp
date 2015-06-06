/*
	*** Popup Widget - File Save
	*** src/gui/popupfilesave_funcs.cpp
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

#include "gui/popupfilesave.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
FileSavePopup::FileSavePopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void FileSavePopup::popup()
{
	refreshing_ = true;

	// Get current model
	Model* currentModel = parent_.aten().currentModelOrFrame();

	ui.OptionsButton->setEnabled(currentModel && currentModel->filter() && (currentModel->filter()->defaultDialog().nWidgets() != 0));
	
	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool FileSavePopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void FileSavePopup::on_OptionsButton_clicked(bool checked)
{
	// Hide popup
	done();

	// Get current model
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;

	if (currentModel->filter() == NULL) Messenger::print("No filter currently assigned to model '%s', so there are no export options.", qPrintable(currentModel->name()));
	else currentModel->filter()->defaultDialog().execute();
}
