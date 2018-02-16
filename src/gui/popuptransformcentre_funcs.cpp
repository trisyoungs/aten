/*
	*** Popup Widget - Transform Centre Functions
	*** src/gui/popuptransformcentre_funcs.cpp
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

#include "gui/popuptransformcentre.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformCentrePopup::TransformCentrePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformCentrePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformCentrePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "centre")
	{
		// Get new coordinates and lock flags
		Vec3<double> centre(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
		Vec3<int> lock(ui.LockXCheck->isChecked(), ui.LockYCheck->isChecked(), ui.LockZCheck->isChecked());

		// Run command
		CommandNode::run(Commands::Centre, "dddiii", centre.x, centre.y, centre.z, lock.x, lock.y, lock.z);

		parent_.updateWidgets(AtenWindow::AtomsTableTarget);
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

void TransformCentrePopup::on_DefineFromSelectionButton_clicked(bool checked)
{
	// Get centre of current selection
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;
// 	if (!parent_.aten().currentModelOrFrame(currentModel);

	// Get selection centre and set controls
	Vec3<double> centre = currentModel->selectionCentreOfGeometry();
	ui.XSpin->setValue(centre.x);
	ui.YSpin->setValue(centre.y);
	ui.ZSpin->setValue(centre.z);

	// Hide popup
	hide();
}
