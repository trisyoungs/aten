/*
	*** Popup Widget - Transform Reposition Functions
	*** src/gui/popuptransformreposition_funcs.cpp
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

#include "gui/popuptransformreposition.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformRepositionPopup::TransformRepositionPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformRepositionPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformRepositionPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "reposition")
	{
		Model* currentModel = parent_.aten().currentModelOrFrame();
		if (!currentModel) return false;

		Vec3<double> v;
		v.x = ui.TargetXSpin->value() - ui.ReferenceXSpin->value();
		v.y = ui.TargetYSpin->value() - ui.ReferenceYSpin->value();
		v.z = ui.TargetZSpin->value() - ui.ReferenceZSpin->value();

		currentModel->beginUndoState("Reposition %i atom(s) {%f,%f,%f}", currentModel->nSelected(), v.x, v.y, v.z);
		currentModel->translateSelectionLocal(v);
		currentModel->endUndoState();

		parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
	}
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

void TransformRepositionPopup::on_DefineReferenceButton_clicked(bool checked)
{
	// Get current model
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;

	Vec3<double> centre = currentModel->selectionCentreOfGeometry();

	ui.ReferenceXSpin->setValue(centre.x);
	ui.ReferenceYSpin->setValue(centre.y);
	ui.ReferenceZSpin->setValue(centre.z);
}

void TransformRepositionPopup::on_DefineTargetButton_clicked(bool checked)
{
	// Get current model
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;

	Vec3<double> centre = currentModel->selectionCentreOfGeometry();

	ui.TargetXSpin->setValue(centre.x);
	ui.TargetYSpin->setValue(centre.y);
	ui.TargetZSpin->setValue(centre.z);
}
