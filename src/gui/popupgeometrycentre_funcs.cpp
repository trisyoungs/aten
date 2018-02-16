/*
	*** Popup Widget - Calculate Centre Functions
	*** src/gui/popupgeometrycentre_funcs.cpp
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

#include "gui/popupgeometrycentre.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GeometryCentrePopup::GeometryCentrePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void GeometryCentrePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool GeometryCentrePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "addDummy")
	{
		rv.set(ui.AddDummyCheck->isChecked());
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

void GeometryCentrePopup::on_MassButton_clicked(bool checked)
{
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;
	
	// Get the centre of geometry of the current selection and, if the model is periodic, fold it into the unit cell
	Vec3<double> com = currentModel->selectionCentreOfMass();
	if (currentModel->isPeriodic()) com = currentModel->cell().fold(com);
	Messenger::print("Geometric centre of current selection (%i atoms) is { %f %f %f }.", currentModel->nSelected(), com.x, com.y, com.z);
	
	// Create dummy atom at the position?
	if (ui.AddDummyCheck->isChecked())
	{
		currentModel->beginUndoState("Add dummy atom at selection's centre of mass");
		currentModel->addAtom(0, com);
		currentModel->endUndoState();
	}
	
	// Update display
	parent_.updateWidgets();

	done();
}

