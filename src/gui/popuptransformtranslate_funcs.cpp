/*
	*** Popup Widget - Transform Translate Functions
	*** src/gui/popuptransformtranslate_funcs.cpp
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

#include "gui/popuptransformtranslate.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformTranslatePopup::TransformTranslatePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformTranslatePopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformTranslatePopup::callMethod(QString methodName, ReturnValue& rv)
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

// Translate current selection
void TransformTranslatePopup::translateSelection(int axis, int dir)
{
	// Get current model
	Model* currentModel = parent_.aten().currentModelOrFrame();

	if (!currentModel) return;
	double step = ui.ShiftSpin->value();	
	Vec3<double> tvec;
	tvec.set(axis, double(dir));

	// Translation vector depends on selected frame of reference
	if (ui.ModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		currentModel->beginUndoState("Translate Cartesian (%i atom(s), %f %f %f)", currentModel->nSelected(), tvec.x, tvec.y, tvec.z);
		currentModel->translateSelectionLocal(tvec);
	}
	else if (ui.WorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		currentModel->beginUndoState("Translate Screen (%i atom(s), %f %f %f)", currentModel->nSelected(), tvec.x, tvec.y, tvec.z);
		currentModel->translateSelectionWorld(tvec);
	}
	else if (ui.CellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (currentModel->cell().type() == UnitCell::NoCell)
		{
			Messenger::print("No unit cell defined for model.");
			return;
		}
		tvec = parent_.aten().currentModelOrFrame()->cell().axes().columnAsVec3(axis);
		tvec *= double(dir) * step;
		currentModel->beginUndoState("Translate Cell (%i atom(s), %f %f %f)", currentModel->nSelected(), tvec.x, tvec.y, tvec.z);
		currentModel->translateSelectionLocal(tvec);
	}
	currentModel->endUndoState();

	// Update
	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void TransformTranslatePopup::on_PositiveXButton_clicked(bool checked)
{
	translateSelection(0, 1);
}

void TransformTranslatePopup::on_PositiveYButton_clicked(bool checked)
{
	translateSelection(1, 1);
}

void TransformTranslatePopup::on_PositiveZButton_clicked(bool checked)
{
	translateSelection(2, 1);
}

void TransformTranslatePopup::on_NegativeXButton_clicked(bool checked)
{
	translateSelection(0, -1);
}

void TransformTranslatePopup::on_NegativeYButton_clicked(bool checked)
{
	translateSelection(1, -1);
}

void TransformTranslatePopup::on_NegativeZButton_clicked(bool checked)
{
	translateSelection(2, -1);
}
