/*
	*** Popup Widget - Transform Shift Functions
	*** src/gui/popuptransformshift_funcs.cpp
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

#include "gui/popuptransformshift.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
TransformShiftPopup::TransformShiftPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformShiftPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformShiftPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "setShiftVector")
	{
		bool success;
		Vec3<double> v = rv.asVector(success);
		if (!success)
		{
			Messenger::error("Failed to get vector for shift axis.");
			return false;
		}
		ui.XSpin->setValue(v.x);
		ui.YSpin->setValue(v.y);
		ui.ZSpin->setValue(v.z);
		ui.MagnitudeLabel->setText(QString::number(v.magnitude()));
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

void TransformShiftPopup::on_PickButton_clicked(bool checked)
{
	// Enter manual picking mode
	parent_.setSelectedMode(UserAction::ShiftPickVectorAction);

	done();
}

void TransformShiftPopup::on_NormaliseButton_clicked(bool checked)
{
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	v.normalise();
	ui.XSpin->setValue(v.x);
	ui.YSpin->setValue(v.y);
	ui.ZSpin->setValue(v.z);
}

void TransformShiftPopup::on_XSpin_valueChanged(double value)
{
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	ui.MagnitudeLabel->setText(QString::number(v.magnitude()));
}

void TransformShiftPopup::on_YSpin_valueChanged(double value)
{
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	ui.MagnitudeLabel->setText(QString::number(v.magnitude()));
}

void TransformShiftPopup::on_ZSpin_valueChanged(double value)
{
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	ui.MagnitudeLabel->setText(QString::number(v.magnitude()));
}

void TransformShiftPopup::on_ForwardButton_clicked(bool checked)
{
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;

	// Get current vector, and multiply by shift amount
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	v *= ui.ShiftSpin->value();

	// Make the adjustment
	currentModel->beginUndoState("Vector shift %i atom(s) {%f,%f,%f}", currentModel->nSelected(),v.x,v.y,v.z);
	currentModel->translateSelectionLocal(v);
	currentModel->endUndoState();

	// Update
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);
}

void TransformShiftPopup::on_BackwardButton_clicked(bool checked)
{
	Model* currentModel = parent_.aten().currentModelOrFrame();
	if (!currentModel) return;

	// Get current vector, and multiply by shift amount
	Vec3<double> v(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());
	v *= -ui.ShiftSpin->value();

	// Make the adjustment
	currentModel->beginUndoState("Vector shift %i atom(s) {%f,%f,%f}", currentModel->nSelected(),v.x,v.y,v.z);
	currentModel->translateSelectionLocal(v);
	currentModel->endUndoState();

	// Update
	parent_.updateWidgets(AtenWindow::AtomsTableTarget);
}
