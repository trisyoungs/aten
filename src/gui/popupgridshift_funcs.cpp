/*
	*** Popup Widget - Grid Shift Functions
	*** src/gui/popupgridshift_funcs.cpp
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

#include "gui/popupgridshift.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"
#include "command/commands.h"

ATEN_USING_NAMESPACE

// Constructor
GridShiftPopup::GridShiftPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void GridShiftPopup::updateControls()
{
	refreshing_ = true;

	refreshing_ = false;
}

// Call named method associated to popup
bool GridShiftPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
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

void GridShiftPopup::gridShiftChanged()
{
	if (refreshing_) return;
	// Get currently selected grid(s) and set data
	Grid* currentGrid;
	if (!parent_.aten().currentGrid(currentGrid)) return;

	// Grab old shift values
	Vec3<int> oldshift = currentGrid->shift();
	currentGrid->setShift(ui.XSpin->value(), ui.YSpin->value(), ui.ZSpin->value());

	if (!ui.NoAtomsRadio->isChecked())
	{
		Model* currentModel = currentGrid->parent();

		// Determine shift amount...
		Vec3<int> delta = currentGrid->shift() - oldshift;
		Vec3<double> vec;
		vec += currentGrid->axes().columnAsVec3(0) * delta.x;
		vec += currentGrid->axes().columnAsVec3(1) * delta.y;
		vec += currentGrid->axes().columnAsVec3(2) * delta.z;

		// Move atoms....
		currentModel->beginUndoState("Shift atoms with grid");
		if (ui.AllAtomsRadio->isChecked())
		{
			currentModel->markAll();
			currentModel->translateSelectionLocal(vec, true);
		}
		else currentModel->translateSelectionLocal(vec, false);
		currentModel->endUndoState();
	}

	// Update
	parent_.updateWidgets();
}

void GridShiftPopup::on_PositiveXButton_clicked(bool checked)
{
	ui.XSpin->stepUp();
	gridShiftChanged();
}

void GridShiftPopup::on_PositiveYButton_clicked(bool checked)
{
	ui.YSpin->stepUp();
	gridShiftChanged();
}

void GridShiftPopup::on_PositiveZButton_clicked(bool checked)
{
	ui.ZSpin->stepUp();
	gridShiftChanged();
}

void GridShiftPopup::on_NegativeXButton_clicked(bool checked)
{
	ui.XSpin->stepDown();
	gridShiftChanged();
}

void GridShiftPopup::on_NegativeYButton_clicked(bool checked)
{
	ui.YSpin->stepDown();
	gridShiftChanged();
}

void GridShiftPopup::on_NegativeZButton_clicked(bool checked)
{
	ui.ZSpin->stepDown();
	gridShiftChanged();
}

void GridShiftPopup::on_XSpin_valueChanged(int i)
{
	gridShiftChanged();
}

void GridShiftPopup::on_YSpin_valueChanged(int i)
{
	gridShiftChanged();
}

void GridShiftPopup::on_ZSpin_valueChanged(int i)
{
	gridShiftChanged();
}
