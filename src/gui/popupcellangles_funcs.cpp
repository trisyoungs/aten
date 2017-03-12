/*
	*** Popup Widget - Cell Angles Functions
	*** src/gui/popupcellangles_funcs.cpp
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

#include "gui/popupcellangles.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
CellAnglesPopup::CellAnglesPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void CellAnglesPopup::updateControls()
{
	// Update angles in spin boxes
	refreshing_ = true;

	// Get current model
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		ui.AlphaSpin->setValue(model->cell().angles().x);
		ui.BetaSpin->setValue(model->cell().angles().y);
		ui.GammaSpin->setValue(model->cell().angles().z);
	}

	refreshing_ = false;
}

// Call named method associated to popup
bool CellAnglesPopup::callMethod(QString methodName, ReturnValue& rv)
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

// Adjust matrix of current model
void CellAnglesPopup::adjustCurrentMatrix(int angleIndex, double value)
{
	// Get current model and set new angle in cell
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		UnitCell cell = model->cell();
		cell.setAngle(angleIndex, value);
		CommandNode::run(Commands::CellAxes, "ddddddddd", cell.parameter(UnitCell::CellAX), cell.parameter(UnitCell::CellAY), cell.parameter(UnitCell::CellAZ), cell.parameter(UnitCell::CellBX), cell.parameter(UnitCell::CellBY), cell.parameter(UnitCell::CellBZ),  cell.parameter(UnitCell::CellCX), cell.parameter(UnitCell::CellCY), cell.parameter(UnitCell::CellCZ)); 
	}
}

void CellAnglesPopup::on_AlphaSpin_valueChanged(double value)
{
	if (refreshing_) return;

	adjustCurrentMatrix(0, value);

	// Update display
	parent_.updateWidgets();
}

void CellAnglesPopup::on_BetaSpin_valueChanged(double value)
{
	if (refreshing_) return;

	adjustCurrentMatrix(1, value);

	// Update display
	parent_.updateWidgets();
}

void CellAnglesPopup::on_GammaSpin_valueChanged(double value)
{
	if (refreshing_) return;

	adjustCurrentMatrix(2, value);

	// Update display
	parent_.updateWidgets();
}
