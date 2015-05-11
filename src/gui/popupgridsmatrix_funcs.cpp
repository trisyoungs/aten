/*
	*** Popup Widget - Grids Matrix Functions
	*** src/gui/popupgridsmatrix_funcs.cpp
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

#include "gui/popupgridsmatrix.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridsMatrixPopup::GridsMatrixPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void GridsMatrixPopup::popup()
{
	// Update angles in spin boxes
	refreshing_ = true;

	// Get current model
	Grid* grid = parent_.aten().current().g;
	if (grid)
	{
		ui.AxisAXSpin->setValue(grid->cell()->parameter(UnitCell::CellAX));
		ui.AxisAYSpin->setValue(grid->cell()->parameter(UnitCell::CellAY));
		ui.AxisAZSpin->setValue(grid->cell()->parameter(UnitCell::CellAZ));
		ui.AxisBXSpin->setValue(grid->cell()->parameter(UnitCell::CellBX));
		ui.AxisBYSpin->setValue(grid->cell()->parameter(UnitCell::CellBY));
		ui.AxisBZSpin->setValue(grid->cell()->parameter(UnitCell::CellBZ));
		ui.AxisCXSpin->setValue(grid->cell()->parameter(UnitCell::CellCX));
		ui.AxisCYSpin->setValue(grid->cell()->parameter(UnitCell::CellCY));
		ui.AxisCZSpin->setValue(grid->cell()->parameter(UnitCell::CellCZ));
	}

	show();

	refreshing_ = false;
}

// Call named method associated to popup
bool GridsMatrixPopup::callMethod(QString methodName, ReturnValue& rv)
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

void GridsMatrixPopup::setCurrentMatrix()
{
	// Get current model and set new angle in cell
	Grid* grid = parent_.aten().current().g;
	if (!grid) return;

	// Get the cell vectors from the GridsMatrixPopup widget
	CommandNode::run(Commands::GridAxes, "ddddddddd", ui.AxisAXSpin->value(), ui.AxisAYSpin->value(), ui.AxisAZSpin->value(), ui.AxisBXSpin->value(), ui.AxisBYSpin->value(), ui.AxisBZSpin->value(), ui.AxisCXSpin->value(), ui.AxisCYSpin->value(), ui.AxisCZSpin->value());
}

void GridsMatrixPopup::on_AxisAXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisAYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisAZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisBXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisBYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisBZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisCXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisCYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void GridsMatrixPopup::on_AxisCZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}
