/*
	*** Popup Widget - Grids Matrix Functions
	*** src/gui/popupgridmatrix_funcs.cpp
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

#include "gui/popupgridmatrix.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
GridMatrixPopup::GridMatrixPopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void GridMatrixPopup::updateControls()
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

	refreshing_ = false;
}

// Call named method associated to popup
bool GridMatrixPopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else if (methodName == "setMatrix")
	{
		setCurrentMatrix();
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

void GridMatrixPopup::setCurrentMatrix()
{
	// Get current model and set new grid voxel matrix
	Grid* grid;
	if (!parent_.aten().currentGrid(grid)) return;

	// Get the cell vectors from the GridMatrixPopup widget
	CommandNode::run(Commands::GridAxes, "ddddddddd", ui.AxisAXSpin->value(), ui.AxisAYSpin->value(), ui.AxisAZSpin->value(), ui.AxisBXSpin->value(), ui.AxisBYSpin->value(), ui.AxisBZSpin->value(), ui.AxisCXSpin->value(), ui.AxisCYSpin->value(), ui.AxisCZSpin->value());
}

void GridMatrixPopup::on_AxisAXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisAYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisAZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisBXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisBYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisBZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisCXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisCYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}

void GridMatrixPopup::on_AxisCZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::MainViewTarget);
}
