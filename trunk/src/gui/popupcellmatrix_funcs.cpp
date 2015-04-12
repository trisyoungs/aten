/*
	*** Popup Widget - Cell Matrix Functions
	*** src/gui/popupcellangles_funcs.cpp
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

#include "gui/popupcellmatrix.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
CellMatrixPopup::CellMatrixPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Show popup, updating any controls as necessary beforehand
void CellMatrixPopup::popup()
{
	// Update angles in spin boxes
	refreshing_ = true;

	// Get current model
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		ui.AxisAXSpin->setValue(model->cell()->parameter(UnitCell::CellAX));
		ui.AxisAYSpin->setValue(model->cell()->parameter(UnitCell::CellAY));
		ui.AxisAZSpin->setValue(model->cell()->parameter(UnitCell::CellAZ));
		ui.AxisBXSpin->setValue(model->cell()->parameter(UnitCell::CellBX));
		ui.AxisBYSpin->setValue(model->cell()->parameter(UnitCell::CellBY));
		ui.AxisBZSpin->setValue(model->cell()->parameter(UnitCell::CellBZ));
		ui.AxisCXSpin->setValue(model->cell()->parameter(UnitCell::CellCX));
		ui.AxisCYSpin->setValue(model->cell()->parameter(UnitCell::CellCY));
		ui.AxisCZSpin->setValue(model->cell()->parameter(UnitCell::CellCZ));
	}

	show();

	refreshing_ = false;
}

/*
 * Widget Functions
 */

void CellMatrixPopup::setCurrentMatrix()
{
	// Get current model and set new angle in cell
	Model* model = parent_.aten().currentModelOrFrame();
	if (!model) return;

	// Get the cell vectors from the CellMatrixPopup widget
	CommandNode::run(Commands::CellAxes, "ddddddddd", ui.AxisAXSpin->value(), ui.AxisAYSpin->value(), ui.AxisAZSpin->value(), ui.AxisBXSpin->value(), ui.AxisBYSpin->value(), ui.AxisBZSpin->value(), ui.AxisCXSpin->value(), ui.AxisCYSpin->value(), ui.AxisCZSpin->value());
}

void CellMatrixPopup::on_AxisAXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisAYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisAZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisBXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisBYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisBZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisCXSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisCYSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellMatrixPopup::on_AxisCZSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Set current matrix
	setCurrentMatrix();

	// Update display
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}
