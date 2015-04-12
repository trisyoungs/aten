/*
	*** Main Window - Cell Panel Functions
	*** src/gui/mainwindow_panel_cell.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupcellmatrix.h"

/*
 * Define
 */

void AtenWindow::on_CellDefinePeriodicButton_clicked(bool checked)
{
	if (refreshing_) return;

	if (checked) 
	{
		// Get the cell vectors from the CellMatrixPopup widget
		CellMatrixPopup* popup = (CellMatrixPopup*) ui.CellDefineMatrixButton->popupWidget();
		CommandNode::run(Commands::CellAxes, "ddddddddd", popup->ui.AxisAXSpin->value(), popup->ui.AxisAYSpin->value(), popup->ui.AxisAZSpin->value(), popup->ui.AxisBXSpin->value(), popup->ui.AxisBYSpin->value(), popup->ui.AxisBZSpin->value(), popup->ui.AxisCXSpin->value(), popup->ui.AxisCYSpin->value(), popup->ui.AxisCZSpin->value());
	}
	else CommandNode::run(Commands::NoCell, "");

	// Update display
	updateWidgets(AtenWindow::CanvasTarget);
}

