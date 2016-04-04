/*
	*** Main Window - Transform Panel Functions
	*** src/gui/mainwindow_panel_transform.cpp
	Copyright T. Youngs 2007-2016

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

// Update transform panel
void AtenWindow::updateTransformPanel(Model* sourceModel)
{
}

/*
 * Geometry
 */

void AtenWindow::on_TransformGeometryDistanceButton_clicked(bool checked)
{
	// Call the set method in the popup
	ReturnValue rv;
	ui.TransformGeometryDistanceButton->callPopupMethod("set", rv);
}

void AtenWindow::on_TransformGeometryAngleButton_clicked(bool checked)
{
	// Call the set method in the popup
	ReturnValue rv;
	ui.TransformGeometryAngleButton->callPopupMethod("set", rv);
}

void AtenWindow::on_TransformGeometryTorsionButton_clicked(bool checked)
{
	// Call the set method in the popup
	ReturnValue rv;
	ui.TransformGeometryTorsionButton->callPopupMethod("set", rv);
}

/*
 * Position
 */

void AtenWindow::on_TransformPositionCentreButton_clicked(bool checked)
{
	// Call the centre method in the popup
	ReturnValue rv;
	ui.TransformPositionCentreButton->callPopupMethod("centre", rv);
}

void AtenWindow::on_TransformPositionRepositionButton_clicked(bool checked)
{
	// Call the centre method in the popup
	ReturnValue rv;
	ui.TransformPositionRepositionButton->callPopupMethod("reposition", rv);
}

void AtenWindow::on_TransformPositionZeroButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Centre, "ddd", 0.0, 0.0, 0.0);

	// Update display
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

/*
 * Transform
 */

void AtenWindow::on_TransformTransformRotateButton_clicked(bool checked)
{
	// ATEN2 TODO
}

void AtenWindow::on_TransformTransformMultiplyButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.TransformTransformMultiplyButton->callPopupMethod("multiply", rv);
}

void AtenWindow::on_TransformTransformConvertButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.TransformTransformConvertButton->callPopupMethod("convert", rv);
}
