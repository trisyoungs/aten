/*
	*** Main Window - Calculate Panel Functions
	*** src/gui/mainwindow_panel_calculate.cpp
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
#include "main/aten.h"

// Update calculate panel
void AtenWindow::updateCalculatePanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateCalculatePanel");

	Messenger::exit("AtenWindow::updateCalculatePanel");
}

/*
 * Measure
 */

void AtenWindow::on_CalculateMeasureDistanceButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::MeasureDistanceAction);
}

void AtenWindow::on_CalculateMeasureAngleButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::MeasureAngleAction);
}

void AtenWindow::on_CalculateMeasureTorsionButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::MeasureTorsionAction);
}

void AtenWindow::on_CalculateMeasureClearButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ClearMeasurements);

	// Update display
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_CalculateMeasureListButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->listMeasurements();

	// Update display
	updateWidgets(AtenWindow::MainViewTarget);
}
