/*
	*** Main Window - Tools Panel Functions
	*** src/gui/mainwindow_panel_tools.cpp
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

// Update tools panel
void AtenWindow::updateToolsPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateToolsPanel");

	Messenger::exit("AtenWindow::updateToolsPanel");
}

/*
 * Disorder
 */

void AtenWindow::on_ToolsDisorderCreateButton_clicked(bool checked)
{
	disorderWizard_.run();
}

/*
 * Surface
 */

void AtenWindow::on_ToolsSurfaceTerminateButton_clicked(bool checked)
{
	// First check - are any atoms selected
	Model* currentModel = aten().currentModelOrFrame();
	if (currentModel->nSelected() == 0)
	{
		Messenger::print("No atoms selected in current model, so nothing to terminate.");
		return;
	}

	// Run the command
	CommandNode::run(Commands::Terminate, "");

	// Update the main window
	updateWidgets(AtenWindow::MainViewTarget);
}
