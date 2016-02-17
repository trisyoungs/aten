/*
	*** Main Window - Build Panel Functions
	*** src/gui/mainwindow_panel_build.cpp
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
#include "main/aten.h"

// Update build panel
void AtenWindow::updateBuildPanel(Model* sourceModel)
{
	// Deactivate Fragments button if no fragments are available...
	ui.BuildDrawFragmentButton->setEnabled(aten_.nFragments() > 0);
}

/*
 * Select
 */

void AtenWindow::on_BuildSelectAtomsButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::SelectAction);
}

void AtenWindow::on_BuildSelectBoundButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::SelectBoundAction);
}

void AtenWindow::on_BuildSelectElementButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::SelectElementAction);
}

/*
 * Draw
 */

void AtenWindow::on_BuildDrawDrawButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawAtomsAction);
}

void AtenWindow::on_BuildDrawFragmentButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawFragmentsAction);
}

void AtenWindow::on_BuildDrawDeleteButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawDeleteAction);
}

void AtenWindow::on_BuildDrawTransmuteButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawTransmuteAction);
}

void AtenWindow::on_BuildDrawAddHButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawAddHydrogenAction);
}

void AtenWindow::on_BuildDrawGrowButton_clicked(bool checked)
{
	if (checked) setSelectedMode(UserAction::DrawGrowAtomsAction);
}

/*
 * Bonding
 */

void AtenWindow::on_BuildBondingRebondButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ReBond, "");

	// Update
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_BuildBondingAugmentButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Augment, "");

	// Update
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_BuildBondingClearButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ClearBonds, "");

	// Update
	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Public Functions
 */

// Return currently-selected build element
int AtenWindow::currentBuildElement()
{
	ReturnValue rv(0);
	if (ui.BuildElementTableButton->isChecked()) ui.BuildElementTableButton->callPopupMethod("currentElement", rv);
	else if (ui.BuildElementCommonButton->isChecked()) ui.BuildElementCommonButton->callPopupMethod("currentElement", rv);
	else Messenger::print("Internal Error: No element button is checked?");

	return rv.asInteger();
}
