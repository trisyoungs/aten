/*
	*** Main Window - Select Panel Functions
	*** src/gui/mainwindow_panel_select.cpp
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

// Update select panel
void AtenWindow::updateSelectPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateSelectPanel");

	Messenger::exit("AtenWindow::updateSelectPanel");
}

/*
 * Basic
 */

void AtenWindow::on_SelectBasicAllButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectAll, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget); // ATEN2 TODO Plus wherever selection information is going to be displayed.
}

void AtenWindow::on_SelectBasicNoneButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SelectNone, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget); // ATEN2 TODO Plus wherever selection information is going to be displayed.
}

void AtenWindow::on_SelectBasicInvertButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Invert, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget); // ATEN2 TODO Plus wherever selection information is going to be displayed.
}

void AtenWindow::on_SelectBasicExpandButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::Expand, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget); // ATEN2 TODO Plus wherever selection information is going to be displayed.
}

/*
 * ID / Element
 */

void AtenWindow::on_SelectElementSelectButton_clicked(bool checked)
{
	CommandNode::run(Commands::Select, "c", qPrintable(ui.SelectElementSelectCombo->currentText()));

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_SelectElementDeselectButton_clicked(bool checked)
{
	CommandNode::run(Commands::DeSelect, "c", qPrintable(ui.SelectElementSelectCombo->currentText()));

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
}

/*
 * Type
 */

void AtenWindow::on_SelectNETASelectButton_clicked(bool checked)
{
	// Get element
	ReturnValue rv;
	if (!ui.SelectNETAElementButton->callPopupMethod("currentElement", rv)) return;
	
	if (rv.asInteger() == 0) Messenger::print("Invalid element '%s'", qPrintable(rv.asInteger()));
	else
	{
		CommandNode::run(Commands::SelectType, "ic", rv.asInteger(), qPrintable(ui.SelectNETACodeCombo->currentText()));

		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
	}
}

void AtenWindow::on_SelectNETADeselectButton_clicked(bool checked)
{
	// Get element
	ReturnValue rv;
	if (!ui.SelectNETAElementButton->callPopupMethod("currentElement", rv)) return;
	
	if (rv.asInteger() == 0) Messenger::print("Invalid element '%s'", qPrintable(rv.asInteger()));
	else
	{
		CommandNode::run(Commands::DeSelectType, "ic", rv.asInteger(), qPrintable(ui.SelectNETACodeCombo->currentText()));

		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
	}
}

/*
 * Code
 */

void AtenWindow::on_SelectCodeSelectButton_clicked(bool checked)
{
	CommandNode::run(Commands::SelectType, "c", qPrintable(ui.SelectCodeCodeCombo->currentText()));

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_SelectCodeDeselectButton_clicked(bool checked)
{
	CommandNode::run(Commands::DeSelectType, "c", qPrintable(ui.SelectCodeCodeCombo->currentText()));

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::SelectPanelTarget);
}

// 	// Update selection text details
// 	// First label, total number of selected atoms.
// 	ui.SelectionLabel1->setText("Total selected : " + (m ? QString::number(m->nSelected()) : "0"));
// 	
// 	// Second label contains empirical formula of selection
// 	if (m) ui.SelectionLabel2->setText(m ? m->selectionEmpirical(false, true) : "--");
