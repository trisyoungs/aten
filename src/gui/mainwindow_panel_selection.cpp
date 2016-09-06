/*
	*** Main Window - Selection Panel Functions
	*** src/gui/mainwindow_panel_selection.cpp
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

// Update selection panel
void AtenWindow::updateSelectionPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateSelectionPanel");

	Messenger::exit("AtenWindow::updateSelectionPanel");
}

/*
 * Appearance
 */

void AtenWindow::on_SelectionAppearanceStyleButton_clicked(bool checked)
{
	// Get current style selected in popup widget
	ReturnValue rv;
	if (!ui.SelectionAppearanceStyleButton->callPopupMethod("currentStyle", rv)) return;

	// Apply the selected style to the current atom selection
	CommandNode::run(Commands::AtomStyle, "c", qPrintable(rv.asString()));

	updateWidgets();
}

void AtenWindow::on_SelectionAppearanceColourButton_clicked(bool checked)
{
	// Get current colour selected in popup widget
	ReturnValue rv;
	if (!ui.SelectionAppearanceColourButton->callPopupMethod("currentColour", rv)) return;

	// Apply the selected style to the current atom selection
	bool success;
	CommandNode::run(Commands::ColourAtoms, "dddd", rv.asDouble(0, success), rv.asDouble(1, success), rv.asDouble(2, success), rv.asDouble(3, success));

	updateWidgets();
}

void AtenWindow::on_SelectionAppearanceResetToElementButton_clicked(bool checked)
{
	CommandNode::run(Commands::RecolourAtoms, "");

	updateWidgets();
}

void AtenWindow::on_SelectionAppearanceHideButton_clicked(bool checked)
{
	CommandNode::run(Commands::Hide, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

/*
 * Label
 */

void AtenWindow::on_SelectionLabelIDButton_clicked(bool checked)
{
	CommandNode::run(Commands::Label, "c", Atom::atomLabel(Atom::IdLabel));

	updateWidgets();
}

void AtenWindow::on_SelectionLabelElementButton_clicked(bool checked)
{
	CommandNode::run(Commands::Label, "c", Atom::atomLabel(Atom::ElementLabel));

	updateWidgets();
}

void AtenWindow::on_SelectionLabelChargeButton_clicked(bool checked)
{
	CommandNode::run(Commands::Label, "c", Atom::atomLabel(Atom::ChargeLabel));

	updateWidgets();
}

void AtenWindow::on_SelectionLabelTypeButton_clicked(bool checked)
{
	CommandNode::run(Commands::Label, "c", Atom::atomLabel(Atom::TypeLabel));

	updateWidgets();
}

void AtenWindow::on_SelectionLabelEquivalentButton_clicked(bool checked)
{
	CommandNode::run(Commands::Label, "c", Atom::atomLabel(Atom::EquivalentLabel));

	updateWidgets();
}

void AtenWindow::on_SelectionLabelClearButton_clicked(bool checked)
{
	CommandNode::run(Commands::RemoveLabels, "");

	updateWidgets();
}

/*
 * Position
 */

void AtenWindow::on_SelectionPositionFixButton_clicked(bool checked)
{
	CommandNode::run(Commands::Fix, "");

	updateWidgets();
}

void AtenWindow::on_SelectionPositionFreeButton_clicked(bool checked)
{
	CommandNode::run(Commands::Free, "");

	updateWidgets();
}

void AtenWindow::on_SelectionPositionSetViewOriginButton_clicked(bool checked)
{
	Model* viewTarget = aten_.currentModelOrFrame();

	viewTarget->setViewOrigin(viewTarget->selectionCentreOfGeometry());

	updateWidgets();
}

