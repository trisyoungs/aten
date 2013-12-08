/*
	*** Build Dock Widget
	*** src/gui/build_funcs.cpp
	Copyright T. Youngs 2007-2013

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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/build.h"
#include "gui/toolbox.h"
#include "gui/selectelement.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
BuildWidget::BuildWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	// Set up interface
	ui.setupUi(this);
	
	// Create a subgroup for the element select buttons
	QButtonGroup *elementGroup = new QButtonGroup(this);
	elementGroup->addButton(ui.ElementHButton);
	elementGroup->addButton(ui.ElementCButton);
	elementGroup->addButton(ui.ElementNButton);
	elementGroup->addButton(ui.ElementOButton);
	elementGroup->addButton(ui.ElementCustomButton);
	ui.ElementCButton->setChecked(TRUE);
	
	// Add items to submenus for buttons that need it
	// -- Grow Atom
	ui.DrawGrowMenuButton->addMenuItem("Selection", 0);
	// -- Transmute
	ui.DrawTransmuteMenuButton->addMenuItem("Selection", 0);
	// -- Add Hydrogen
	ui.DrawAddHMenuButton->addMenuItem("Selection", 0);
	ui.DrawAddHMenuButton->addMenuItem("Model", 1);
	// -- Rebond
	ui.DrawRebondMenuButton->addMenuItem("Model (no augment)", BuildWidget::ModelNoAugmentItem);
	ui.DrawRebondMenuButton->addMenuItem("Selection", BuildWidget::SelectionItem);
	ui.DrawRebondMenuButton->addMenuItem("Selection (no augment)", BuildWidget::SelectionNoAugmentItem);
	ui.DrawRebondMenuButton->addMenuItem("Within Patterns", BuildWidget::PatternsItem);
	ui.DrawRebondMenuButton->addMenuItem("Within Patterns (no augment)", BuildWidget::PatternsNoAugmentItem);
	// -- Clear Bonding
	ui.DrawClearBondingMenuButton->addMenuItem("Selection", 0);

	// Add geometry types to combo box, and select 'tetrahedral'
	for (int n = 0; n < Atom::nAtomGeometries; ++n) ui.DrawGrowGeometryCombo->addItem(Atom::atomGeometry( (Atom::AtomGeometry) n));
	ui.DrawGrowGeometryCombo->setCurrentIndex(Atom::TetrahedralGeometry);
	
	// Private variables
	customElement_ = 9;
}

// Show window
void BuildWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.BuildButton->setChecked(TRUE);
}

/*
// Edit Tab - Draw
*/

void BuildWidget::on_ElementHButton_clicked(bool checked)
{
	if (checked) gui.mainCanvas()->setSketchElement(1);
}

void BuildWidget::on_ElementCButton_clicked(bool checked)
{
	if (checked) gui.mainCanvas()->setSketchElement(6);
}

void BuildWidget::on_ElementNButton_clicked(bool checked)
{
	if (checked) gui.mainCanvas()->setSketchElement(7);
}

void BuildWidget::on_ElementOButton_clicked(bool checked)
{
	if (checked) gui.mainCanvas()->setSketchElement(8);
}

void BuildWidget::on_ElementCustomButton_clicked(bool checked)
{
	if (checked) gui.mainCanvas()->setSketchElement(customElement_);
}

void BuildWidget::on_ElementPickButton_clicked(bool checked)
{
	// Call the select element dialog...
	int newel = gui.selectElementDialog->selectElement();
	if (newel != -1)
	{
		// Set text of custom element button
		ui.ElementCustomButton->setText( elements().symbol(newel) );
		customElement_ = newel;
		// Activate custom element button
		ui.ElementCustomButton->setChecked(TRUE);
		gui.mainCanvas()->setSketchElement(customElement_);
	}
}

void BuildWidget::on_DrawGrowMenuButton_menuItemClicked(int menuItemId)
{
	// Get atom geometry from combo box
	Atom::AtomGeometry ag = (Atom::AtomGeometry) ui.DrawGrowGeometryCombo->currentIndex();
	switch (menuItemId)
	{
		case (0):
			CommandNode::run(Command::SelectionGrowAtom, "ic", gui.mainCanvas()->sketchElement(), Atom::atomGeometry(ag));
			break;
		default:
			printf("BuildWidget - Unhandled menu item from DrawGrowMenuButton.\n");
			break;
	}
	gui.update(GuiQt::CanvasTarget);
}

void BuildWidget::on_DrawTransmuteMenuButton_menuItemClicked(int menuItemId)
{
	switch (menuItemId)
	{
		case (0):
		CommandNode::run(Command::Transmute, "i", gui.mainCanvas()->sketchElement());
			break;
		default:
			printf("BuildWidget - Unhandled menu item from DrawTransmuteMenuButton.\n");
			break;
	}
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void BuildWidget::on_DrawAddHMenuButton_menuItemClicked(int menuItemId)
{
	switch (menuItemId)
	{
		case (0):
			CommandNode::run(Command::SelectionAddHydrogen, "");
			break;
		case (1):
			CommandNode::run(Command::AddHydrogen, "");
			break;
		default:
			printf("BuildWidget - Unhandled menu item from DrawAddHMenuButton.\n");
			break;
	}
	gui.update(GuiQt::AllTarget);
}

/*
// Edit Tab - Bond
*/

void BuildWidget::on_DrawRebondButton_clicked(bool checked)
{
	CommandNode::run(Command::ReBond, "");
	gui.update(GuiQt::CanvasTarget);
}

void BuildWidget::on_DrawClearBondingButton_clicked(bool checked)
{
	CommandNode::run(Command::ClearBonds, "");
	gui.update(GuiQt::CanvasTarget);
}

void BuildWidget::on_DrawRebondMenuButton_menuItemClicked(int menuItemId)
{
	switch (menuItemId)
	{
		case (BuildWidget::ModelNoAugmentItem):
			CommandNode::run(Command::ReBond, "i", 0);
			break;
		case (BuildWidget::SelectionItem):
			CommandNode::run(Command::ReBondSelection, "i", 0);
			break;
		case (BuildWidget::SelectionNoAugmentItem):
			CommandNode::run(Command::ReBondSelection, "i", 1);
			break;
		case (BuildWidget::PatternsItem):
			CommandNode::run(Command::ReBondPatterns, "i", 0);
			break;
		case (BuildWidget::PatternsNoAugmentItem):
			CommandNode::run(Command::ReBondPatterns, "i", 1);
			break;
		default:
			printf("BuildWidget - Unhandled menu item from DrawRebondButton.\n");
			break;
	}
	gui.update(GuiQt::CanvasTarget);
}

void BuildWidget::on_DrawClearBondingMenuButton_menuItemClicked(int menuItemId)
{
	// Only one action to consider,,.
	if (menuItemId == 0)
	{
		CommandNode::run(Command::ClearSelectedBonds, "");
		gui.update(GuiQt::CanvasTarget);
	}
	else printf("BuildWidget - Unhandled menu item from DrawRebondButton.\n");
}

void BuildWidget::on_DrawAugmentButton_clicked(bool checked)
{
	CommandNode::run(Command::Augment, "");
	gui.update(GuiQt::CanvasTarget);
}

/*
// Tools Tab - Add Atom
*/

void BuildWidget::on_AddAtomButton_clicked(bool on)
{
	if (ui.AddAtomFractionalCheck->isChecked())
	{
		CommandNode::run(Command::NewAtomFrac, "iddd", gui.mainCanvas()->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	else
	{
		CommandNode::run(Command::NewAtom, "iddd", gui.mainCanvas()->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	gui.update(GuiQt::CanvasTarget);
}

/*
// Options Tab
*/

void BuildWidget::on_BondToleranceSlider_valueChanged(int value)
{
	// Convert integer value to real value
	double tolerance = double(value)/1000.0;
	// Set spinbox
	ui.BondToleranceSpin->setValue(tolerance);
	// Set prefs value
	prefs.setBondTolerance(tolerance);
}

void BuildWidget::on_BondToleranceSpin_valueChanged(double value)
{
	// Convert integer value to real value
	int tolerance = int(value*1000.0);
	// Set slider
	ui.BondToleranceSlider->setValue(tolerance);
	// Set prefs value
	prefs.setBondTolerance(value);
}

void BuildWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.BuildButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainCanvas()->postRedisplay();

	// Return to select mode if one of the modes in this window is still selected
	if (UserAction::isBuildWidgetAction(gui.mainCanvas()->selectedMode())) gui.mainWindow()->cancelCurrentMode();

	event->accept();
}
