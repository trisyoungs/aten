/*
	*** Build Dock Widget
	*** src/gui/build_funcs.cpp
	Copyright T. Youngs 2007-2011

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
	
	// Add items to submenus for Rebond and Clear buttons
	ui.DrawRebondMenuButton->addMenuItem("Model (no augment)", BuildWidget::ModelNoAugmentItem);
	ui.DrawRebondMenuButton->addMenuItem("Selection", BuildWidget::SelectionItem);
	ui.DrawRebondMenuButton->addMenuItem("Selection (no augment)", BuildWidget::SelectionNoAugmentItem);
	ui.DrawRebondMenuButton->addMenuItem("Within Patterns", BuildWidget::PatternsItem);
	ui.DrawRebondMenuButton->addMenuItem("Within Patterns (no augment)", BuildWidget::PatternsNoAugmentItem);
	ui.DrawClearBondingMenuButton->addMenuItem("Selection", 0);
	
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
	if (checked) gui.mainWidget()->setSketchElement(1);
}

void BuildWidget::on_ElementCButton_clicked(bool checked)
{
	if (checked) gui.mainWidget()->setSketchElement(6);
}

void BuildWidget::on_ElementNButton_clicked(bool checked)
{
	if (checked) gui.mainWidget()->setSketchElement(7);
}

void BuildWidget::on_ElementOButton_clicked(bool checked)
{
	if (checked) gui.mainWidget()->setSketchElement(8);
}

void BuildWidget::on_ElementCustomButton_clicked(bool checked)
{
	if (checked) gui.mainWidget()->setSketchElement(customElement_);
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
		gui.mainWidget()->setSketchElement(customElement_);
	}
}

void BuildWidget::on_DrawAddHModelButton_clicked(bool checked)
{
	CommandNode::run(Command::AddHydrogen, "");
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void BuildWidget::on_DrawTransmuteSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::Transmute, "i", gui.mainWidget()->sketchElement());
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
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
			CommandNode::run(Command::ReBondSelection, "");
			break;
		case (BuildWidget::SelectionNoAugmentItem):
			CommandNode::run(Command::ReBondSelection, "i", 1);
			break;
		case (BuildWidget::PatternsItem):
			CommandNode::run(Command::ReBondPatterns, "");
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
		CommandNode::run(Command::NewAtomFrac, "iddd", gui.mainWidget()->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	else
	{
		CommandNode::run(Command::NewAtom, "iddd", gui.mainWidget()->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	gui.update(GuiQt::CanvasTarget);
}

void BuildWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.BuildButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();

	// Return to select mode if one of the modes in this window is still selected
	if (UserAction::isBuildWidgetAction(gui.mainWidget()->selectedMode())) gui.mainWindow()->cancelCurrentMode();

	event->accept();
}
