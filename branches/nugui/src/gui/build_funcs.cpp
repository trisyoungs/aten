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
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
BuildWidget::BuildWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
BuildWidget::~BuildWidget()
{
}

// Show window
void BuildWidget::showWidget()
{
	show();
}

/*
// Edit Tab - Draw
*/
void BuildWidget::on_ElementHButton_clicked(bool checked)
{
}

void BuildWidget::on_ElementCButton_clicked(bool checked)
{
}

void BuildWidget::on_ElementNButton_clicked(bool checked)
{
}

void BuildWidget::on_ElementOButton_clicked(bool checked)
{
}

void BuildWidget::on_ElementCustomButton_clicked(bool checked)
{
}

void BuildWidget::on_ElementPickButton_clicked(bool checked)
{
}

void BuildWidget::on_DrawAddHModelButton_clicked(bool checked)
{
}

void BuildWidget::on_DrawTransmuteSelectionButton_clicked(bool checked)
{
}

/*
// Edit Tab - Bond
*/

void BuildWidget::on_DrawRebondButton_clicked(bool checked)
{
	CommandNode::run(Command::ReBond, "");
	gui.update(FALSE,FALSE,FALSE);
}

void BuildWidget::on_DrawClearBondingButton_clicked(bool checked)
{
	CommandNode::run(Command::ClearBonds, "");
	gui.update(FALSE,FALSE,FALSE);
}

void BuildWidget::on_DrawAugmentButton_clicked(bool checked)
{
	CommandNode::run(Command::Augment, "");
	gui.update(FALSE,FALSE,FALSE);
}

void BuildWidget::on_DrawRebondSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::ReBondSelection, "");
	gui.update(FALSE,FALSE,FALSE);
}

void BuildWidget::on_DrawClearSelectionButton_clicked(bool checked)
{
	CommandNode::run(Command::ClearSelectedBonds, "");
	gui.update(FALSE,FALSE,FALSE);
}

/*
// Tools Tab - Add Atom
*/

void BuildWidget::on_AddAtomButton_clicked(bool on)
{
	if (ui.AddAtomFractionalCheck->isChecked())
	{
		CommandNode::run(Command::NewAtomFrac, "iddd", gui.mainWidget->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	else
	{
		CommandNode::run(Command::NewAtom, "iddd", gui.mainWidget->sketchElement(), ui.AtomXCoordSpin->value(), ui.AtomYCoordSpin->value(), ui.AtomZCoordSpin->value());
	}
	gui.update();
}

void BuildWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.BuildButton->setChecked(FALSE);
	event->accept();
}
