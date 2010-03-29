/*
	*** Qt GUI: Editing actions
	*** src/gui/editactions.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "parser/commandnode.h"

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	CommandNode::run(Command::Undo, "");
	gui.mainView.postRedisplay();
	gui.update();
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	CommandNode::run(Command::Redo, "");
	gui.mainView.postRedisplay();
	gui.update();
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	CommandNode::run(Command::Cut, "");
	gui.update(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	CommandNode::run(Command::Copy, "");
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	CommandNode::run(Command::Paste, "");
	gui.mainView.postRedisplay();
	gui.update(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	CommandNode::run(Command::Delete, "");
	gui.update(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditSelectAll_triggered(bool checked)
{
	CommandNode::run(Command::SelectAll, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectNone_triggered(bool checked)
{
	CommandNode::run(Command::SelectNone, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditInvert_triggered(bool checked)
{
	CommandNode::run(Command::Invert, "");
	gui.update(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectExpand_triggered(bool on)
{
	CommandNode::run(Command::Expand, "");
	gui.update(TRUE,FALSE,FALSE);
}

