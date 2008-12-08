/*
	*** Qt GUI: Editing actions
	*** src/gui/editactions.cpp
	Copyright T. Youngs 2007,2008

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
#include "command/staticcommand.h"

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_UNDO,"");
	cmd.execute();
	gui.mainView.postRedisplay();
	gui.modelChanged();
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_REDO,"");
	cmd.execute();
	gui.mainView.postRedisplay();
	gui.modelChanged();
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_CUT,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_COPY,"");
	cmd.execute();
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_PASTE,"");
	cmd.execute();
	gui.mainView.postRedisplay();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_DELETE,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditSelectAll_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SELECTALL,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectNone_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SELECTNONE,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditInvert_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_INVERT,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectExpand_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_EXPAND,"");
	cmd.execute();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

