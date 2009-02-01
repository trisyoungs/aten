/*
	*** Qt GUI: Bond toolbar actions
	*** src/gui/bondactions.cpp
	Copyright T. Youngs 2007-2009

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
#include "gui/gui.h"
#include "model/model.h"
#include "command/staticcommand.h"

void AtenForm::on_actionCalculateBonding_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_REBOND, "");
	cmd.execute();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionClearBonding_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_CLEARBONDS, "");
	cmd.execute();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionCalculateBondingSelection_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_REBONDSELECTION, "");
	cmd.execute();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionClearBondingSelection_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_CLEARSELECTEDBONDS, "");
	cmd.execute();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionAugmentBonding_triggered(bool on)
{
	static StaticCommandNode cmd(Command::CA_AUGMENT, "");
	cmd.execute();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::bondTolerance_valueChanged(double value)
{
	static StaticCommandNode cmd(Command::CA_BONDTOLERANCE, "d", 1.1);
	cmd.pokeArguments("d", value);
	cmd.execute();
}
