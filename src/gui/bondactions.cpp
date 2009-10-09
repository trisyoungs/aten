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
#include "parser/commandnode.h"

void AtenForm::on_actionCalculateBonding_triggered(bool on)
{
	CommandNode::run(Command::ReBond, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionClearBonding_triggered(bool on)
{
	CommandNode::run(Command::ClearBonds, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionCalculateBondingSelection_triggered(bool on)
{
	CommandNode::run(Command::ReBondSelection, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionClearBondingSelection_triggered(bool on)
{
	CommandNode::run(Command::ClearSelectedBonds, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionAugmentBonding_triggered(bool on)
{
	CommandNode::run(Command::Augment, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::bondTolerance_valueChanged(double value)
{
	CommandNode::run(Command::BondTolerance, "d", value);
}
