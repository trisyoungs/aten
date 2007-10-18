/*
	*** Qt edit functions interface
	*** src/gui-qt/edit_funcs.cpp

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

#include "base/master.h"
#include "base/elements.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"

void AtenForm::on_SelectExpandButton_clicked(bool on)
{
	master.get_currentmodel()->selection_expand();
	gui.refresh();
}

void AtenForm::on_ElementUserButton_clicked(bool on)
{
	master.sketchelement = elements.find(qPrintable(ui.ElementUserButton->text()));
}

void AtenForm::on_BondCalcButton_clicked(bool on)
{
	master.get_currentmodel()->clear_bonding();
	master.get_currentmodel()->calculate_bonding();
	gui.refresh();
}

void AtenForm::on_BondClearButton_clicked(bool on)
{
	master.get_currentmodel()->clear_bonding();
	gui.refresh();
}

void AtenForm::on_BondCalcSelButton_clicked(bool on)
{
	master.get_currentmodel()->selection_calculate_bonding();
	gui.refresh();
}

void AtenForm::on_BondClearSelButton_clicked(bool on)
{
	master.get_currentmodel()->selection_clear_bonding();
	gui.refresh();
}

void AtenForm::on_BondAugmentButton_clicked(bool on)
{
	master.get_currentmodel()->augment_bonding();
	gui.refresh();
}
