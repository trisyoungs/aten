/*
	*** Qt model actions
	*** src/gui-qt/action_funcs.cpp
	Copyright T. Youngs 2007

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
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"

/*
// Model Menu Actions
*/

void AtenForm::on_actionFFType_triggered(bool checked)
{
	master.get_currentmodel()->type_all();
	gui.refresh();
}

void AtenForm::on_actionFFUntype_triggered(bool checked)
{
	master.get_currentmodel()->remove_typing();
	gui.refresh();
}

void AtenForm::on_actionFoldAtoms_triggered(bool checked)
{
	master.get_currentmodel()->fold_all_atoms();
	gui.refresh();
}

void AtenForm::on_actionFoldMolecules_triggered(bool checked)
{
	master.get_currentmodel()->fold_all_molecules();
	gui.refresh();
}

void AtenForm::on_actionModelNext_triggered(bool checked)
{
	// Get current ID of modeltabs, increase it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid ++;
	if (newid > (master.get_nmodels() - 1)) newid = 0;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}

void AtenForm::on_actionModelPrevious_triggered(bool checked)
{
	// Get current ID of modeltabs, decrease it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid --;
	if (newid < 0) newid = master.get_nmodels() - 1;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}
