/*
	*** Qt model actions
	*** src/gui/modelactions.cpp
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

#include "base/master.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

/*
// Model Menu Actions
*/

void AtenForm::on_actionFFType_triggered(bool checked)
{
	master.currentModel()->typeAll();
	gui.refresh();
}

void AtenForm::on_actionFFUntype_triggered(bool checked)
{
	master.currentModel()->removeTyping();
	gui.refresh();
}

void AtenForm::on_actionFoldAtoms_triggered(bool checked)
{
	master.currentModel()->foldAllAtoms();
	gui.refresh();
}

void AtenForm::on_actionFoldMolecules_triggered(bool checked)
{
	master.currentModel()->foldAllMolecules();
	gui.refresh();
}

void AtenForm::on_actionModelNext_triggered(bool checked)
{
	// Get current ID of modeltabs, increase it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid ++;
	if (newid > (master.nModels() - 1)) newid = 0;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}

void AtenForm::on_actionModelPrevious_triggered(bool checked)
{
	// Get current ID of modeltabs, decrease it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid --;
	if (newid < 0) newid = master.nModels() - 1;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}

void AtenForm::on_actionModelShowAll_triggered(bool checked)
{
	// Make all atoms in model visible
	Model *m = master.currentModel();
	for (Atom *i = m->atoms(); i != NULL; i = i->next)
		m->setHidden(i, FALSE);
	m->logChange(LOG_VISUAL);
}
