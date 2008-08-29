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

#include "aten/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

/*
// Model Menu Actions
*/

void AtenForm::on_actionModelFFType_triggered(bool checked)
{
	aten.currentModel()->typeAll();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionModelFFUntype_triggered(bool checked)
{
	aten.currentModel()->removeTyping();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionModelFoldAtoms_triggered(bool checked)
{
	Model *m = aten.currentModel();
	m->beginUndoState("Fold all atoms");
	m->foldAllAtoms();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionModelFoldMolecules_triggered(bool checked)
{
	Model *m = aten.currentModel();
	m->beginUndoState("Fold all molecules");
	m->foldAllMolecules();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionModelNext_triggered(bool checked)
{
	// Get current ID of modeltabs, increase it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid ++;
	if (newid > (aten.nModels() - 1)) newid = 0;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}

void AtenForm::on_actionModelPrevious_triggered(bool checked)
{
	// Get current ID of modeltabs, decrease it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid --;
	if (newid < 0) newid = aten.nModels() - 1;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
}

void AtenForm::on_actionModelShowAll_triggered(bool checked)
{
	// Make all atoms in model visible
	Model *m = aten.currentModel();
	for (Atom *i = m->atoms(); i != NULL; i = i->next) m->setHidden(i, FALSE);
	m->changeLog.add(Log::Visual);
}

void AtenForm::on_actionModelRename_triggered(bool checked)
{
	Model *m = aten.currentModel();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		m->beginUndoState("Rename Model");
		m->setName(qPrintable(text));
		m->endUndoState();
		refreshModelTabs();
		gui.updateWindowTitle();
	}
}
