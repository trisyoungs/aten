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

#include "base/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

/*
// Model Menu Actions
*/

void AtenForm::on_actionFFType_triggered(bool checked)
{
	aten.currentModel()->typeAll();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionFFUntype_triggered(bool checked)
{
	aten.currentModel()->removeTyping();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionFoldAtoms_triggered(bool checked)
{
	Model *m = aten.currentModel();
	char s[128];
	sprintf(s,"Fold all atoms");
	m->beginUndostate(s);
	m->foldAllAtoms();
	m->endUndostate();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionFoldMolecules_triggered(bool checked)
{
	Model *m = aten.currentModel();
	char s[128];
	sprintf(s,"Fold all molecules");
	m->beginUndostate(s);
	m->foldAllMolecules();
	m->endUndostate();
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
	gui.updateTrajControls();
}

void AtenForm::on_actionModelPrevious_triggered(bool checked)
{
	// Get current ID of modeltabs, decrease it, and check we're still within range
	int newid = ui.ModelTabs->currentIndex();
	newid --;
	if (newid < 0) newid = aten.nModels() - 1;
	// Activate new model tab
	ui.ModelTabs->setCurrentIndex(newid);
	gui.updateTrajControls();
}

void AtenForm::on_actionModelShowAll_triggered(bool checked)
{
	// Make all atoms in model visible
	Model *m = aten.currentModel();
	for (Atom *i = m->atoms(); i != NULL; i = i->next) m->setHidden(i, FALSE);
	m->logChange(Change::VisualLog);
}

void AtenForm::on_actionModelRename_triggered(bool checked)
{
	Model *m = aten.currentModel();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		m->setName(qPrintable(text));
		refreshModelTabs();
	}
}
