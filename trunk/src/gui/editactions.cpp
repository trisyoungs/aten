/*
	*** Qt edit actions
	*** src/gui/editfuncs.cpp
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
#include "classes/clipboard.h"

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	master.currentModel()->undo();
	gui.refresh();
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	master.currentModel()->redo();
	gui.refresh();
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	// Cut the selected atoms from the model, copying to the paste buffer
	char s[128];
	Model *m = master.currentModel();
	sprintf(s,"Cut %i atom%s\n",m->nSelected(),(m->nSelected() == 1 ? "" : "s"));
	m->beginUndostate(s);
	master.userClipboard->cutSelection(m);
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	// Copy the selected atoms in the model into the paste buffer
	master.userClipboard->copySelection(master.currentModel());
	msg(DM_NONE,"%i atoms copied to clipboard.\n",master.userClipboard->nAtoms());
	msg(DM_VERBOSE, "Copied selection (%i atoms) from model %s\n",master.userClipboard->nAtoms(), master.currentModel()->name());
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	// Paste the buffered atoms into the model
	char s[128];
	Model *m = master.currentModel();
	sprintf(s,"Paste %i atom%s\n",master.userClipboard->nAtoms(),(master.userClipboard->nAtoms() == 1 ? "" : "s"));
	m->beginUndostate(s);
	master.userClipboard->pasteToModel(m);
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	// Delete the selected atoms in the model
	char s[128];
	// Clear the main canvas' selection array to be on the safe side, since we might have deleted an atom in it
	gui.mainView.clearSubselection();
	Model *m = master.currentModel();
	sprintf(s,"Delete %i atom%s\n",m->nSelected(),(m->nSelected() == 1 ? "" : "s"));
	m->beginUndostate(s);
	m->selectionDelete();
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditSelectAll_triggered(bool checked)
{
	// Select all atoms in the current model
	Model *m = master.currentModel();
	m->beginUndostate("Select All");
	m->selectAll();
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditSelectNone_triggered(bool checked)
{
	// Select all atoms in the current model
	Model *m = master.currentModel();
	m->beginUndostate("Select None");
	m->selectNone();
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditInvert_triggered(bool checked)
{
	// Invert selection in the current model
	Model *m = master.currentModel();
	m->beginUndostate("Invert Selection");
	m->selectionInvert();
	m->endUndostate();
	gui.refresh();
}

void AtenForm::on_actionEditSelectExpand_triggered(bool on)
{
	Model *m = master.currentModel();
	m->beginUndostate("Expand Selection");
	m->selectionExpand();
	m->endUndostate();
	gui.refresh();
}

