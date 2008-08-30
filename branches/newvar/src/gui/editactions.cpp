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

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	aten.currentModel()->undo();
	gui.mainView.postRedisplay();
	gui.modelChanged();
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	aten.currentModel()->redo();
	gui.mainView.postRedisplay();
	gui.modelChanged();
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	// Cut the selected atoms from the model, copying to the paste buffer
	char s[128];
	Model *m = aten.currentModel()->renderSource();
	sprintf(s,"Cut %i atom%s\n",m->nSelected(),(m->nSelected() == 1 ? "" : "s"));
	m->beginUndoState(s);
	aten.userClipboard->cutSelection(m);
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	// Copy the selected atoms in the model into the paste buffer
	aten.userClipboard->copySelection(aten.currentModel()->renderSource());
	msg.print("%i atoms copied to clipboard.\n",aten.userClipboard->nAtoms());
	msg.print(Messenger::Verbose, "Copied selection (%i atoms) from model %s\n",aten.userClipboard->nAtoms(), aten.currentModel()->name());
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	// Paste the buffered atoms into the model
	char s[128];
	Model *m = aten.currentModel()->renderSource();
	sprintf(s,"Paste %i atom%s\n",aten.userClipboard->nAtoms(),(aten.userClipboard->nAtoms() == 1 ? "" : "s"));
	m->beginUndoState(s);
	aten.userClipboard->pasteToModel(m);
	m->endUndoState();
	gui.mainView.postRedisplay();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	// Delete the selected atoms in the model
	char s[128];
	// Clear the main canvas' selection array to be on the safe side, since we might have deleted an atom in it
	gui.mainView.clearPicked();
	Model *m = aten.currentModel()->renderSource();
	sprintf(s,"Delete %i atom%s\n",m->nSelected(),(m->nSelected() == 1 ? "" : "s"));
	m->beginUndoState(s);
	m->selectionDelete();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,TRUE);
}

void AtenForm::on_actionEditSelectAll_triggered(bool checked)
{
	// Select all atoms in the current model
	Model *m = aten.currentModel()->renderSource();
	m->beginUndoState("Select All");
	m->selectAll();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectNone_triggered(bool checked)
{
	// Select all atoms in the current model
	Model *m = aten.currentModel()->renderSource();
	m->beginUndoState("Select None");
	m->selectNone();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditInvert_triggered(bool checked)
{
	// Invert selection in the current model
	Model *m = aten.currentModel()->renderSource();
	m->beginUndoState("Invert Selection");
	m->selectionInvert();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenForm::on_actionEditSelectExpand_triggered(bool on)
{
	Model *m = aten.currentModel()->renderSource();
	m->beginUndoState("Expand Selection");
	m->selectionExpand();
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

