/*
	*** Qt edit actions
	*** src/gui/editfuncs.cpp
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
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include <QtGui/QFileDialog>
#include <QtGui/QPixmap>
#include <QtGui/QMessageBox>

/*
// Editing Actions
*/

void AtenForm::on_actionEditUndo_triggered(bool checked)
{
	master.get_currentmodel()->undo();
	gui.refresh();
}

void AtenForm::on_actionEditRedo_triggered(bool checked)
{
	master.get_currentmodel()->redo();
	gui.refresh();
}

void AtenForm::on_actionEditCut_triggered(bool checked)
{
	// Cut the selected atoms from the model, copying to the paste buffer
	model *m = master.get_currentmodel();
	m->begin_undostate("Cut Selection");
	master.userclip.cut_selection(m);
	m->end_undostate();
	gui.refresh();
}

void AtenForm::on_actionEditCopy_triggered(bool checked)
{
	// Copy the selected atoms in the model into the paste buffer
	master.userclip.copy_selection(master.get_currentmodel());
	msg(DM_NONE,"%i atoms copied to clipboard.\n",master.userclip.get_natoms());
	msg(DM_VERBOSE, "Copied selection (%i atoms) from model %s\n",master.userclip.get_natoms(), master.get_currentmodel()->get_name());
}

void AtenForm::on_actionEditPaste_triggered(bool checked)
{
	// Paste the buffered atoms into the model
	model *m = master.get_currentmodel();
	m->begin_undostate("Paste");
	master.userclip.paste_to_model(m);
	m->end_undostate();
	gui.refresh();
}

void AtenForm::on_actionEditDelete_triggered(bool checked)
{
	// Delete the selected atoms in the model
	model *m = master.get_currentmodel();
	m->begin_undostate("Delete Selection");
	m->selection_delete();
	m->end_undostate();
	// Clear the main canvas' selection array to be on the safe side, since we might have deleted an atom in it!
	gui.mainview.clear_subsel();
	gui.refresh();
}

void AtenForm::on_actionEditSelectAll_triggered(bool checked)
{
	// Select all atoms in the current model
	model *m = master.get_currentmodel();
	m->begin_undostate("Select All");
	m->select_all();
	m->end_undostate();
	gui.refresh();
}

void AtenForm::on_actionEditSelectNone_triggered(bool checked)
{
	// Select all atoms in the current model
	model *m = master.get_currentmodel();
	m->begin_undostate("Select None");
	m->select_none();
	m->end_undostate();
	gui.refresh();
}

void AtenForm::on_actionEditInvert_triggered(bool checked)
{
	// Invert selection in the current model
	model *m = master.get_currentmodel();
	m->begin_undostate("Invert Selection");
	m->selection_invert();
	m->end_undostate();
	gui.refresh();
}

void AtenForm::on_actionEditSelectExpand_triggered(bool on)
{
	model *m = master.get_currentmodel();
	m->begin_undostate("Expand Selection");
	m->selection_expand();
	m->end_undostate();
	gui.refresh();
}

