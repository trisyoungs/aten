/*
	*** Qt context menu functions
	*** src/gui/contextmenu_funcs.cpp
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

// Local variables
atom *target = NULL;

// Show the modelview context menu
void gui_qt::call_atompopup(atom *undermouse, int x, int y)
{
	model *m = master.get_currentmodel();
	target = undermouse;
	if ((m->get_nselected() != 0) && (undermouse->is_selected())) target = NULL;
	QPoint pos(x,y);
	// If there are no atoms selected we must enable the menu first...
	if (m->get_nselected() == 0) mainwindow->ui.AtomMenu->setEnabled(TRUE);
	mainwindow->ui.AtomMenu->exec(pos);
	if (m->get_nselected() == 0) mainwindow->ui.AtomMenu->setEnabled(FALSE);
}

// Set atom style
void AtenForm::set_atomstyle(draw_style ds)
{
	if (target == NULL) master.get_currentmodel()->selection_set_style(ds);
	else target->set_style(ds);
	target = NULL;
}

// Set atom labels
void AtenForm::set_atomlabel(atom_label al)
{
	model *m = master.get_currentmodel();
	m->begin_undostate("Add Labels");
	if (target == NULL) m->selection_add_labels(al);
	else target->add_label(al);
	target = NULL;
	m->end_undostate();
}

// Clear atom labels
void AtenForm::remove_atomlabels(bool all)
{
	model *m = master.get_currentmodel();
	if (all)
	{
		m->begin_undostate("Clear All Labels");
		master.get_currentmodel()->clear_all_labels();
	}
	else
	{
		m->begin_undostate("Clear All Labels");
		master.get_currentmodel()->selection_clear_labels();
	}
	m->end_undostate();
	gui.refresh();
}

// Set atom labels
void AtenForm::set_atomhidden(bool hidden)
{
	model *m = master.get_currentmodel();
	if (target == NULL) m->selection_set_hidden(hidden);
	else m->set_hidden(target, hidden);
	target = NULL;
}
