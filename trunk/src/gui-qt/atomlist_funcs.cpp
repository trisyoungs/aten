/*
	*** Qt atomlist functions interface
	*** src/gui-qt/atomlist_funcs.cpp

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
#include "gui-qt/ttreewidgetatom.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"
#include <QtGui/QTreeWidget>
#include <QtCore/QMimeData>

// Local Variables
int liststructure_point = -1, listselection_point = -1;
model *list_lastmodel = NULL;
bool REFRESHING = FALSE;

/*
// TTreeWidgetAtom Functions
*/

TTreeWidgetAtom::TTreeWidgetAtom(QTreeWidgetItem *parent) : QTreeWidgetItem(parent)
{
	i = NULL;
}

void TTreeWidgetAtom::set_columns()
{
	if (i == NULL) printf("TTreeWidgetAtom::set_columns <<<< Atom has not yet been set.\n");
	else
	{
		setText(TWA_ID, itoa(i->get_id()));
		setText(TWA_ELEMENT, elements.symbol(i));
		setText(TWA_RX, ftoa(i->r.x));
		setText(TWA_RY, ftoa(i->r.y));
		setText(TWA_RZ, ftoa(i->r.z));
	}
}

/*
// Atom Tree List Management
*/

void AtenForm::on_AtomTreeList_itemSelectionChanged()
{
	if (REFRESHING) return;
	dbg_begin(DM_CALLS,"AtenForm::on_AtomTreeList_selectionChanged");
	// Selection has changed, so go through the reflist of TTreeWidgetAtoms and check their selection status
	model *m = master.get_currentmodel();
	atom *i;
	for (refitem<TTreeWidgetAtom> *ri = ui.AtomTreeList->get_atomitems(); ri != NULL; ri = ri->next)
	{
		//printf("atomitem = %li\n",ri->item);
		//printf("atomitem atom = %li\n", ri->item->get_atom());
		i = ri->item->get_atom();
		ri->item->isSelected() ? m->select_atom(i) : m->deselect_atom(i);
	}
	gui.mainview.postredisplay();
	dbg_end(DM_CALLS,"AtenForm::on_AtomTreeList_selectionChanged");
}

void AtenForm::refresh_atompage()
{
	dbg_begin(DM_CALLS,"AtenForm::refresh_atompage");
	// If the atom list page is not visible, don't do anything
	if (!ui.ShowAtomPageButton->isChecked())
	{
		dbg_end(DM_CALLS,"AtenForm::refresh_atompage");
		return;
	}
	// Check stored log point against 'structure' and 'visual' log points in model to see if we need to refresh the list
	REFRESHING = TRUE;
	pattern *p;
	TTreeWidgetAtom *item;
	refitem<TTreeWidgetAtom> *ri;
	atom *i;
	int n;
	model *m = master.get_currentmodel();
	// Check this model against the last one we represented in the list
	if (m != list_lastmodel)
	{
		liststructure_point = -1;
		listselection_point = -1;
	}
	list_lastmodel = m;
	if (liststructure_point != (m->get_log(LOG_STRUCTURE) + m->get_log(LOG_COORDS)))
	{
		// Clear the current list
		ui.AtomTreeList->clear();
		ui.AtomTreeList->clear_atomitems();
		// If there are no atoms in the current model, exit now.
		if (m->get_natoms() == 0)
		{
			dbg_end(DM_CALLS,"AtenForm::refresh_atompage");
			REFRESHING = FALSE;
			return;
		}
		// Add patterns as root nodes in the list, followed by atoms in each pattern.
		// If no patterns are yet defined, store them in a generic rootnode.
		if (m->get_npatterns() == 0)
		{
			// Create new root node for all atoms
			QTreeWidgetItem *pat = new QTreeWidgetItem(ui.AtomTreeList);
			ui.AtomTreeList->setItemExpanded(pat, TRUE);
			pat->setText(0, tr("All"));
			for (i = m->get_atoms(); i != NULL; i = i->next)
			{
				// Add the atom
				item = ui.AtomTreeList->addTreeItem(pat);
				item->set_atom(i);
				item->set_columns();
				// Set the row selection property here.
				ui.AtomTreeList->setItemSelected(item, i->is_selected());
			}
		}
		else
		{
			// Get pointer to first atom in model. We'll skip through it numerically in each pattern
			i = m->get_atoms();
			for (p = m->get_patterns(); p != NULL; p = p->next)
			{
				// Create new root node for the pattern
				QTreeWidgetItem *pat = new QTreeWidgetItem(ui.AtomTreeList);
				ui.AtomTreeList->setItemExpanded(pat, TRUE);
				pat->setText(0, p->get_name());
				for (n = 0; n<p->get_totalatoms(); n++)
				{
					// Create atom in the pattern root node
					item = ui.AtomTreeList->addTreeItem(pat);
					item->set_atom(i);
					item->set_columns();
					// Set the row selection property here
					ui.AtomTreeList->setItemSelected(item, i->is_selected());
					i = i->next;
				}
			}
		}
		// Set new log points
		liststructure_point = m->get_log(LOG_STRUCTURE) + m->get_log(LOG_COORDS);
		listselection_point = m->get_log(LOG_SELECTION);
	}
	else if (listselection_point != m->get_log(LOG_SELECTION))
	{
		// If we haven't cleared and repopulated the list and the selection point is old, go through the list and apply the new atom selection
		// Grab the list of TTreeWidgetAtoms
		for (ri = ui.AtomTreeList->get_atomitems(); ri != NULL; ri = ri->next)
		{
			i = ri->item->get_atom();
			ui.AtomTreeList->setItemSelected(ri->item, i->is_selected());
		}
		listselection_point = m->get_log(LOG_SELECTION);
	}
	REFRESHING = FALSE;
	dbg_end(DM_CALLS,"AtenForm::refresh_atompage");
}
