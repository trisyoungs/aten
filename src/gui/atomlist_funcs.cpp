/*
	*** Qt atomlist functions interface
	*** src/gui/atomlist_funcs.cpp
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

#include "classes/pattern.h"
#include "gui/ttreewidgetitem.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"
#include "base/master.h"
#include <QtGui/QTreeWidget>
#include <QtGui/QScrollBar>

// Local Variables
int listStructurePoint = -1, listSelectionPoint = -1;
Model *listLastModel = NULL;
bool REFRESHING = FALSE;
int listPosition;

/*
// Atom Tree List Management
*/

void AtenForm::on_AtomTree_itemSelectionChanged()
{
	if (REFRESHING) return;
	dbgBegin(Debug::Calls,"AtenForm::on_AtomTree_selectionChanged");
	//printf("AtenForm:: atom selection has changed...\n");
	// Selection has changed, so go through the Reflist of TTreeWidgetItems and check their selection status
	Model *m = master.currentModel();
	Atom *i;
	for (Refitem<TTreeWidgetItem,int> *ri = ui.AtomTree->atomItems(); ri != NULL; ri = ri->next)
	{
		//printf("atomitem = %li\n",ri->item);
		//printf("atomitem atom = %li\n", ri->item->atom());
		i = ri->item->atom();
		ri->item->isSelected() ? m->selectAtom(i) : m->deselectAtom(i);
	}
	gui.modelChanged();
	dbgEnd(Debug::Calls,"AtenForm::on_AtomTree_selectionChanged");
}

void AtenForm::refreshAtomPage()
{
	dbgBegin(Debug::Calls,"AtenForm::refreshAtomPage");
	// If the atom list page is not visible, don't do anything
	if (!ui.ShowAtomPageButton->isChecked())
	{
		dbgEnd(Debug::Calls,"AtenForm::refreshAtomPage");
		return;
	}
	// Check stored log point against 'structure' and 'visual' log points in model to see if we need to refresh the list
	REFRESHING = TRUE;
	//printf("Refreshing atompage.....\n");
	Pattern *p;
	TTreeWidgetItem *item;
	Refitem<TTreeWidgetItem,int> *ri;
	Atom *i;
	int n;
	Model *m = master.currentModel();
	// Check this model against the last one we represented in the list
	if (m != listLastModel)
	{
		listStructurePoint = -1;
		listSelectionPoint = -1;
	}
	listLastModel = m;
	if (listStructurePoint != (m->log(LOG_STRUCTURE) + m->log(LOG_COORDS)))
	{
		//printf("List must be cleared and repopulated...\n");
		// Clear the current list
		ui.AtomTree->clear();
		ui.AtomTree->clearAtomItems();
		// If there are no atoms in the current model, exit now.
		if (m->nAtoms() == 0)
		{
			dbgEnd(Debug::Calls,"AtenForm::refreshAtomPage");
			REFRESHING = FALSE;
			return;
		}
		// Add patterns as root nodes in the list, followed by atoms in each pattern.
		// If no patterns are yet defined, store them in a generic rootnode.
		if (m->nPatterns() == 0)
		{
			// Create new root node for all atoms
			QTreeWidgetItem *pat = new QTreeWidgetItem(ui.AtomTree);
			ui.AtomTree->setItemExpanded(pat, TRUE);
			pat->setText(0, tr("All"));
			for (i = m->atoms(); i != NULL; i = i->next)
			{
				// Add the atom
				item = ui.AtomTree->addTreeItem(pat);
				//item->setFlags(Qt::ItemIsEditable | Qt::ItemIsSelectable | Qt::ItemIsEnabled);
				item->setAtom(i);
				item->setAtomColumns();
				// Set the row selection property here.
				ui.AtomTree->setItemSelected(item, i->isSelected());
			}
		}
		else
		{
			// Get pointer to first atom in model. We'll skip through it numerically in each pattern
			i = m->atoms();
			for (p = m->patterns(); p != NULL; p = p->next)
			{
				// Create new root node for the pattern
				QTreeWidgetItem *pat = new QTreeWidgetItem(ui.AtomTree);
				ui.AtomTree->setItemExpanded(pat, TRUE);
				pat->setText(0, p->name());
				for (n = 0; n<p->totalAtoms(); n++)
				{
					// Create atom in the pattern root node
					item = ui.AtomTree->addTreeItem(pat);
					//item->setFlags(Qt::ItemIsEditable | Qt::ItemIsSelectable | Qt::ItemIsEnabled);
					item->setAtom(i);
					item->setAtomColumns();
					// Set the row selection property here
					ui.AtomTree->setItemSelected(item, i->isSelected());
					i = i->next;
				}
			}
		}
		// Set new log points
		listStructurePoint = m->log(LOG_STRUCTURE) + m->log(LOG_COORDS);
		listSelectionPoint = m->log(LOG_SELECTION);
	}
	else if (listSelectionPoint != m->log(LOG_SELECTION))
	{
		// If we haven't cleared and repopulated the list and the selection point is old, go through the list and apply the new atom selection
		// Grab the list of TTreeWidgetItems
		//printf("Just updating selection....\n");
		for (ri = ui.AtomTree->atomItems(); ri != NULL; ri = ri->next)
		{
			i = ri->item->atom();
			ui.AtomTree->setItemSelected(ri->item, i->isSelected());
		}
		listSelectionPoint = m->log(LOG_SELECTION);
	}
	for (n=0; n<6; n++) ui.AtomTree->resizeColumnToContents(n);
	REFRESHING = FALSE;
	dbgEnd(Debug::Calls,"AtenForm::refreshAtomPage");
}

void AtenForm::peekScrollBar()
{
	listPosition = ui.AtomTree->verticalScrollBar()->sliderPosition();
}

void AtenForm::pokeScrollBar()
{
	ui.AtomTree->verticalScrollBar()->setSliderPosition(listPosition);
}

void AtenForm::on_ShiftUpButton_clicked(bool checked)
{
	Model *m = master.currentModel();
	m->beginUndostate("Shift Selection Up");
	m->shiftSelectionUp();
	m->endUndostate();
	peekScrollBar();
	refreshAtomPage();
	pokeScrollBar();
	gui.mainView.postRedisplay();
}

void AtenForm::on_ShiftDownButton_clicked(bool checked)
{
	Model *m = master.currentModel();
	m->beginUndostate("Shift Selection Down");
	m->shiftSelectionDown();
	m->endUndostate();
	peekScrollBar();
	refreshAtomPage();
	pokeScrollBar();
	gui.mainView.postRedisplay();
}

void AtenForm::on_MoveToStartButton_clicked(bool checked)
{
	master.currentModel()->moveSelectionToStart();
	master.currentModel()->logChange(LOG_STRUCTURE);
	refreshAtomPage();
	gui.mainView.postRedisplay();
}

void AtenForm::on_MoveToEndButton_clicked(bool checked)
{
	master.currentModel()->moveSelectionToEnd();
	master.currentModel()->logChange(LOG_STRUCTURE);
	refreshAtomPage();
	gui.mainView.postRedisplay();
}
