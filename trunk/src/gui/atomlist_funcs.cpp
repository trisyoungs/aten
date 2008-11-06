/*
	*** Qt GUI: Atomlist window functions
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

#include "base/pattern.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/atomlist.h"
#include "model/model.h"
#include "main/aten.h"
#include "command/staticcommand.h"

/*
// Atom list window
*/

TTreeWidgetItem *lastItem = NULL;

// Constructor
AtenAtomlist::AtenAtomlist(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
	
	// Private variables
	listStructurePoint_ = -1;
	listSelectionPoint_ = -1;
	listLastModel_ = NULL;
	refreshing_ = FALSE;
	shouldRefresh_ = FALSE;
	listPosition_ = -1;

	setMouseTracking(TRUE);
	QObject::connect(ui.AtomTree, SIGNAL(itemSelectionChanged()), this, SLOT(updateSelection()));
}

// Destructor
AtenAtomlist::~AtenAtomlist()
{
}

void AtenAtomlist::showWindow()
{
	if (shouldRefresh_) refresh();
	show();
}

// void AtenAtomlist::on_AtomTree_itemPressed(QTreeWidgetItem *item, int column)
// {
// 	if (refreshing_) return;
// 	// Cast *item into a TTreeWidgetItem
// 	TTreeWidgetItem *ti = (TTreeWidgetItem*) item;
// 	Model *m = aten.currentModel();
// 	// If this was a pattern treeitem, (de)select the whole pattern, otherwise (de)select atom
// 	if (ti->pattern() != NULL)
// 	{
// 		Atom *i = ti->pattern()->firstAtom();
// 		for (int n=0; n<ti->pattern()->totalAtoms(); n++)
// 		{
// 			item->isSelected() ? m->selectAtom(i) : m->deselectAtom(i);
// 			i = i->next;
// 		}
// 	}
// 	else if (ti->atom() != NULL) item->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
// 	gui.modelChanged(FALSE,FALSE,FALSE);
// }

void AtenAtomlist::updateSelection()
{
	//printf("Selection has been updated.\n");
	TTreeWidgetItem *ti;
	Model *m = aten.currentModel();
	foreach( QTreeWidgetItem *item, ui.AtomTree->selectedItems() )
	{
		ti = (TTreeWidgetItem*) item;
		if (ti->atom() != NULL) item->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
	}
	gui.modelChanged(FALSE,FALSE,FALSE);
}

// void AtenAtomlist::selectionChanged( const QItemSelection & selected, const QItemSelection & deselected )
// {
// 	printf("XXX\n");
// 	for (int i = 0; i < selected.size(); ++i) printf("Selected range height = %d\n", selected.at(i).height());
// 	for (int i = 0; i < deselected.size(); ++i) printf("Deselected range height = %d\n", deselected.at(i).height());
// 	printf("XXX\n");
// }

// void AtenAtomlist::on_AtomTree_itemEntered(QTreeWidgetItem *item, int column)
// {
// 	if (refreshing_) return;
// 	// Cast *item into a TTreeWidgetItem
// 	TTreeWidgetItem *ti = (TTreeWidgetItem*) item;
// 	Model *m = aten.currentModel();
// 	// If this was a pattern treeitem, (de)select the whole pattern, otherwise (de)select atom
// 	if (ti->atom() != NULL) !item->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
// 	gui.modelChanged(FALSE,FALSE,FALSE);
// }

// void AtenAtomlist::mouseMoveEvent(QMouseEvent *event)
// {
// 	if (refreshing_) return;
// 	if (event->buttons() != Qt::NoButton)
// 	{
// 		TTreeWidgetItem *ti = (TTreeWidgetItem*) ui.AtomTree->itemAt(event->pos());
// 		if (ti != NULL)
// 		{
// 			Model *m = aten.currentModel();
// 			if (ti->atom() != NULL) ti->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
// 			gui.modelChanged(FALSE,FALSE,FALSE);
// 		}
// 	}
// }

// void AtenAtomlist::on_AtomTree_currentItemChanged(QTreeWidgetItem *current, QTreeWidgetItem *prev)
// {
// 	if (current == NULL) return;
// 	if (refreshing_) return;
// 	// Cast *item into a TTreeWidgetItem
// 	TTreeWidgetItem *ti = (TTreeWidgetItem*) current;
// 	Model *m = aten.currentModel();
// 	// If this was a pattern treeitem, (de)select the whole pattern, otherwise (de)select atom
// 	if (ti->pattern() != NULL)
// 	{
// 		Atom *i = ti->pattern()->firstAtom();
// 		for (int n=0; n<ti->pattern()->totalAtoms(); n++)
// 		{
// 			current->isSelected() ? m->selectAtom(i) : m->deselectAtom(i);
// 			i = i->next;
// 		}
// 	}
// 	else if (ti->atom() != NULL) current->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
// 	gui.modelChanged(FALSE,FALSE,FALSE);
// }

void AtenAtomlist::refresh()
{
	msg.enter("AtenAtomlist::refresh");
	// If the atom list page is not visible, don't do anything
	if (!gui.atomlistWindow->isVisible())
	{
		shouldRefresh_ = TRUE;
		msg.exit("AtenAtomlist::refresh");
		return;
	}
	refreshing_ = TRUE;
	//printf("Refreshing atompage.....\n");
	Model *m = aten.currentModel();
	// Check this model against the last one we represented in the list
	if (m != listLastModel_)
	{
		listStructurePoint_ = -1;
		listSelectionPoint_ = -1;
	}
	listLastModel_ = m;
	// Start the refresh (note, this does not run in a thread yet!)
	refreshThread.run();
	msg.exit("AtenAtomlist::refresh");
}

void AtenAtomlist::peekScrollBar()
{
	listPosition_ = ui.AtomTree->verticalScrollBar()->sliderPosition();
}

void AtenAtomlist::pokeScrollBar()
{
	ui.AtomTree->verticalScrollBar()->setSliderPosition(listPosition_);
}

void AtenAtomlist::on_ShiftUpButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SHIFTUP, "i", 1);
	peekScrollBar();
	cmd.execute();
	refresh();
	pokeScrollBar();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenAtomlist::on_ShiftDownButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SHIFTDOWN, "i", 1);
	peekScrollBar();
	cmd.execute();
	refresh();
	pokeScrollBar();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenAtomlist::on_MoveToStartButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_TOSTART, "");
	cmd.execute();
	refresh();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenAtomlist::on_MoveToEndButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_TOEND, "");
	cmd.execute();
	refresh();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenAtomlist::dialogFinished(int result)
{
	gui.mainWindow->ui.actionAtomlistWindow->setChecked(FALSE);
}

/*
// Refresh thread
*/

// Constructor
AtomlistRefreshThread::AtomlistRefreshThread()
{
	// Private variables
	kill_ = FALSE;
	restart_ = FALSE;
}

// Execute thread
void AtomlistRefreshThread::run()
{
	Pattern *p;
	TTreeWidgetItem *item;
	Refitem<TTreeWidgetItem,int> *ri;
	Atom *i;
	int n, count;
	// Grab model to be displayed
	Model *m = gui.atomlistWindow->listLastModel_;
	// Set progress bar
	gui.atomlistWindow->ui.RefreshProgressBar->setRange(0, m->nAtoms());
	count = 0;
	if (gui.atomlistWindow->listStructurePoint_ != (m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates)))
	{
		//printf("List must be cleared and repopulated...\n");
		// Clear the current list
		gui.atomlistWindow->ui.AtomTree->clear();
		gui.atomlistWindow->ui.AtomTree->clearAtomItems();
		// If there are no atoms in the current model, exit now.
		if (m->nAtoms() == 0)
		{
			gui.atomlistWindow->refreshing_ = FALSE;
			return;
		}
		// Add patterns as root nodes in the list, followed by atoms in each pattern.
		// If no patterns are yet defined, store them in a generic rootnode.
		if (m->nPatterns() == 0)
		{
			// Create new root node for all atoms (still a TTreeWidgetItem
			TTreeWidgetItem *pat = new TTreeWidgetItem(gui.atomlistWindow->ui.AtomTree);
			gui.atomlistWindow->ui.AtomTree->setItemExpanded(pat, TRUE);
			pat->setText(0, "All");
			for (i = m->atoms(); i != NULL; i = i->next)
			{
				// Add the atom
				item = gui.atomlistWindow->ui.AtomTree->addTreeItem(pat);
				//item->setFlags(Qt::ItemIsEditable | Qt::ItemIsSelectable | Qt::ItemIsEnabled);
				item->setAtom(i);
				item->setAtomColumns();
				// Set the row selection property here.
				gui.atomlistWindow->ui.AtomTree->setItemSelected(item, i->isSelected());
				// Update progress bar
				gui.atomlistWindow->ui.RefreshProgressBar->setValue(++count);
			}
		}
		else
		{
			// Get pointer to first atom in model. We'll skip through it numerically in each pattern
			i = m->atoms();
			for (p = m->patterns(); p != NULL; p = p->next)
			{
				// Create new root node for the pattern
				TTreeWidgetItem *pat = new TTreeWidgetItem(gui.atomlistWindow->ui.AtomTree);
				gui.atomlistWindow->ui.AtomTree->setItemExpanded(pat, TRUE);
				pat->setText(0, p->name());
				pat->setPattern(p);
				for (n = 0; n<p->totalAtoms(); n++)
				{
					// Create atom in the pattern root node
					item = gui.atomlistWindow->ui.AtomTree->addTreeItem(pat);
					//item->setFlags(Qt::ItemIsEditable | Qt::ItemIsSelectable | Qt::ItemIsEnabled);
					item->setAtom(i);
					item->setAtomColumns();
					// Set the row selection property here
					gui.atomlistWindow->ui.AtomTree->setItemSelected(item, i->isSelected());
					i = i->next;
					// Update progress bar
					gui.atomlistWindow->ui.RefreshProgressBar->setValue(++count);
				}
			}
		}
		// Set new log points
		gui.atomlistWindow->listStructurePoint_ = m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates);
		gui.atomlistWindow->listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	else if (gui.atomlistWindow->listSelectionPoint_ != m->changeLog.log(Log::Selection))
	{
		// If we haven't cleared and repopulated the list and the selection point is old, go through the list and apply the new atom selection
		// Grab the list of TTreeWidgetItems
		//printf("Just updating selection....\n");
		for (ri = gui.atomlistWindow->ui.AtomTree->atomItems(); ri != NULL; ri = ri->next)
		{
			i = ri->item->atom();
			//gui.atomlistWindow->ui.AtomTree->setItemSelected(ri->item, i->isSelected());
			ri->item->setSelected(i->isSelected());
			// Update progress bar
			gui.atomlistWindow->ui.RefreshProgressBar->setValue(++count);
		}
		gui.atomlistWindow->listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	for (n=0; n<6; n++) gui.atomlistWindow->ui.AtomTree->resizeColumnToContents(n);
	gui.atomlistWindow->refreshing_ = FALSE;
	gui.atomlistWindow->shouldRefresh_ = FALSE;
}

// Restart thread
void AtomlistRefreshThread::restart()
{
}

// Kill thread
void AtomlistRefreshThread::kill()
{
}
