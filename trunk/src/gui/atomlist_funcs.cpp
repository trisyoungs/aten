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
#include "gui/tcanvas.uih"

/*
// Atom list window
*/

// Constructor
AtenAtomlist::AtenAtomlist(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);
	
	// Private variables
	listStructurePoint_ = -1;
	listSelectionPoint_ = -1;
	listLastModel_ = NULL;
	shouldRefresh_ = FALSE;
	listPosition_ = -1;
	lastClicked_ = NULL;
	lastHovered_ = NULL;

	QObject::connect(ui.AtomTree, SIGNAL(mousePressEvent(QMouseEvent*)), this, SLOT(treeMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTree, SIGNAL(mouseReleaseEvent(QMouseEvent*)), this, SLOT(treeMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTree, SIGNAL(mouseMoveEvent(QMouseEvent*)), this, SLOT(treeMouseMoveEvent(QMouseEvent*)));


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

// void AtenAtomlist::on_AtomTree_itemSelectionChanged()
// {
// 	printf("Selection changed.\n");
// 	gui.mainWidget->setEnabled(FALSE);
// 	Model *m = aten.currentModel();
// 	TTreeWidgetItem *ti;
// 	foreach( QTreeWidgetItem *item, ui.AtomTree->selectedItems() )
// 	{
// 		ti = (TTreeWidgetItem*) item;
// 		if (ti->atom() != NULL) item->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
// 	}
// 	gui.mainWidget->setEnabled(TRUE);
// }

void AtenAtomlist::updateSelection()
{
	//printf("Selection has been updated.\n");
	TTreeWidgetItem *ti;
	gui.mainView.disableDrawing();
	Model *m = aten.currentModel();
	foreach( QTreeWidgetItem *item, ui.AtomTree->selectedItems() )
	{
		ti = (TTreeWidgetItem*) item;
		if (ti->atom() != NULL) item->isSelected() ? m->selectAtom(ti->atom()) : m->deselectAtom(ti->atom());
	}
	gui.mainView.enableDrawing();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

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
	Pattern *p;
	TTreeWidgetItem *item;
	Refitem<TTreeWidgetItem,int> *ri;
	Atom *i;
	int n, count;
	// Set progress bar
	count = 0;
	if (gui.atomlistWindow->listStructurePoint_ != (m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates)))
	{
		//printf("List must be cleared and repopulated...\n");
		// Clear the current list
		ui.AtomTree->clear();
		ui.AtomTree->clearAtomItems();
		// Add patterns as root nodes in the list, followed by atoms in each pattern.
		// If no patterns are yet defined, store them in a generic rootnode.
		if (m->nPatterns() == 0)
		{
			// Create new root node for all atoms (still a TTreeWidgetItem
			TTreeWidgetItem *pat = new TTreeWidgetItem(ui.AtomTree);
			ui.AtomTree->setItemExpanded(pat, TRUE);
			pat->setText(0, "All");
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
				TTreeWidgetItem *pat = new TTreeWidgetItem(ui.AtomTree);
				ui.AtomTree->setItemExpanded(pat, TRUE);
				pat->setText(0, p->name());
				pat->setPattern(p);
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
		gui.atomlistWindow->listStructurePoint_ = m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates);
		gui.atomlistWindow->listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	else if (gui.atomlistWindow->listSelectionPoint_ != m->changeLog.log(Log::Selection))
	{
		// If we haven't cleared and repopulated the list and the selection point is old, go through the list and apply the new atom selection
		// Grab the list of TTreeWidgetItems
		//printf("Just updating selection....\n");
		for (ri = ui.AtomTree->atomItems(); ri != NULL; ri = ri->next)
		{
			i = ri->item->atom();
			//ui.AtomTree->setItemSelected(ri->item, i->isSelected());
			ri->item->setSelected(i->isSelected());
		}
		listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	for (n=0; n<6; n++) ui.AtomTree->resizeColumnToContents(n);
	msg.exit("AtenAtomlist::refresh");
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

void AtenAtomlist::peekScrollBar()
{
}

void AtenAtomlist::pokeScrollBar()
{
}

void AtenAtomlist::dialogFinished(int result)
{
	gui.mainWindow->ui.actionAtomlistWindow->setChecked(FALSE);
}

// Return item under mouse (if any)
TTreeWidgetItem *AtenAtomlist::itemUnderMouse(const QPoint &pos)
{
	QTreeWidgetItem *qwi = ui.AtomTree->itemAt(pos);
	if (qwi == NULL) return NULL;
	else return (TTreeWidgetItem*) qwi;
}

// Toggle the selection state in the model
void AtenAtomlist::toggleItemSelection(TTreeWidgetItem *twi)
{
	if (twi == NULL) return;
	// Check here for header items...
	if (twi->atom() == NULL) return;
	bool state = twi->isSelected();
	twi->setSelected(!state);
	state ? listLastModel_->deselectAtom(twi->atom()) : listLastModel_->selectAtom(twi->atom());
}

void AtenAtomlist::treeMousePressEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
// 	printf("Mouse press event\n");
	lastClicked_ = itemUnderMouse(event->pos());
	toggleItemSelection(lastClicked_);
	lastHovered_ = lastClicked_;
}

void AtenAtomlist::treeMouseReleaseEvent(QMouseEvent *event)
{
// 	printf("Mouse release event.\n");
	lastHovered_ = NULL;
	gui.modelChanged(FALSE, FALSE, FALSE);
}

void AtenAtomlist::treeMouseMoveEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
// 	printf("Mouse move event.\n");
	TTreeWidgetItem *twi = itemUnderMouse(event->pos());
	// If the current hovered item is the same as the last one, ignore it
	if (twi != lastHovered_)
	{
		toggleItemSelection(twi);
		lastHovered_ = twi;
		gui.modelChanged(FALSE, FALSE, FALSE);
	}
}
