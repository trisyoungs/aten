/*
	*** Atom List Dock Widget
	*** src/gui/atomlist_funcs.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/sysfunc.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/atomlist.h"
#include "gui/toolbox.h"
#include "model/model.h"
#include "main/aten.h"
#include "parser/commandnode.h"
#include "gui/tcanvas.uih"

/*
// Atom list window
*/

// Constructor
AtomListWidget::AtomListWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
	
	// Private variables
	listStructurePoint_ = -1;
	listSelectionPoint_ = -1;
	listLastModel_ = NULL;
	shouldRefresh_ = TRUE;
	lastClicked_ = NULL;
	lastHovered_ = NULL;
	viewingByAtom_ = TRUE;

	QObject::connect(ui.AtomTree, SIGNAL(mousePressEvent(QMouseEvent*)), this, SLOT(treeMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTree, SIGNAL(mouseReleaseEvent(QMouseEvent*)), this, SLOT(treeMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTree, SIGNAL(mouseMoveEvent(QMouseEvent*)), this, SLOT(treeMouseMoveEvent(QMouseEvent*)));
}

void AtomListWidget::showWidget()
{
	show();
	if (shouldRefresh_) refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.AtomListButton->setChecked(TRUE);
}

// Update slection states of TTreeWidgetItems from selection in model
void AtomListWidget::updateSelection()
{
	//printf("Selection has been updated.\n");
	TTreeWidgetItem *ti;
	Atom *i;
	gui.mainWidget->disableDrawing();
	Model *m = aten.currentModelOrFrame();
	foreach( QTreeWidgetItem *item, ui.AtomTree->selectedItems() )
	{
		ti = (TTreeWidgetItem*) item;
		i = (Atom*) ti->data.asPointer(VTypes::AtomData);
		if (i != NULL) item->isSelected() ? m->selectAtom(i) : m->deselectAtom(i);
	}
	gui.mainWidget->enableDrawing();
	gui.update(GuiQt::CanvasTarget);
}

// Set column data for specified item
void AtomListWidget::setColumns(TTreeWidgetItem *twi)
{
	static Vec3<double> r;
	Atom *i = (Atom*) twi->data.asPointer(VTypes::AtomData);
	if (i == NULL) printf("AtomListWidget::setColumns <<<< NULL atom pointer found >>>>\n");
	else
	{
		twi->setText(AtomListWidget::IdData, itoa(i->id()+1));
		twi->setText(AtomListWidget::ElementData, elements().symbol(i));
		r = i->r();
		twi->setText(AtomListWidget::RxData, ftoa(r.x));
		twi->setText(AtomListWidget::RyData, ftoa(r.y));
		twi->setText(AtomListWidget::RzData, ftoa(r.z));
	}
}

// Refresh the atom list
void AtomListWidget::refresh(bool forceupdate)
{
	msg.enter("AtomListWidget::refresh");
	// If the atom list page is not visible, don't do anything
	if (!gui.atomListWidget->isVisible())
	{
		shouldRefresh_ = TRUE;
		msg.exit("AtomListWidget::refresh");
		return;
	}
	Model *m = aten.currentModelOrFrame();
	// Check this model against the last one we represented in the list
	if (m != listLastModel_)
	{
		listStructurePoint_ = -1;
		listSelectionPoint_ = -1;
	}
	listLastModel_ = m;
	// Start the refresh
	Pattern *p;
	TTreeWidgetItem *item;
	Refitem<TTreeWidgetItem,int> *ri;
	Atom *i;
	int mol, n, count;
	if (forceupdate || (gui.atomListWidget->listStructurePoint_ != (m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates))))
	{
		// Clear the current list
		ui.AtomTree->clear();
		ui.AtomTree->clearAtomItems();
		// If we're viewing all atoms, just add all atoms!
		if (viewingByAtom_)
		{
			for (i = m->atoms(); i != NULL; i = i->next)
			{
				// Add the atom
				item = ui.AtomTree->addTreeItem(ui.AtomTree);
				item->data.set(VTypes::AtomData, i);
				setColumns(item);
				// Set the row selection property here.
				item->setSelected(i->isSelected());
			}
		}
		else
		{
			// We must have at least the default pattern definition...
			m->autocreatePatterns(TRUE);
			// Get pointer to first atom in model. We'll skip through it numerically in each pattern
			for (p = m->patterns(); p != NULL; p = p->next)
			{
				// Create new root node for the pattern
				TTreeWidgetItem *pat = new TTreeWidgetItem(ui.AtomTree);
				ui.AtomTree->setItemExpanded(pat, TRUE);
				pat->setText(0, p->name());
				pat->data.set(VTypes::PatternData, p);
				// Get first atom
				i = p->firstAtom();
				count = p->firstAtom()->id();
				// Loop over atoms in molecule
				for (n = 0; n<p->nAtoms(); n++)
				{
					// Create atom in the pattern root node
					item = ui.AtomTree->addTreeItem(pat);
					item->data.set(VTypes::AtomData, i);
					setColumns(item);
					if (i->isSelected()) item->setSelected(TRUE);
					// Check related atoms in other molecules for selection state
					for (mol = 1; mol<p->nMolecules(); mol++)
					{
						if (m->atom(count+mol*p->nAtoms())->isSelected() && (!item->isSelected()))
						{
							QBrush brush(Qt::Dense4Pattern);
							item->setSelected(TRUE);
							item->setForeground(3,brush);
						}
					}
					count++;
					i = i->next;
				}
			}
		}
		// Set new log points
		gui.atomListWidget->listStructurePoint_ = m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates);
		gui.atomListWidget->listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	else if (gui.atomListWidget->listSelectionPoint_ != m->changeLog.log(Log::Selection))
	{
		// If we haven't cleared and repopulated the list and the selection point is old, go through the list and apply the new atom selection
		if (viewingByAtom_)
		{
			for (ri = ui.AtomTree->atomItems(); ri != NULL; ri = ri->next)
			{
				i = (Atom*) ri->item->data.asPointer(VTypes::AtomData);
				if (i != NULL) ri->item->setSelected(i->isSelected());
			}
		}
		else refresh(TRUE);
		listSelectionPoint_ = m->changeLog.log(Log::Selection);
	}
	for (n=0; n<6; n++) ui.AtomTree->resizeColumnToContents(n);
	msg.exit("AtomListWidget::refresh");
}

void AtomListWidget::on_ViewByAtomButton_clicked(bool checked)
{
	// Check previous state and refresh if necessary
	if (!viewingByAtom_)
	{
		viewingByAtom_ = TRUE;
		refresh(TRUE);
	}
	else viewingByAtom_ = TRUE;
}

void AtomListWidget::on_ViewByPatternButton_clicked(bool checked)
{
	// Must clear selection in the current model
	if (viewingByAtom_)
	{
		viewingByAtom_ = FALSE;
		refresh(TRUE);
	}
	else viewingByAtom_ = FALSE;
}

void AtomListWidget::on_ShiftUpButton_clicked(bool checked)
{
	peekScrollBar();
	CommandNode::run(Command::ShiftUp, "i", 1);
	refresh();
	pokeScrollBar();
	gui.update(GuiQt::CanvasTarget);
}

void AtomListWidget::on_ShiftDownButton_clicked(bool checked)
{
	peekScrollBar();
	CommandNode::run(Command::ShiftDown, "i", 1);
	refresh();
	pokeScrollBar();
	gui.update(GuiQt::CanvasTarget);
}

void AtomListWidget::on_MoveToStartButton_clicked(bool checked)
{
	CommandNode::run(Command::MoveToStart, "");
	refresh();
	gui.update(GuiQt::CanvasTarget);
}

void AtomListWidget::on_MoveToEndButton_clicked(bool checked)
{
	CommandNode::run(Command::MoveToEnd, "");
	refresh();
	gui.update(GuiQt::CanvasTarget);
}

void AtomListWidget::peekScrollBar()
{
}

void AtomListWidget::pokeScrollBar()
{
}

// Return item under mouse (if any)
TTreeWidgetItem *AtomListWidget::itemUnderMouse(const QPoint &pos)
{
	QTreeWidgetItem *qwi = ui.AtomTree->itemAt(pos);
	if (qwi == NULL) return NULL;
	else return (TTreeWidgetItem*) qwi;
}

// Toggle the selection state in the model
void AtomListWidget::toggleItem(TTreeWidgetItem *twi)
{
	// Check for no item or header item
	if (twi == NULL) return;
	Atom *i = (Atom*) twi->data.asPointer(VTypes::AtomData);
	if (i == NULL) return;
	bool state = twi->isSelected();
	twi->setSelected(!state);
	state ? listLastModel_->deselectAtom(i) : listLastModel_->selectAtom(i);
}

// Select tree widget item *and* model atom, provided the tree widget item is not selected already
void AtomListWidget::selectItem(TTreeWidgetItem *twi)
{
	if (twi == NULL) return;
	if (twi->isSelected()) return;
	twi->setSelected(TRUE);
	Atom *i = (Atom*) twi->data.asPointer(VTypes::AtomData);
	listLastModel_->selectAtom(i);
}

// Deselect tree widget item *and* model atom, provided the tree widget item is not deselected already
void AtomListWidget::deselectItem(TTreeWidgetItem *twi)
{
	if (twi == NULL) return;
	if (!twi->isSelected()) return;
	twi->setSelected(FALSE);
	Atom *i = (Atom*) twi->data.asPointer(VTypes::AtomData);
	listLastModel_->deselectAtom(i);
}

void AtomListWidget::treeMousePressEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
	// Start an undo state for the model
	listLastModel_->beginUndoState("Change selection through atomlist");
	lastClicked_ = itemUnderMouse(event->pos());
	// Check for header items to we can (un)collapse them or select all atoms within them
	if (lastClicked_ != NULL)
	{
		// If the clicked item contains a pattern pointer, its a collapsible list item root node
		if (lastClicked_->data.type() == VTypes::AtomData) toggleItem(lastClicked_);
		else if (lastClicked_->data.type() == VTypes::PatternData)
		{
			// If the x-coordinate is less than 15, change the collapsed state of the item
			if (event->x() < 15) lastClicked_->setExpanded(!lastClicked_->isExpanded());
			else
			{
				if (event->modifiers()&Qt::ShiftModifier) for (int n=0; n < lastClicked_->childCount(); n++) deselectItem((TTreeWidgetItem*) lastClicked_->child(n));
				else if (event->modifiers()&Qt::ControlModifier) for (int n=0; n < lastClicked_->childCount(); n++) toggleItem((TTreeWidgetItem*) lastClicked_->child(n));
				else for (int n=0; n < lastClicked_->childCount(); n++) selectItem((TTreeWidgetItem*) lastClicked_->child(n));
			}
		}
		else printf("Internal Error: Atomlist item contains an unrecognised pointer type.\n");
	}
	lastHovered_ = lastClicked_;
}

void AtomListWidget::treeMouseReleaseEvent(QMouseEvent *event)
{
// 	printf("Mouse release event.\n");
	lastHovered_ = NULL;
	listLastModel_->endUndoState();
	gui.update(GuiQt::CanvasTarget);
}

void AtomListWidget::treeMouseMoveEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
// 	printf("Mouse move event.\n");
	TTreeWidgetItem *twi = itemUnderMouse(event->pos());
	// If the current hovered item is the same as the last one, ignore it
	if (twi != lastHovered_)
	{
		toggleItem(twi);
		lastHovered_ = twi;
		gui.update(GuiQt::CanvasTarget);
	}
}

void AtomListWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.AtomListButton->setChecked(FALSE);
	event->accept();
}
