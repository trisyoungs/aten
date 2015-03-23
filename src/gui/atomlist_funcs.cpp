/*
	*** Atom List Dock Widget
	*** src/gui/atomlist_funcs.cpp
	Copyright T. Youngs 2007-2015

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

#include <QtGui/QMouseEvent>
#include "gui/atomlist.h"
#include "base/sysfunc.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "main/aten.h"
#include "gui/tdoublespindelegate.uih"
#include "base/forcefieldatom.h"
#include "base/namespace.h"

// ATEN_USING_NAMESPACE

const char* AtomListItemHeader[AtomListWidget::nAtomItems] = { "ID", "El", "Type", "X", "Y", "Z", "Q" };
bool AtomListItemDelegateType[AtomListWidget::nAtomItems] = { 0, 0, 0, 1, 1, 1, 1 };

/*
// Atom list window
*/

// Constructor
AtomListWidget::AtomListWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent,flags), parent_(parent)
{
	ui.setupUi(this);
	
	// Private variables
	listStructurePoint_ = -1;
	listSelectionPoint_ = -1;
	listLastModel_ = NULL;
	shouldRefresh_ = TRUE;
	refreshing_ = FALSE;
	prevClicked_ = NULL;
	lastClicked_ = NULL;
	lastHovered_ = NULL;
	viewingByAtom_ = TRUE;
	maxTableRows_ = 0;
	currentRootId_ = 0;

	// Set custom item delegates for each column
	AtomListItemDelegates[AtomListWidget::AtomIdItem] = NULL;
	AtomListItemDelegates[AtomListWidget::AtomElementItem] = NULL;
	AtomListItemDelegates[AtomListWidget::AtomTypeItem] = NULL;
	AtomListItemDelegates[AtomListWidget::AtomXItem] = new TDoubleSpinDelegate(this);
	AtomListItemDelegates[AtomListWidget::AtomYItem] = new TDoubleSpinDelegate(this);
	AtomListItemDelegates[AtomListWidget::AtomZItem] = new TDoubleSpinDelegate(this);
	AtomListItemDelegates[AtomListWidget::AtomQItem] = new TDoubleSpinDelegate(this);
	
	// Set initial display items
	visibleItems_[AtomListWidget::AtomIdItem] = ui.ViewIdCheck->isChecked();
	visibleItems_[AtomListWidget::AtomElementItem] = ui.ViewElementCheck->isChecked();
	visibleItems_[AtomListWidget::AtomTypeItem] = ui.ViewTypeCheck->isChecked();
	visibleItems_[AtomListWidget::AtomXItem] = ui.ViewXCheck->isChecked();
	visibleItems_[AtomListWidget::AtomYItem] = ui.ViewYCheck->isChecked();
	visibleItems_[AtomListWidget::AtomZItem] = ui.ViewZCheck->isChecked();
	visibleItems_[AtomListWidget::AtomQItem] = ui.ViewChargeCheck->isChecked();
	updateDisplayItems();

	// Connect mouse-tracking signals to AtomTable
	QObject::connect(ui.AtomTable, SIGNAL(mousePressEvent(QMouseEvent*)), this, SLOT(tableMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTable, SIGNAL(mouseReleaseEvent(QMouseEvent*)), this, SLOT(tableMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTable, SIGNAL(mouseMoveEvent(QMouseEvent*)), this, SLOT(tableMouseMoveEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTable, SIGNAL(mouseDoubleClickEvent(QMouseEvent*)), this, SLOT(tableMouseDoubleClickEvent(QMouseEvent*)));
	QObject::connect(ui.AtomTable, SIGNAL(itemChanged(QTableWidgetItem*)), this, SLOT(tableItemChanged(QTableWidgetItem*)));
}

void AtomListWidget::showWidget()
{
	show();
	recalculateRowSize();
	if (shouldRefresh_) refresh();
	// Make sure toolbutton is in correct state
// 	gui.toolBoxWidget->ui.AtomListButton->setChecked(TRUE);
}

// Redetermine max number of visible rows
void AtomListWidget::recalculateRowSize()
{
	int requestedRowHeight = 20, currentRowHeight;
	// Get current row height
	if (ui.AtomTable->rowCount() == 0) ui.AtomTable->setRowCount(2);
	ui.AtomTable->setRowHeight(1, requestedRowHeight);
	currentRowHeight = ui.AtomTable->rowHeight(1);
	
	// Check new size of AtomTable, and determine new number of items to display (subtracting 1 because of the table header)
	// --- Always subtract horizontal scrollbar height, since we can't reliably detect when or when not to subtract it
	int tableHeight = ui.AtomTable->size().height();
	tableHeight -= ui.AtomTable->horizontalScrollBar()->size().height();
	maxTableRows_ = (tableHeight / currentRowHeight) - 1;
	if (maxTableRows_ < 0) maxTableRows_ = 0;

	// Create new table rows and their items
	ui.AtomTable->setRowCount(maxTableRows_);
	QTableWidgetItem *item;
	for (int row=0; row<maxTableRows_; ++row)
	{
		ui.AtomTable->setRowHeight(row, currentRowHeight);
	}
	
	// Extend atom reference list to new size (if necessary)
	Refitem<Atom,int>* ri;
	for (int n=atomItems_.nItems(); n<maxTableRows_; ++n)
	{
		ri = atomItems_.add(NULL);
		ri->data = n;
	}
	
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	refresh();
}

// Set column data for specified item
void AtomListWidget::updateRow(int row)
{
	if ((row < 0) || (row >= maxTableRows_))
	{
		printf("Internal Error - Invalid row passed to AtomListWidget::updateRow()\n");
		return;
	}

	// Go through visible data items, setting relevant column
	QTableWidgetItem *item;
	static Vec3<double> r;
	Refitem<Atom,int>* ri = atomItems_[row];
	if (ri == NULL)
	{
		printf("Internal Error - Couldn't get atom reference in AtomListWidget::updateRow()\n");
		return;
	}
	Atom* i = ri->item;
	if (i == NULL)
	{
		printf("Internal Error - NULL atom pointer found in AtomListWidget::updateRow()\n");
		return;
	}
	r = i->r();
	for (int column = 0; column < displayItems_.count(); ++column)
	{
		item = ui.AtomTable->item(row, column);
		if (item == NULL)
		{
			item = new QTableWidgetItem();
			ui.AtomTable->setItem(row, column, item);
		}
		switch (displayItems_.at(column))
		{
			case (AtomListWidget::AtomIdItem):
				item->setText(QString::number(i->id()+1));
				break;
			case (AtomListWidget::AtomElementItem):
				item->setText(Elements().symbol(i));
				break;
			case (AtomListWidget::AtomTypeItem):
				item->setText((i->type() == NULL ? "" : i->type()->name()));
				break;
			case (AtomListWidget::AtomXItem):
				item->setText(ftoa(r.x));
				break;
			case (AtomListWidget::AtomYItem):
				item->setText(ftoa(r.y));
				break;
			case (AtomListWidget::AtomZItem):
				item->setText(ftoa(r.z));
				break;
			case (AtomListWidget::AtomQItem):
				item->setText(ftoa(i->charge()));
				break;
		}
		if (AtomListItemDelegateType[displayItems_.at(column)] == 0) item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
		else item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable);
	}
}

void AtomListWidget::updateDisplayItems()
{
	displayItems_.clear();
	for (int n=0; n<AtomListWidget::nAtomItems; ++n)
	{
		if (visibleItems_[n]) displayItems_ << n;
	}
	
	// Redo column headings and set delegates
	ui.AtomTable->setColumnCount(displayItems_.count());
	QStringList headerLabels;
	int id;
	for (int n=0; n<displayItems_.count(); ++n)
	{
		id = displayItems_.at(n);
		headerLabels << AtomListItemHeader[id];
		if (AtomListItemDelegates[id] != NULL) ui.AtomTable->setItemDelegateForColumn(id, AtomListItemDelegates[id]);
	}
	ui.AtomTable->setHorizontalHeaderLabels(headerLabels);
}

// Set selection of items in list
void AtomListWidget::updateSelection()
{
	// Clear selection in table
	ui.AtomTable->clearSelection();

	Refitem<Atom,int>* refAtom = atomItems_.first();
	Atom* i;
	for (int row = 0; row < ui.AtomTable->rowCount(); ++row)
	{
		// Get atom pointer corresponding to scrollbar top position
		i = listLastModel_->atom(currentRootId_+row);
		
		// Check reference
		if (refAtom == NULL)
		{
			printf("Internal Error: Reference atom is NULL in AtomListWidget::updateSelection()\n");
			return;
		}

		// Select row if the atom is selected. Otherwise, no need to do anything
		if (i->isSelected()) ui.AtomTable->selectRow(row);

		refAtom = refAtom->next;
	}
}

// Return atom corresponding to row ID provided
Atom* AtomListWidget::atomInRow(int row)
{
	if ((row < 0) || (row > maxTableRows_)) return NULL;
	Refitem<Atom,int>* ri = atomItems_[row];
	if (ri == NULL) return NULL;
	return ri->item;
}

// Refresh the atom list
void AtomListWidget::refresh()
{
	if (refreshing_) return;

	Messenger::enter("AtomListWidget::refresh");

	refreshing_ = TRUE;

	// Check the current active model against the last one we represented in the list
	bool updateSel = FALSE, updateAtoms = FALSE;
	Model* m = parent_.aten().currentModelOrFrame();
	if (m != listLastModel_)
	{
		listStructurePoint_ = -1;
		listSelectionPoint_ = -1;
		listLastModel_ = m;
		updateSel = TRUE;
		updateAtoms = TRUE;
	}
	else
	{
		if (listStructurePoint_ != (m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates))) updateAtoms = TRUE;
		if (listSelectionPoint_ != m->changeLog.log(Log::Selection)) updateSel = TRUE;
	}
	
	if ((!updateAtoms) && (!updateSel))
	{
		refreshing_ = FALSE;
		Messenger::exit("AtomListWidget::refresh");
		return;
	}

	// Set the scrollbar up
	ui.AtomTableScrollBar->setMinimum(0);
	int final = m->nAtoms() - maxTableRows_;
	if (final < 0) final = 0;
	ui.AtomTableScrollBar->setMaximum(final);
	ui.AtomTableScrollBar->setPageStep(maxTableRows_);
	
	// Limit number of visible rows if necessary
	ui.AtomTable->setRowCount((m->nAtoms() < maxTableRows_) ? m->nAtoms() : maxTableRows_);
	
	// Start the refresh
	currentRootId_ = ui.AtomTableScrollBar->value();
// 	printf("Scrollbar value is now %i\n", atomId);
	
	Refitem<Atom,int>* refAtom = atomItems_.first();
	Atom* i;
	for (int row = 0; row < ui.AtomTable->rowCount(); ++row)
	{
		// Get atom pointer corresponding to scrollbar top position
		i = m->atom(currentRootId_+row);
		
		// Check reference
		if (refAtom == NULL)
		{
			printf("Internal Error: Reference atom is NULL in AtomListWidget::refresh()\n");
			refreshing_ = FALSE;
			return;
		}
		
		// Now check reference atom and update data if necessary
		if (updateAtoms || (refAtom->item != i))
		{
			refAtom->item = i;
			updateRow(row);
		}

		// Increase counters
		refAtom = refAtom->next;
	}

	if (updateSel) updateSelection();
	
	// Set new log points
	listStructurePoint_ = m->changeLog.log(Log::Structure) + m->changeLog.log(Log::Coordinates);
	listSelectionPoint_ = m->changeLog.log(Log::Selection);

	// Resize columns to contents
	for (int column = 0; column<displayItems_.count(); ++column) ui.AtomTable->resizeColumnToContents(column);

	refreshing_ = FALSE;
	Messenger::exit("AtomListWidget::refresh");
}

void AtomListWidget::on_AtomTableScrollBar_valueChanged(int value)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	refresh();
}

void AtomListWidget::on_ViewStyleCombo_currentIndexChanged(int index)
{
	// Check previous state and refresh if necessary
	if (index == viewingByAtom_)
	{
		viewingByAtom_ = !index;
		refreshing_ = FALSE;
		listSelectionPoint_ = -1;
		listStructurePoint_ = -1;
		refresh();
	}
}

void AtomListWidget::on_ViewElementCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomElementItem] = ui.ViewElementCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewIdCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomIdItem] = ui.ViewIdCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewTypeCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomTypeItem] = ui.ViewTypeCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewXCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomXItem] = ui.ViewXCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewYCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomYItem] = ui.ViewYCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewZCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomZItem] = ui.ViewZCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ViewChargeCheck_clicked(bool checked)
{
	listSelectionPoint_ = -1;
	listStructurePoint_ = -1;
	visibleItems_[AtomListWidget::AtomQItem] = ui.ViewChargeCheck->isChecked();
	updateDisplayItems();
	refresh();
}

void AtomListWidget::on_ShiftUpButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShiftUp, "i", 1);
	updateSelection();
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtomListWidget::on_ShiftDownButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShiftDown, "i", 1);
	updateSelection();
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtomListWidget::on_MoveToStartButton_clicked(bool checked)
{
	CommandNode::run(Commands::MoveToStart, "");
	updateSelection();
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtomListWidget::on_MoveToEndButton_clicked(bool checked)
{
	CommandNode::run(Commands::MoveToEnd, "");
	updateSelection();
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

//  the selection state in the model
void AtomListWidget::toggleItem(Atom* i)
{
	// Swap selection status of atom, and update the list
	if (i->isSelected()) listLastModel_->deselectAtom(i);
	else listLastModel_->selectAtom(i);
	updateSelection();
}

void AtomListWidget::tableMousePressEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	// Determine index of row under mouse, and whether it corresponds to a valid atom
	int row = ui.AtomTable->rowAt(event->pos().y());

	lastClicked_ = atomInRow(row);
	lastHovered_ = lastClicked_;
	if (lastClicked_ == NULL)
	{
		event->ignore();
		return;
	}

	// Start an undo state for the model (if one doesn't already exist)
	if (!listLastModel_->recordingUndoState()) listLastModel_->beginUndoState("Change selection through atomlist");
	
	toggleItem(lastClicked_);
}

void AtomListWidget::tableMouseReleaseEvent(QMouseEvent *event)
{
	bool shift = event->modifiers()&Qt::ShiftModifier;
	int row = ui.AtomTable->rowAt(event->pos().y());
	Atom* i = atomInRow(row);
// 	printf("mouse release event. shift status is %i  %p  %p\n", shift, prevClicked_, i);

	// If SHIFT was held, (de)select all atoms between the last clicked atom and this one
	if (shift && (prevClicked_ != NULL) && (i != NULL))
	{
		// Determine IDs of first and last atom
		int first = prevClicked_->id(), last = i->id();
		if (first > last)
		{
			last = first;
			first = i->id();
		}

		// Start an undo state for the model (if one doesn't already exist)
		if (!listLastModel_->recordingUndoState()) listLastModel_->beginUndoState("Change selection through atomlist");

// 		printf("First and last atoms are %i and %i\n", first, last);
		for (int n=first; n<=last; ++n)
		{
			if (prevClicked_->isSelected()) listLastModel_->selectAtom(n);
			else listLastModel_->deselectAtom(n);
		}
	}

	prevClicked_ = lastClicked_;
	lastHovered_ = NULL;
	if (listLastModel_->recordingUndoState()) listLastModel_->endUndoState();
	
	refresh();
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtomListWidget::tableMouseMoveEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	bool shift = event->modifiers()&Qt::ShiftModifier;
	if (shift) return;
	
	int row = ui.AtomTable->rowAt(event->pos().y());
	Atom* i = atomInRow(row);
	
	// If the hovered widget is NULL, then we are either above or below the visible widgets, so scroll the slider...
	if (i == NULL)
	{
		int delta, y = event->pos().y();
		int rowHeight = ui.AtomTable->rowHeight(0);
		int oldValue = ui.AtomTableScrollBar->value();
		int newValue = oldValue;

		// Start an undo state for the model (if one doesn't already exist)
		if (!listLastModel_->recordingUndoState()) listLastModel_->beginUndoState("Change selection through atomlist");

		if (y < rowHeight) 
		{
			// Mouse at top (over header) so scroll up
			delta = (rowHeight - y) / 2;
			newValue -= delta;
			if (newValue < 0) newValue = 0;
			delta = oldValue - newValue;

			// Before we set the scrollbar to the new value, set the selected state of the atoms between the last
			// hovered one and the new position
			bool lastSelected = lastHovered_->isSelected(); 
			for (int n = 0; n < delta; ++n)
			{
				lastHovered_ = lastHovered_->prev;
				printf("(De)Selecting atom %i\n", lastHovered_->id());
				if (lastSelected) listLastModel_->selectAtom(lastHovered_);
				else listLastModel_->deselectAtom(lastHovered_);
			}
		}
		else
		{	int row = ui.AtomTable->rowAt(event->pos().y());

			// Mouse at bottom (over horizontal scroll bar area) so scroll down
			delta = (y - (maxTableRows_*rowHeight)) / 2;
			newValue += delta;
			if (newValue > (listLastModel_->nAtoms()-maxTableRows_)) newValue = listLastModel_->nAtoms() - maxTableRows_;
			delta = newValue - oldValue;

			// Before we set the scrollbar to the new value, set the selected state of the atoms between the last
			// hovered one and the new position
			bool lastSelected = lastHovered_->isSelected(); 
			for (int n = 0; n < delta; ++n)
			{
				lastHovered_ = lastHovered_->next;
				printf("(De)Selecting atom %i\n", lastHovered_->id());
				if (lastSelected) listLastModel_->selectAtom(lastHovered_);
				else listLastModel_->deselectAtom(lastHovered_);
			}
		}

		ui.AtomTableScrollBar->setValue(newValue);
	}
	else if (i != lastHovered_)
	{
		// If not NULL, and the current hovered item is not the same as the previous one, toggle the item
		toggleItem(i);
		lastHovered_ = i;
		parent_.updateWidgets(AtenWindow::CanvasTarget);
	}
}

void AtomListWidget::tableMouseDoubleClickEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	// Determine index of row under mouse, and whether it corresponds to a valid atom
	int row = ui.AtomTable->rowAt(event->pos().y());

	lastClicked_ = atomInRow(row);
	lastHovered_ = lastClicked_;

	QTableWidgetItem *item = ui.AtomTable->itemAt(event->pos());
	if (item != NULL) ui.AtomTable->editItem(item);
	
	event->accept();
}

void AtomListWidget::tableItemChanged(QTableWidgetItem *item)
{
	if (refreshing_) return;
	int row = item->row(), column = item->column();
	Atom* i = atomInRow(row);
	if (i == NULL)
	{
		printf("Internal Error: NULL atom pointer in AtomListWidget::tableItemChanged.\n");
		return;
	}

	// From the current visible columns list, determine where to poke the new value...
	double valueD;
	bool success;
	Vec3<double> v;
	int element;
	switch (displayItems_.at(column))
	{
		case (AtomListWidget::AtomXItem):
		case (AtomListWidget::AtomYItem):
		case (AtomListWidget::AtomZItem):
			element = displayItems_.at(column) - AtomListWidget::AtomXItem;
			valueD = item->text().toDouble(&success);
			v = i->r();
			if (success)
			{
				listLastModel_->beginUndoState("Edit %c coordinate of atom %i", char(88+element), i->id()+1);
				v[element] = valueD;
				listLastModel_->positionAtom(i, v);
				listLastModel_->endUndoState();
			}
			else item->setText(ftoa(v[element]));
			break;
		case (AtomListWidget::AtomQItem):
			valueD = item->text().toDouble(&success);
			if (success)
			{
				listLastModel_->beginUndoState("Edit charge of atom %i", i->id()+1);
				listLastModel_->atomSetCharge(i, valueD);
				listLastModel_->endUndoState();
			}
			else item->setText(ftoa(i->charge()));
			break;
		default:
			printf("Internal Error: This column contains no editable data!\n");
			break;
	}

	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtomListWidget::closeEvent(QCloseEvent *event)
{
// 	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
// 	gui.toolBoxWidget->ui.AtomListButton->setChecked(FALSE);
// 	if (this->isFloating()) parent_.postRedisplay();
	event->accept();
}

void AtomListWidget::resizeEvent(QResizeEvent *event)
{
	recalculateRowSize();
}

void AtomListWidget::wheelEvent(QWheelEvent *event)
{
	// Grab current slider position
	int pos = ui.AtomTableScrollBar->value();
	pos -= ui.AtomTableScrollBar->pageStep() * (event->delta() / 120);
	ui.AtomTableScrollBar->setValue(pos);
}
