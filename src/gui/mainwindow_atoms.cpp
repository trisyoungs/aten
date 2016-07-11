/*
	*** Main Window - AtomsList Functions
	*** src/gui/mainwindow_atoms.cpp
	Copyright T. Youngs 2007-2016

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
#include "gui/mainwindow.h"
#include "base/forcefieldatom.h"
#include "templates/variantpointer.h"
#include <QMouseEvent>

const char* AtomsTableItemHeader[AtenWindow::nAtomItems] = { "ID", "El", "Type", "X", "Y", "Z", "Q" };
bool AtomsTableItemDelegateType[AtenWindow::nAtomItems] = { 0, 0, 0, 1, 1, 1, 1 };

// Refresh atoms table
void AtenWindow::updateAtomsTable(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateAtomsTable");

	if (!sourceModel)
	{
		ui.AtomsTable->clear();
		Messenger::exit("AtenWindow::updateAtomsTable");
		return;
	}

	// Check the current active model against the last one we represented in the list
	bool updateSel = false, updateAtoms = false;
	if (sourceModel != atomsTableLastModel_)
	{
		atomsTableStructurePoint_ = -1;
		atomsTableSelectionPoint_ = -1;
		atomsTableLastModel_ = sourceModel;

		atomsTableUpdateDisplayItems();

		updateSel = true;
		updateAtoms = true;
	}
	else
	{
		if (atomsTableStructurePoint_ != (sourceModel->log(Log::Structure) + sourceModel->log(Log::Coordinates))) updateAtoms = true;
		if (atomsTableSelectionPoint_ != sourceModel->log(Log::Selection)) updateSel = true;
	}
	
	if ((!updateAtoms) && (!updateSel))
	{
		Messenger::exit("AtenWindow::updateAtomsTable");
		return;
	}

	// Set the scrollbar up
	ui.AtomsTableScrollBar->setMinimum(0);
	int final = sourceModel->nAtoms() - atomsTableMaxRows_;
	if (final < 0) final = 0;
	ui.AtomsTableScrollBar->setMaximum(final);
	ui.AtomsTableScrollBar->setPageStep(atomsTableMaxRows_);
	
	// Limit number of visible rows if necessary
	ui.AtomsTable->setRowCount((sourceModel->nAtoms() < atomsTableMaxRows_) ? sourceModel->nAtoms() : atomsTableMaxRows_);
	
	// Start the refresh
	atomsTableCurrentRootId_ = ui.AtomsTableScrollBar->value();
	
	RefListItem<Atom,int>* refAtom = atomsTableItems_.first();
	Atom* i;
	for (int row = 0; row < ui.AtomsTable->rowCount(); ++row)
	{
		// Get atom pointer corresponding to scrollbar top position
		i = sourceModel->atom(atomsTableCurrentRootId_+row);
		
		// Check reference
		if (refAtom == NULL)
		{
			printf("Internal Error: Reference atom is NULL in AtenWindow::refresh()\n");
			return;
		}
		
		// Now check reference atom and update data if necessary
		if (updateAtoms || (refAtom->item != i))
		{
			refAtom->item = i;
			atomsTableUpdateRow(row);
		}

		// Increase counters
		refAtom = refAtom->next;
	}

	if (updateSel) atomsTableUpdateSelection();
	
	// Set new log points
	atomsTableStructurePoint_ = sourceModel->log(Log::Structure) + sourceModel->log(Log::Coordinates);
	atomsTableSelectionPoint_ = sourceModel->log(Log::Selection);

	// Resize columns to contents
	for (int column = 0; column<atomsTableDisplayItems_.count(); ++column) ui.AtomsTable->resizeColumnToContents(column);

	Messenger::exit("AtenWindow::updateAtomsTable");
}

void AtenWindow::on_AtomsTableToggleButton_clicked(bool checked)
{
	ui.AtomsTableWidget->setVisible(checked);
	if (checked)
	{
		atomsTableRecalculateRowSize();
		updateWidgets(AtenWindow::ModelsListTarget+AtenWindow::AtomsTableTarget);
	}
}

// Redetermine max number of visible rows
void AtenWindow::atomsTableRecalculateRowSize()
{
	int requestedRowHeight = 20, currentRowHeight;

	// Get current row height
	if (ui.AtomsTable->rowCount() == 0) ui.AtomsTable->setRowCount(2);
	ui.AtomsTable->setRowHeight(1, requestedRowHeight);
	currentRowHeight = ui.AtomsTable->rowHeight(1);
	
	// Check new size of AtomTable, and determine new number of items to display (subtracting 1 because of the table header)
	// --- Always subtract horizontal scrollbar height, since we can't reliably detect when or when not to subtract it
	int tableHeight = ui.AtomsTable->size().height();
	tableHeight -= ui.AtomsTable->horizontalScrollBar()->size().height();
	atomsTableMaxRows_ = (tableHeight / currentRowHeight) - 1;
	if (atomsTableMaxRows_ < 0) atomsTableMaxRows_ = 0;

	// Create new table rows and their items
	ui.AtomsTable->setRowCount(atomsTableMaxRows_);
	QTableWidgetItem *item;
	for (int row=0; row<atomsTableMaxRows_; ++row)
	{
		ui.AtomsTable->setRowHeight(row, currentRowHeight);
	}
	
	// Extend atom reference list to new size (if necessary)
	RefListItem<Atom,int>* ri;
	for (int n=atomsTableItems_.nItems(); n<atomsTableMaxRows_; ++n)
	{
		ri = atomsTableItems_.add(NULL);
		ri->data = n;
	}
	
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;

	updateWidgets(AtenWindow::AtomsTableTarget);
}

// Set column data for specified item
void AtenWindow::atomsTableUpdateRow(int row)
{
	if ((row < 0) || (row >= atomsTableMaxRows_))
	{
		printf("Internal Error - Invalid row passed to AtenWindow::updateRow()\n");
		return;
	}

	// Go through visible data items, setting relevant column
	QTableWidgetItem *item;
	static Vec3<double> r;
	RefListItem<Atom,int>* ri = atomsTableItems_[row];
	if (ri == NULL)
	{
		printf("Internal Error - Couldn't get atom reference in AtenWindow::updateRow()\n");
		return;
	}
	Atom* i = ri->item;
	if (i == NULL)
	{
		printf("Internal Error - NULL atom pointer found in AtenWindow::updateRow()\n");
		return;
	}
	r = i->r();
	for (int column = 0; column < atomsTableDisplayItems_.count(); ++column)
	{
		item = ui.AtomsTable->item(row, column);
		if (item == NULL)
		{
			item = new QTableWidgetItem();
			ui.AtomsTable->setItem(row, column, item);
		}
		switch (atomsTableDisplayItems_.at(column))
		{
			case (AtenWindow::AtomIdItem):
				item->setText(QString::number(i->id()+1));
				break;
			case (AtenWindow::AtomElementItem):
				item->setText(ElementMap::symbol(i));
				break;
			case (AtenWindow::AtomTypeItem):
				item->setText((i->type() == NULL ? "" : i->type()->name()));
				break;
			case (AtenWindow::AtomXItem):
				item->setText(QString::number(r.x));
				break;
			case (AtenWindow::AtomYItem):
				item->setText(QString::number(r.y));
				break;
			case (AtenWindow::AtomZItem):
				item->setText(QString::number(r.z));
				break;
			case (AtenWindow::AtomQItem):
				item->setText(QString::number(i->charge()));
				break;
		}
		if (AtomsTableItemDelegateType[atomsTableDisplayItems_.at(column)] == 0) item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
		else item->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable | Qt::ItemIsEditable);
	}
}

void AtenWindow::atomsTableUpdateDisplayItems()
{
	atomsTableDisplayItems_.clear();
	for (int n=0; n<AtenWindow::nAtomItems; ++n)
	{
		if (atomsTableVisibleItems_[n]) atomsTableDisplayItems_ << n;
	}
	
	// Redo column headings and set delegates
	ui.AtomsTable->setColumnCount(atomsTableDisplayItems_.count());
	QStringList headerLabels;
	int id;
	for (int n=0; n<atomsTableDisplayItems_.count(); ++n)
	{
		id = atomsTableDisplayItems_.at(n);
		headerLabels << AtomsTableItemHeader[id];
		if (atomsTableItemDelegates_[id] != NULL) ui.AtomsTable->setItemDelegateForColumn(id, atomsTableItemDelegates_[id]);
	}
	ui.AtomsTable->setHorizontalHeaderLabels(headerLabels);
}

// Set selection of items in list
void AtenWindow::atomsTableUpdateSelection()
{
	// Clear selection in table
	ui.AtomsTable->clearSelection();

	RefListItem<Atom,int>* refAtom = atomsTableItems_.first();
	Atom* i;
	for (int row = 0; row < ui.AtomsTable->rowCount(); ++row)
	{
		// Get atom pointer corresponding to scrollbar top position
		i = atomsTableLastModel_->atom(atomsTableCurrentRootId_+row);
		
		// Check reference
		if (refAtom == NULL)
		{
			printf("Internal Error: Reference atom is NULL in AtenWindow::updateSelection()\n");
			return;
		}

		// Select row if the atom is selected. Otherwise, no need to do anything
		if (i->isSelected()) ui.AtomsTable->selectRow(row);

		refAtom = refAtom->next;
	}
}

// Return atom corresponding to row ID provided
Atom* AtenWindow::atomsTableAtomInRow(int row)
{
	if ((row < 0) || (row > atomsTableMaxRows_)) return NULL;
	RefListItem<Atom,int>* ri = atomsTableItems_[row];
	if (ri == NULL) return NULL;
	return ri->item;
}

//  Toggle selection state of the specified atom
void AtenWindow::atomsTableToggleItem(Atom* i)
{
	// Swap selection status of atom, and update the list
	if (i->isSelected()) atomsTableLastModel_->deselectAtom(i);
	else atomsTableLastModel_->selectAtom(i);

	atomsTableUpdateSelection();
}

void AtenWindow::on_AtomsTableScrollBar_valueChanged(int value)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewElementCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomElementItem] = ui.AtomsViewElementCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewIdCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomIdItem] = ui.AtomsViewIdCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewTypeCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomTypeItem] = ui.AtomsViewTypeCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewXCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomXItem] = ui.AtomsViewXCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewYCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomYItem] = ui.AtomsViewYCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewZCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomZItem] = ui.AtomsViewZCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsViewChargeCheck_clicked(bool checked)
{
	atomsTableSelectionPoint_ = -1;
	atomsTableStructurePoint_ = -1;
	atomsTableVisibleItems_[AtenWindow::AtomQItem] = ui.AtomsViewChargeCheck->isChecked();

	atomsTableUpdateDisplayItems();

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsShiftUpButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShiftUp, "i", 1);
	atomsTableUpdateSelection();

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsShiftDownButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShiftDown, "i", 1);
	atomsTableUpdateSelection();

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsMoveToStartButton_clicked(bool checked)
{
	CommandNode::run(Commands::MoveToStart, "");
	atomsTableUpdateSelection();

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_AtomsMoveToEndButton_clicked(bool checked)
{
	CommandNode::run(Commands::MoveToEnd, "");
	atomsTableUpdateSelection();
	
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::atomsTableMousePressEvent(QMouseEvent* event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	// Determine index of row under mouse, and whether it corresponds to a valid atom
	int row = ui.AtomsTable->rowAt(event->pos().y());

	atomsTableLastClicked_ = atomsTableAtomInRow(row);
	atomsTableLastHovered_ = atomsTableLastClicked_;
	if (atomsTableLastClicked_ == NULL)
	{
		event->ignore();
		return;
	}

	// Start an undo state for the model (if one doesn't already exist)
	if (!atomsTableLastModel_->recordingUndoState()) atomsTableLastModel_->beginUndoState("Change selection through Atoms table");
	
	atomsTableToggleItem(atomsTableLastClicked_);
}

void AtenWindow::atomsTableMouseReleaseEvent(QMouseEvent* event)
{
	bool shift = event->modifiers()&Qt::ShiftModifier;
	int row = ui.AtomsTable->rowAt(event->pos().y());
	Atom* i = atomsTableAtomInRow(row);
// 	printf("mouse release event. shift status is %i  %p  %p\n", shift, atomsTablePrevClicked_, i);

	// If SHIFT was held, (de)select all atoms between the last clicked atom and this one
	if (shift && (atomsTablePrevClicked_ != NULL) && (i != NULL))
	{
		// Determine IDs of first and last atom
		int first = atomsTablePrevClicked_->id(), last = i->id();
		if (first > last)
		{
			last = first;
			first = i->id();
		}

		// Start an undo state for the model (if one doesn't already exist)
		if (!atomsTableLastModel_->recordingUndoState()) atomsTableLastModel_->beginUndoState("Change selection through Atoms table");

// 		printf("First and last atoms are %i and %i\n", first, last);
		for (int n=first; n<=last; ++n)
		{
			if (atomsTablePrevClicked_->isSelected()) atomsTableLastModel_->selectAtom(n);
			else atomsTableLastModel_->deselectAtom(n);
		}
	}

	atomsTablePrevClicked_ = atomsTableLastClicked_;
	atomsTableLastHovered_ = NULL;
	if (atomsTableLastModel_->recordingUndoState()) atomsTableLastModel_->endUndoState();
	
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::atomsTableMouseMoveEvent(QMouseEvent* event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	bool shift = event->modifiers()&Qt::ShiftModifier;
	if (shift) return;

	// If the last hovered atom is null, then we have nothing to do
	if (atomsTableLastHovered_ == NULL) return;
	
	int row = ui.AtomsTable->rowAt(event->pos().y());
	Atom* i = atomsTableAtomInRow(row);
	
	// If the hovered atom is NULL, then we are either above or below the visible widgets, so scroll the slider...
	if (i == NULL)
	{
		// Quick check - if 
		int delta, y = event->pos().y();
		int rowHeight = ui.AtomsTable->rowHeight(0);
		int oldValue = ui.AtomsTableScrollBar->value();
		int newValue = oldValue;

		// Start an undo state for the model (if one doesn't already exist)
		if (!atomsTableLastModel_->recordingUndoState()) atomsTableLastModel_->beginUndoState("Change selection through Atoms table");

		if (y < rowHeight) 
		{
			// Mouse at top (over header) so scroll up
			delta = (rowHeight - y) / 2;
			newValue -= delta;
			if (newValue < 0) newValue = 0;
			delta = oldValue - newValue;

			// Before we set the scrollbar to the new value, set the selected state of the atoms between the last
			// hovered one and the new position
			bool lastSelected = atomsTableLastHovered_->isSelected(); 
			for (int n = 0; n < delta; ++n)
			{
				atomsTableLastHovered_ = atomsTableLastHovered_->prev;
// 				printf("(De)Selecting atom %i\n", atomsTableLastHovered_->id());
				if (lastSelected) atomsTableLastModel_->selectAtom(atomsTableLastHovered_);
				else atomsTableLastModel_->deselectAtom(atomsTableLastHovered_);
			}
		}
		else
		{	int row = ui.AtomsTable->rowAt(event->pos().y());

			// Mouse at bottom (over horizontal scroll bar area) so scroll down
			delta = (y - (atomsTableMaxRows_*rowHeight)) / 2;
			newValue += delta;
			if (newValue > (atomsTableLastModel_->nAtoms()-atomsTableMaxRows_)) newValue = atomsTableLastModel_->nAtoms() - atomsTableMaxRows_;
			delta = newValue - oldValue;

			// Before we set the scrollbar to the new value, set the selected state of the atoms between the last
			// hovered one and the new position
			bool lastSelected = atomsTableLastHovered_->isSelected(); 
			for (int n = 0; n < delta; ++n)
			{
				atomsTableLastHovered_ = atomsTableLastHovered_->next;
// 				printf("(De)Selecting atom %i\n", atomsTableLastHovered_->id());
				if (lastSelected) atomsTableLastModel_->selectAtom(atomsTableLastHovered_);
				else atomsTableLastModel_->deselectAtom(atomsTableLastHovered_);
			}
		}

		ui.AtomsTableScrollBar->setValue(newValue);
	}
	else if (i != atomsTableLastHovered_)
	{
		// If not NULL, and the current hovered item is not the same as the previous one, toggle the item
		atomsTableToggleItem(i);
		atomsTableLastHovered_ = i;
		updateWidgets(AtenWindow::MainViewTarget);
	}
}

void AtenWindow::atomsTableMouseDoubleClickEvent(QMouseEvent* event)
{
	if (!(event->buttons()&Qt::LeftButton))
	{
		event->ignore();
		return;
	}

	// Determine index of row under mouse, and whether it corresponds to a valid atom
	int row = ui.AtomsTable->rowAt(event->pos().y());

	atomsTableLastClicked_ = atomsTableAtomInRow(row);
	atomsTableLastHovered_ = atomsTableLastClicked_;

	QTableWidgetItem *item = ui.AtomsTable->itemAt(event->pos());
	if (item != NULL) ui.AtomsTable->editItem(item);
	
	event->accept();
}

void AtenWindow::atomsTableItemChanged(QTableWidgetItem* item)
{
	if (refreshing_) return;

	int row = item->row(), column = item->column();
	Atom* i = atomsTableAtomInRow(row);
	if (i == NULL)
	{
		printf("Internal Error: NULL atom pointer in AtenWindow::atomsTableItemChanged.\n");
		return;
	}

	// From the current visible columns list, determine where to poke the new value...
	double valueD;
	bool success;
	Vec3<double> v;
	int element;
	switch (atomsTableDisplayItems_.at(column))
	{
		case (AtenWindow::AtomXItem):
		case (AtenWindow::AtomYItem):
		case (AtenWindow::AtomZItem):
			element = atomsTableDisplayItems_.at(column) - AtenWindow::AtomXItem;
			valueD = item->text().toDouble(&success);
			v = i->r();
			if (success)
			{
				atomsTableLastModel_->beginUndoState("Edit %c coordinate of atom %i", char(88+element), i->id()+1);
				v[element] = valueD;
				atomsTableLastModel_->positionAtom(i, v);
				atomsTableLastModel_->endUndoState();
			}
			else item->setText(QString::number(v[element]));
			break;
		case (AtenWindow::AtomQItem):
			valueD = item->text().toDouble(&success);
			if (success)
			{
				atomsTableLastModel_->beginUndoState("Edit charge of atom %i", i->id()+1);
				atomsTableLastModel_->atomSetCharge(i, valueD);
				atomsTableLastModel_->endUndoState();
			}
			else item->setText(QString::number(i->charge()));
			break;
		default:
			printf("Internal Error: This column contains no editable data!\n");
			break;
	}

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::atomsTableMouseWheelEvent(QWheelEvent* event)
{
	// Grab current slider position
	int pos = ui.AtomsTableScrollBar->value();
	pos -= ui.AtomsTableScrollBar->pageStep() * (event->delta() / 120);
	ui.AtomsTableScrollBar->setValue(pos);
}
