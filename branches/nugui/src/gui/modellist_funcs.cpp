/*
	*** Model List Dock Widget
	*** src/gui/modellist_funcs.cpp
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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/modellist.h"
#include "gui/toolbox.h"
#include "model/model.h"

// Constructor
ModelListWidget::ModelListWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
	
	QObject::connect(ui.ModelTree, SIGNAL(mousePressEvent(QMouseEvent*)), this, SLOT(treeMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.ModelTree, SIGNAL(mouseReleaseEvent(QMouseEvent*)), this, SLOT(treeMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.ModelTree, SIGNAL(mouseMoveEvent(QMouseEvent*)), this, SLOT(treeMouseMoveEvent(QMouseEvent*)));
}

// Destructor
ModelListWidget::~ModelListWidget()
{
}

// Show the widget, refreshing at the same time
void ModelListWidget::showWidget()
{
	show();
	refresh();
}

// Refresh the model list
void ModelListWidget::refresh()
{
	msg.enter("ModelListWidget::refresh");
	// If the model list is not visible, don't do anything
	if (!gui.modelListWidget->isVisible())
	{
		msg.exit("ModelListWidget::refresh");
		return;
	}
	// Clear the current list
	ui.ModelTree->clear();
	ui.ModelTree->setColumnCount(2);
	TTreeWidgetItem *item;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		// Filter?
// 		if (!filterText_.isEmpty() && (strstr(lowerCase(f->masterModel()->name()), filterText_.get()) == 0)) continue;
		item = new TTreeWidgetItem(ui.ModelTree);
		item->data.set(VTypes::ModelData, m);
		item->setIcon(0,m->icon());
		item->setText(1,m->name());
		item->setTextAlignment(1, Qt::AlignLeft | Qt::AlignTop);
		if (m->isVisible()) item->setSelected(TRUE);
	}
	ui.ModelTree->resizeColumnToContents(0);
	ui.ModelTree->resizeColumnToContents(1);
	msg.exit("ModelListWidget::refresh");
}

// Return item under mouse (if any)
TTreeWidgetItem *ModelListWidget::itemUnderMouse(const QPoint &pos)
{
	QTreeWidgetItem *twi = ui.ModelTree->itemAt(pos);
	if (twi == NULL) return NULL;
	else return (TTreeWidgetItem*) twi;
}

// Toggle the selection state in the model
void ModelListWidget::toggleItem(TTreeWidgetItem *twi)
{
	// Check for no item or header item
	if (twi == NULL) return;
	Model *m = (Model*) twi->data.asPointer(VTypes::ModelData);
	if (m == NULL) return;
	bool state = twi->isSelected();
	twi->setSelected(!state);
	aten.setModelVisible(m,!state);
}

// Select tree widget item *and* model atom, provided the tree widget item is not selected already
void ModelListWidget::selectItem(TTreeWidgetItem *twi)
{
	if (twi == NULL) return;
	if (twi->isSelected()) return;
	twi->setSelected(TRUE);
	Model *m = (Model*) twi->data.asPointer(VTypes::ModelData);
	if (m == NULL) return;
	aten.setModelVisible(m,TRUE);
}

// Deselect tree widget item *and* model atom, provided the tree widget item is not deselected already
void ModelListWidget::deselectItem(TTreeWidgetItem *twi)
{
	if (twi == NULL) return;
	if (!twi->isSelected()) return;
	twi->setSelected(FALSE);
	Model *m = (Model*) twi->data.asPointer(VTypes::ModelData);
	if (m == NULL) return;
	aten.setModelVisible(m,FALSE);
}

// Deselect all items in list
void ModelListWidget::deselectAll()
{
	TTreeWidgetItem *twi;
	foreach(QTreeWidgetItem *item, ui.ModelTree->selectedItems())
	{
		twi = (TTreeWidgetItem*) item;
		deselectItem(twi);
	};
}

void ModelListWidget::updateSelection()
{
}

// Mouse pressed on ModelList
void ModelListWidget::treeMousePressEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
	
	// Was an item clicked?
	lastClicked_ = itemUnderMouse(event->pos());
	if (lastClicked_ != NULL)
	{
		// Clear all old selected items, unless Ctrl was pressed at the same time
		if (TRUE) deselectAll();	// TGAY Ctrl!

		// If no item is selected (visible) then set this one to be the current model
		if (lastClicked_->data.type() == VTypes::ModelData)
		{
			// Toggle the selection status of the item
			toggleItem(lastClicked_);
			Model *m = (Model*) lastClicked_->data.asPointer(VTypes::ModelData);
			if (m == NULL) return;
			aten.setCurrentModel(m);
		}
	}
	lastHovered_ = lastClicked_;
}

// Mouse releaseed on ModelList
void ModelListWidget::treeMouseReleaseEvent(QMouseEvent *event)
{
	// 	printf("Mouse release event.\n");
	lastHovered_ = NULL;
	gui.update();
}

// Mouse moved over ModelList
void ModelListWidget::treeMouseMoveEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
	// 	printf("Mouse move event.\n");
	TTreeWidgetItem *twi = itemUnderMouse(event->pos());
	// If the current hovered item is the same as the last one, ignore it
	if (twi != lastHovered_)
	{
		toggleItem(twi);
		lastHovered_ = twi;
		gui.update();
	}
}

// Window closed
void ModelListWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.ModelListButton->setChecked(FALSE);
	event->accept();
}

