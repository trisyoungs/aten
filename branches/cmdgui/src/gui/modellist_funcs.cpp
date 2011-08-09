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
#include "gui/mainwindow.h"
#include "model/model.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Constructor
ModelListWidget::ModelListWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
	refreshing_ = FALSE;
	QObject::connect(ui.ModelTree, SIGNAL(mousePressEvent(QMouseEvent*)), this, SLOT(treeMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.ModelTree, SIGNAL(mouseReleaseEvent(QMouseEvent*)), this, SLOT(treeMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.ModelTree, SIGNAL(mouseMoveEvent(QMouseEvent*)), this, SLOT(treeMouseMoveEvent(QMouseEvent*)));
	QObject::connect(ui.ModelTree, SIGNAL(mouseDoubleClickEvent(QMouseEvent*)), this, SLOT(treeMouseDoubleClickEvent(QMouseEvent*)));
	
	// Create context menu
	QAction *action;
	contextMenu_ = new QMenu(this);
	action = contextMenu_->addAction("Rename");
	QObject::connect(action, SIGNAL(triggered(bool)), this, SLOT(renameModel(bool)));
	action = contextMenu_->addAction("Close Selected");
	QObject::connect(action, SIGNAL(triggered(bool)), this, SLOT(closeSelectedModels(bool)));
	action = contextMenu_->addAction("Close Unselected");
	QObject::connect(action, SIGNAL(triggered(bool)), this, SLOT(closeUnselectedModels(bool)));
	action = contextMenu_->addAction("Close This");
	QObject::connect(action, SIGNAL(triggered(bool)), this, SLOT(closeThisModel(bool)));
}

// Show the widget, refreshing at the same time
void ModelListWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.ModelListButton->setChecked(TRUE);
}

// Refresh the model list
void ModelListWidget::refresh()
{
	msg.enter("ModelListWidget::refresh");
	// If the model list is not visible, don't do anything
	if (refreshing_ || (!gui.modelListWidget->isVisible()))
	{
		msg.exit("ModelListWidget::refresh");
		return;
	}
	
	refreshing_ = TRUE;
	
	// Set number of visible models and model total
	ui.ModelsPerRowSpin->setValue(prefs.nModelsPerRow());
	ui.TotalLoadedModelsLabel->setText(itoa(aten.nModels()));

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
		if (m->isVisible() || (m == aten.currentModel())) item->setSelected(TRUE);
	}
	ui.ModelTree->resizeColumnToContents(0);
	ui.ModelTree->resizeColumnToContents(1);
	
	refreshing_ = FALSE;
	
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

	refreshing_ = TRUE;
	
	// Here we must consider the rules of Aten - that a model must *always* be current.
	// So, if this toggle would result in there being no visible models, ignore the request.
	// Similarly, if this item *can* legitimately be deselected, we must select another current model.

	bool selected = twi->isSelected();
	
	// So, check for a single selected model for which toggling would be invalid
	if (selected && (aten.nVisibleModels() == 1)) return;

	// Now, if the item is *not* selected we can safely select it and make it current
	if (!selected)
	{
		twi->setSelected(TRUE);
		aten.setModelVisible(m,TRUE);
	}
	else
	{
		// We are deselecting, so need to check if its currently the active model
		twi->setSelected(FALSE);
		aten.setModelVisible(m,FALSE);
		if (m == aten.currentModel())
		{
			// Grab the last visible model added to the list
			Refitem<Model,int> *ri;
			m = NULL;
			for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->item != aten.currentModel()) m = ri->item;
			if (ri == NULL) printf("Internal Error: Couldn't reassign active model in ModelListWidget::treeMouseMoveEvent.\n");
			else aten.setCurrentModel(m);
		}
	}
	refreshing_ = FALSE;
	gui.mainWidget()->postRedisplay();
}

// Deselect all items in list (except the supplied item)
void ModelListWidget::deselectAll(TTreeWidgetItem *selectitem)
{
	refreshing_ = TRUE;
	
	// Check supplied except item
	if (selectitem == NULL)
	{
		printf("Internal Error: Can't deselect every item in the ModelList (NULL 'selectitem' except pointer provided).\n");
		return;
	}
	
	// Clear selected items
	TTreeWidgetItem *twi;
	Model *m;
	foreach(QTreeWidgetItem *item, ui.ModelTree->selectedItems())
	{
		twi = (TTreeWidgetItem*) item;
		m = (Model*) twi->data.asPointer(VTypes::ModelData);
		aten.setModelVisible(m, FALSE);
		twi->setSelected(FALSE);
	}
	
	// Make sure the excepted item is selected
	toggleItem(selectitem);
	
	refreshing_ = FALSE;
}

void ModelListWidget::on_RefreshIconsButton_clicked(bool checked)
{
	for (Model *m = aten.models(); m != NULL; m = m->next) m->regenerateIcon();
	refresh();
}

void ModelListWidget::on_ModelsPerRowSpin_valueChanged(int value)
{
	prefs.setNModelsPerRow(value);
	gui.mainWidget()->postRedisplay();
}

// Mouse pressed on ModelList
void ModelListWidget::treeMousePressEvent(QMouseEvent *event)
{
	// Left Button is selection operator, right is context menu
	if ((event->buttons()&Qt::LeftButton))
	{
		// Was an item clicked?
		lastClicked_ = itemUnderMouse(event->pos());
		if (lastClicked_ != NULL)
		{
			// Clear all old selected items, unless Ctrl was pressed at the same time
			if (event->modifiers()&Qt::ControlModifier) toggleItem(lastClicked_);
			else 
			{
				deselectAll(lastClicked_);
				Model *m = (Model*) lastClicked_->data.asPointer(VTypes::ModelData);
				aten.setCurrentModel(m);
			}
			gui.update(GuiQt::AllTarget - GuiQt::ModelsTarget);
		}
		lastHovered_ = lastClicked_;
	}
	else if ((event->buttons()&Qt::RightButton))
	{
		// Is there an item under the mouse?
		lastClicked_ = itemUnderMouse(event->pos());
		if (lastClicked_ == NULL) return;
		// Call context menu...
		contextMenu_->exec(event->globalPos());
	}
}

// Mouse releaseed on ModelList
void ModelListWidget::treeMouseReleaseEvent(QMouseEvent *event)
{
	lastHovered_ = NULL;
	gui.update(GuiQt::AllTarget-GuiQt::ModelsTarget);
}

// Mouse moved over ModelList
void ModelListWidget::treeMouseMoveEvent(QMouseEvent *event)
{
	if (!(event->buttons()&Qt::LeftButton)) return;
	TTreeWidgetItem *twi = itemUnderMouse(event->pos());
	// If the current hovered item is the same as the last one, ignore it
	if (twi != lastHovered_)
	{
		toggleItem(twi);
		lastHovered_ = twi;
	}
}

// Mouse double-clicked over ModelList
void ModelListWidget::treeMouseDoubleClickEvent(QMouseEvent *event)
{
	// Left Button double-click is rename function
	if ((event->buttons()&Qt::LeftButton))
	{
		// Was an item clicked?
		lastClicked_ = itemUnderMouse(event->pos());
		if (lastClicked_ == NULL) return;
			
		Model *m = (Model*) lastClicked_->data.asPointer(VTypes::ModelData);
		m = m->renderSourceModel();
		bool ok;
		QString text = QInputDialog::getText(this, tr("Rename Model/Frame: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
		if (ok && !text.isEmpty())
		{
			// Create a temporary Bundle
			Bundle bundle(m);
			CommandNode::run(Command::SetName, "c", qPrintable(text));
			gui.update(GuiQt::ModelsTarget);
		}
	}
}

void ModelListWidget::renameModel(bool checked)
{
	// Check clicked item...
	if (lastClicked_ == NULL) return;
		
	Model *m = (Model*) lastClicked_->data.asPointer(VTypes::ModelData);
	m = m->renderSourceModel();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model/Frame: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		// Create a temporary Bundle
		Bundle bundle(m);
		CommandNode::run(Command::SetName, "c", qPrintable(text));
		gui.update(GuiQt::ModelsTarget);
	}
}

// Close selected models in list
void ModelListWidget::closeSelectedModels(bool checked)
{
	TTreeWidgetItem *twi;
	Model *m;
	foreach(QTreeWidgetItem *item, ui.ModelTree->selectedItems())
	{
		twi = (TTreeWidgetItem*) item;
		m = (Model*) twi->data.asPointer(VTypes::ModelData);
		if (!aten.closeModel(m)) break;
	}
	// There are probably now no selected models, and potentially none left at all...
	gui.update(GuiQt::AllTarget);
}

// Close unselected models in list
void ModelListWidget::closeUnselectedModels(bool checked)
{
	// Close all models except the currently-selected ones
	// Awkward - QTreeWidget does not return, apparently, even a list of all items...

	// First, create a list of all loaded models...
	Reflist<Model,int> xmodels;
	Model *m;
	TTreeWidgetItem *twi;
	for (m = aten.models(); m != NULL; m = m->next) xmodels.add(m);
	// ...then prune it with the current model selection from the treeview
	foreach(QTreeWidgetItem *item, ui.ModelTree->selectedItems())
	{
		twi = (TTreeWidgetItem*) item;
		m = (Model*) twi->data.asPointer(VTypes::ModelData);
		xmodels.remove(m);
	}

	// The xmodels list now contains all unselected models....
	for (Refitem<Model,int> *ri = xmodels.first(); ri != NULL; ri = ri->next)
	{
		if (!aten.closeModel(ri->item)) break;
	}
	gui.update(GuiQt::AllTarget);
}

void ModelListWidget::closeThisModel(bool checked)
{
	// If there is no clicked item, ignore
	if (lastClicked_ == NULL) return;
	
	// Close clicked model
	Model *m = (Model*) lastClicked_->data.asPointer(VTypes::ModelData);
	aten.closeModel(m);
	gui.update(GuiQt::AllTarget);
}

// Window closed
void ModelListWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.ModelListButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();
	event->accept();
}
