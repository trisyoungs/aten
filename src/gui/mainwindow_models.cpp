/*
	*** Main Window - ModelsList Functions
	*** src/gui/mainwindow_models.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/selectfilter.h"
#include "templates/variantpointer.h"
#include <QInputDialog>
#include <QMessageBox>
#include <QFileDialog>

void AtenWindow::modelsListContextMenuRequested(const QPoint& point)
{
	// Get item at point
	QListWidgetItem* item = ui.ModelsList->itemAt(point);
	if (!item) return;

	// Get model pointer from item
	Model* model = VariantPointer<Model>(item->data(Qt::UserRole));
	if (!model) return;

	// Build the context menu to display
	QMenu contextMenu;
	QAction* renameAction = contextMenu.addAction("Rename...");

	// Show it
	QPoint menuPosition = ui.ModelsList->mapToGlobal(point);
	QAction* menuResult = contextMenu.exec(menuPosition);

	// What was clicked?
	if (menuResult == renameAction)
	{
		bool ok;
		QString text = QInputDialog::getText(this, tr("Rename Model: ") + model->name(), tr("New name:"), QLineEdit::Normal, model->name(), &ok);
		if (ok && !text.isEmpty())
		{
			CommandNode::run(Commands::SetName, "c", qPrintable(text));

			updateWidgets(AtenWindow::ModelsListTarget);
		}
	}
}

void AtenWindow::on_ModelsListToggleButton_clicked(bool checked)
{
	ui.ModelsListWidget->setVisible(checked);
	if (checked) updateWidgets(AtenWindow::ModelsListTarget);
}

void AtenWindow::on_ModelsList_itemSelectionChanged()
{
	if (refreshing_) return;

	// Loop over rows of list, setting 'visible' flags in model list accordingly
	QListWidgetItem* item;
	Model* model, *currentModel = NULL;
	for (int row = 0; row < ui.ModelsList->count(); ++row)
	{
		item = ui.ModelsList->item(row);
		model = VariantPointer<Model>(item->data(Qt::UserRole));
		if (model)
		{
			aten_.setModelVisible(model, item->isSelected());
			if (item->isSelected()) currentModel = model;
		}
	}

	// Is anything selected? If not, select one
	if (!currentModel)
	{
		if (aten_.nModels() == 0) Messenger::print("Internal Error: No model to select.");
		else currentModel = aten_.models();
	}

	// Need to set the (a) current model
	aten_.setCurrentModel(currentModel);

	updateWidgets(AtenWindow::AllTarget);
}

// Move to next model in list
void AtenWindow::on_ModelsNextButton_clicked(bool checked)
{
	// If multiple models are visible, step along to next visible model. Otherwise, just next in list
	if (aten_.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		RefListItem<Model,int>* ri;
		for (ri = aten_.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten_.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		aten_.setCurrentModel(ri->next == NULL ? aten_.visibleModels()->item : ri->next->item);
	}
	else
	{
		Model* m = aten_.currentModel();
		aten_.setCurrentModel(m->next == NULL ? aten_.models() : m->next);
	}

	updateWidgets(AtenWindow::AllTarget);
}

// Move to previous model in list
void AtenWindow::on_ModelsPreviousButton_clicked(bool checked)
{
	// If multiple models are visible, step back to previous visible model. Otherwise, just previous in list
	if (aten_.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		RefListItem<Model,int>* ri;
		for (ri = aten_.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten_.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		// If previous pointer is NULL, need to get the last item in the list by hand
		if (ri->prev != NULL) aten_.setCurrentModel(ri->prev->item);
		else for (ri = aten_.visibleModels(); ri != NULL; ri = ri->next) if (ri->next == NULL) aten_.setCurrentModel(ri->item);
	}
	else
	{
		Model* m = aten_.currentModel();
		aten_.setCurrentModel(m->prev == NULL ? aten_.model(aten_.nModels()-1) : m->prev);
	}

	updateWidgets(AtenWindow::AllTarget);
}


// Refresh model list
void AtenWindow::updateModelsList()
{
	Messenger::enter("AtenWindow::updateModelsList");

	int row;
	QListWidgetItem* item;
	Model* model, *itemModel;

	// First, iterate over existing rows in model list and remove any that aren't in our models
	row = 0;
	while (row < ui.ModelsList->count())
	{
		// Get model pointer from item, and see if its in the model list
		item = ui.ModelsList->item(row);
		model = VariantPointer<Model>(item->data(Qt::UserRole));
		if (!aten_.isModel(model))
		{
			// Not in the list anymore, so remove it from the widget
			item = ui.ModelsList->takeItem(row);
			if (item) delete item;
		}
		else
		{
			// Update current item status
			item->setSelected(model->isVisible());
			if (!model->iconIsValid()) model->setIcon(modelPixmap(model, ui.ModelsList->iconSize()));
			item->setIcon(model->icon());
			++row;
		}
	}

	// Now, iterate over the rows again, adding missing items...
	row = 0;
	for (model = aten_.models(); model != NULL; model = model->next)
	{
		// Get model pointer from current row, and see if it matches the current model
		if (row < ui.ModelsList->count()) item = ui.ModelsList->item(row);
		else item = NULL;
		itemModel = (item ? VariantPointer<Model>(item->data(Qt::UserRole)) : NULL);

		// If the model and itemModel pointers differ, create a new item at this position
		if (model != itemModel)
		{
			item = new QListWidgetItem;
			ui.ModelsList->insertItem(row, item);
		}

		// Update the current item
		item->setData(Qt::UserRole, VariantPointer<Model>(model));
		if (model->isVisible()) item->setSelected(true);
		if (!model->iconIsValid()) model->setIcon(modelPixmap(model, ui.ModelsList->iconSize()));
		item->setIcon(model->icon());

		// Increase row and move on
		++row;
	}

	Messenger::exit("AtenWindow::updateModelsList");
}

// Close specified model, saving first if requested
bool AtenWindow::closeModel(Model* m)
{
	QString text;
	Tree* filter;
	if (m->isModified())
	{
		// Create a modal message dialog
		text.sprintf("Model '%s' has been modified.", qPrintable(m->name()));
		int returnvalue = QMessageBox::warning(this, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				break;
				// Cancel close
			case (QMessageBox::Cancel):
				return false;
				// Save model before quit
			case (QMessageBox::Save):
				// Temporarily disable undo/redo for the model, save, and re-enable
				m->disableUndoRedo();
				// If model has a filter set, just save it
				filter = m->filter();
				if (filter != NULL) filter->executeWrite(m->filename());
				else if (runSaveModelDialog())
				{
					m->setFilter(saveModelFilter_);
					m->setFilename(saveModelFilename_);
					if (!saveModelFilter_->executeWrite(saveModelFilename_))
					{
						Messenger::print("Not saved.");
						m->enableUndoRedo();
						return false;
					}
				}
				else
				{
					m->enableUndoRedo();
					return false;
				}
				break;
		}
	}

	// Remove model and update gui
	aten_.removeModel(m);

	return true;
}

// Check the status of all models, asking to save before close if necessary
bool AtenWindow::saveBeforeClose()
{
	while (aten_.models())
	{
		if (!closeModel(aten_.models())) return false;

		// Update GUI
		updateWidgets(AtenWindow::AllTarget);
	}
	return true;
}

// Run SaveModel dialog to get filename and format
bool AtenWindow::runSaveModelDialog()
{
	saveModelFilename_.clear();
	saveModelFilter_ = NULL;
	Tree* filter = NULL;
	static QString selectedFilter(aten_.filters(FilterData::ModelExport) == NULL ? NULL : aten_.filters(FilterData::ModelExport)->item->filter.name());
	static QDir currentDirectory_(aten_.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Model", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ModelExport), &selectedFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Grab file extension and search for it in our current lists...
		QString ext = QFileInfo(filename).suffix();
		RefList<Tree,int> filters;
		if (ext.isEmpty())
		{
			QFileInfo fileInfo( filename );
			// Does this filename uniquely identify a specific filter?
			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesNameMatch(qPrintable(fileInfo.fileName()))) filters.add(ri->item);
			}
			Messenger::print(Messenger::Verbose, "Exact filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());

			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			AtenSelectFilter selectFilter(*this);
			if (filters.nItems() != 0) filter = selectFilter.selectFilter("Name matches one or more model export filters.", &filters, aten_.filterList(FilterData::ModelExport));
			else
			{
				filter = selectFilter.selectFilter("Couldn't determine format to save expression in.", NULL, aten_.filterList(FilterData::ModelExport), true);
				if ((filter != NULL) && selectFilter.appendExtension())
				{
					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
				}
			}
		}
		else
		{
			// Does this extension uniquely identify a specific filter?
			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesExtensionMatch(ext)) filters.add(ri->item);
			}
			Messenger::print(Messenger::Verbose, "Extension of filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() == 1) filter = filters.first()->item;
			else if (filters.nItems() > 1)
			{
				AtenSelectFilter selectFilter(*this);
				filter = selectFilter.selectFilter("Extension matches one or more model export filters.", &filters, aten_.filterList(FilterData::ModelExport));
			}
			else
			{
				AtenSelectFilter selectFilter(*this);
				filter = selectFilter.selectFilter("Extension doesn't match any in known model export filters.", NULL, aten_.filterList(FilterData::ModelExport), true);
				if ((filter != NULL) && selectFilter.appendExtension())
				{
					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
				}
			}
		}
		saveModelFilter_ = filter;
		saveModelFilename_ = qPrintable(filename);
		if (filter == NULL) Messenger::print("No filter selected to save file '%s'. Not saved.", qPrintable(saveModelFilename_));
		return (saveModelFilter_ == NULL ? false : true);
	}
	else return false;
}
