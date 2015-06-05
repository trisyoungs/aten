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
#include "templates/variantpointer.h"

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
		else ++row;
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

void AtenWindow::on_ModelsListToggleButton_clicked(bool checked)
{
	ui.ModelsList->setVisible(checked);
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

	// Need to set the (a) current model
	aten_.setCurrentModel(currentModel);

	updateWidgets(AtenWindow::AllTarget);
}
