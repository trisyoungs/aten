/*
	*** Main Window - Grids Panel Functions
	*** src/gui/mainwindow_panel_grids.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"
#include "command/commands.h"
#include "templates/variantpointer.h"
#include <QFileDialog>
#include <QInputDialog>

// Update grids panel
void AtenWindow::updateGridsPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateGridPanel");

	Grid* currentGrid;
	if (!aten_.currentGrid(currentGrid))
	{
		Messenger::exit("AtenWindow::updateGridPanel");
		return;
	}

	ui.GridsList->clear();
	if (sourceModel)
	{
		// Update the list of grids
		int count = 0;
		for (Grid* g = sourceModel->grids(); g != NULL; g = g->next, ++count)
		{
			QListWidgetItem* item = new QListWidgetItem(g->name());
			item->setData(Qt::UserRole, VariantPointer<Grid>(g));
			item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsUserCheckable | Qt::ItemIsEnabled);
			item->setToolTip(g->filename());
			ui.GridsList->addItem(item);

			item->setCheckState(g->isVisible() ? Qt::Checked : Qt::Unchecked);

			if (g == currentGrid) ui.GridsList->setCurrentRow(count);
		}
	}

	updateGridInformation(currentGrid);

	ui.GridsManageOpenButton->setEnabled(!aten_.fileDialogFilters(FilterData::GridImport).isEmpty());
	ui.GridsManageRemoveButton->setEnabled(ui.GridsList->currentRow() != -1);

	Messenger::exit("AtenWindow::updateGridPanel");
}

// Update grid information
void AtenWindow::updateGridInformation(Grid* sourceGrid)
{
	// Enable / disable controls
	ui.GridsPrimaryLowerCutoffSpin->setEnabled(sourceGrid);
	ui.GridsPrimaryUpperCutoffSpin->setEnabled(sourceGrid);
	ui.GridsPrimarySetButton->setEnabled(sourceGrid);
	ui.GridsPrimaryColourButton->setEnabled(sourceGrid);
	ui.GridsPrimaryStyleButton->setEnabled(sourceGrid);
	ui.GridsSecondaryLowerCutoffSpin->setEnabled(sourceGrid);
	ui.GridsSecondaryUpperCutoffSpin->setEnabled(sourceGrid);
	ui.GridsSecondaryColourButton->setEnabled(sourceGrid);
	ui.GridsSecondarySetButton->setEnabled(sourceGrid);
	ui.GridsSecondaryStyleButton->setEnabled(sourceGrid);
	ui.GridsSecondaryActiveButton->setEnabled(sourceGrid);
	ui.GridsOptionsOutlineButton->setEnabled(sourceGrid);
	ui.GridsOptionsPeriodicButton->setEnabled(sourceGrid);
	if (!sourceGrid) return;

	ReturnValue rv;

	// Primary surface
	ui.GridsPrimaryLowerCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum(), 100);
	ui.GridsPrimaryLowerCutoffSpin->setValue(sourceGrid->lowerPrimaryCutoff());
	ui.GridsPrimaryUpperCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum(), 100);
	ui.GridsPrimaryUpperCutoffSpin->setValue(sourceGrid->upperPrimaryCutoff());
	rv.setArray(VTypes::DoubleData, sourceGrid->primaryColour(), 4);
	ui.GridsPrimaryColourButton->callPopupMethod("setCurrentColour", rv);
	ui.GridsPrimaryStyleButton->callPopupMethod("updateButtonIcon", rv = QString(Grid::surfaceStyle(sourceGrid->primaryStyle())));

	// Secondary surface
	ui.GridsSecondaryLowerCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum(), 100);
	ui.GridsSecondaryLowerCutoffSpin->setValue(sourceGrid->lowerSecondaryCutoff());
	ui.GridsSecondaryUpperCutoffSpin->setRange(true, sourceGrid->minimum(), true, sourceGrid->maximum(), 100);
	ui.GridsSecondaryUpperCutoffSpin->setValue(sourceGrid->upperSecondaryCutoff());
	rv.setArray(VTypes::DoubleData, sourceGrid->secondaryColour(), 4);
	ui.GridsSecondaryColourButton->callPopupMethod("setCurrentColour", rv);
	ui.GridsSecondaryStyleButton->callPopupMethod("updateButtonIcon", rv = QString(Grid::surfaceStyle(sourceGrid->secondaryStyle())));

	// Enable / disable secondary surface controls
	ui.GridsSecondaryActiveButton->setChecked(sourceGrid->useSecondary());
	ui.GridsSecondaryLowerCutoffSpin->setEnabled(sourceGrid->useSecondary());
	ui.GridsSecondaryUpperCutoffSpin->setEnabled(sourceGrid->useSecondary());
	ui.GridsSecondaryColourButton->setEnabled(sourceGrid->useSecondary());
	ui.GridsSecondaryStyleButton->setEnabled(sourceGrid->useSecondary());

	// Options
	ui.GridsOptionsOutlineButton->setChecked(sourceGrid->outlineVolume());
	ui.GridsOptionsPeriodicButton->setChecked(sourceGrid->periodic());
}

/*
 * Manage
 */

void AtenWindow::on_GridsManageOpenButton_clicked(bool checked)
{
	Tree* filter;
	static QDir currentDirectory_(aten_.workDir());
	QString selFilter;
	QStringList filenames = QFileDialog::getOpenFileNames(this, "Open Grid", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::GridImport), &selFilter);
	for (int n=0; n<filenames.count(); ++n)
	{
		QString filename = filenames.at(n);

		// Store path for next use
		currentDirectory_.setPath(filename);
		
		// Find the filter that was selected
		filter = aten().findFilterByDescription(FilterData::GridImport, qPrintable(selFilter));
		if (filter == NULL) filter = aten_.probeFile(qPrintable(filename), FilterData::GridImport);
		if (filter != NULL)
		{
			// Run any import options in the filter
			if (!filter->executeRead(qPrintable(filename))) return;
		}
	}

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_GridsManageRemoveButton_clicked(bool checked)
{
	if (ui.GridsList->currentRow() == -1) return;

	// Get current selected grid
	QListWidgetItem* item = ui.GridsList->item(ui.GridsList->currentRow());
	if (!item) return;

	// Get grid from current item
	Grid* grid = (Grid*) VariantPointer<Grid>(item->data(Qt::UserRole));
	if (!grid) return;

	Model* parentModel = grid->parent();
	if (parentModel)
	{
		aten_.setCurrentGrid(grid->next ? grid->next : grid->prev);
		parentModel->removeGrid(grid);
	}

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_GridsList_currentRowChanged(int row)
{
	if (refreshing_ || (row == -1)) return;

	// Get widget for specified row
	QListWidgetItem* item = ui.GridsList->item(row);
	if (!item) return;

	// Get grid from current item
	Grid* grid = (Grid*) VariantPointer<Grid>(item->data(Qt::UserRole));
	if (!grid) return;

	aten_.setCurrentGrid(grid);

	refreshing_ = true;

	updateGridInformation(grid);

	refreshing_ = false;
}

void AtenWindow::on_GridsList_itemChanged(QListWidgetItem* item)
{
	if (refreshing_ || (!item)) return;

	// Get grid from current item
	Grid* grid = (Grid*) VariantPointer<Grid>(item->data(Qt::UserRole));
	if (!grid) return;

	// This signal could have originated from the checkstate being changed, so check the text...
	if (item->text() != grid->name())
	{
		grid->setName(item->text());

		// ATEN2 TODO Update main window undo/redo (when this is undo/redoable)
	}

	// What about the checkstate?
	bool checked = item->checkState() == Qt::Checked;
	if (checked != grid->isVisible()) grid->setVisible(checked);

	updateWidgets(AtenWindow::MainViewTarget);
}

// Context menu requested for GridsList
void AtenWindow::gridsListContextMenuRequested(const QPoint& point)
{
	// Is there a current grid?
	Grid* currentGrid;
	if (!aten_.currentGrid(currentGrid)) return;

	// Build the context menu to display
	QMenu contextMenu;
	QAction* action;
	// -- Rename / Reload
	QAction* renameAction = contextMenu.addAction("&Rename");
	QAction* reloadAction = contextMenu.addAction("Re&load");	
	// -- Main 'edit' functions
	contextMenu.addSeparator();
	QAction* copyAction = contextMenu.addAction("&Copy");
	QAction* cutAction = contextMenu.addAction("Cu&t");
	QAction* pasteAction = contextMenu.addAction("&Paste");
	contextMenu.addSeparator();
	QAction* duplicateAction = contextMenu.addAction("D&uplicate");
	QAction* deleteAction = contextMenu.addAction("&Delete");
	
	// Show it
	QAction* menuResult = contextMenu.exec(QCursor::pos());

	// What was clicked?
	if (menuResult == renameAction)
	{
		bool ok = false;
		QString newName = QInputDialog::getText(this, "Rename Grid", "Enter new name for grid", QLineEdit::Normal, currentGrid->name(), &ok);
		if (ok)
		{
			currentGrid->setName(newName);
			updateWidgets(AtenWindow::GridsPanelTarget);
		}
	}
	else if (menuResult == reloadAction)
	{
		// TODO
	}
	else if (menuResult == copyAction)
	{
		aten_.copyGrid(currentGrid);
	}
	else if (menuResult == cutAction)
	{
		Model* parentModel = currentGrid->parent();
		if (!parentModel) return;
		aten().copyGrid(currentGrid);
		aten_.setCurrentGrid(currentGrid->next ? currentGrid->next : currentGrid->prev);
		parentModel->removeGrid(currentGrid);
		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
	}
	else if (menuResult == deleteAction)
	{
		Model* parentModel = currentGrid->parent();
		if (!parentModel) return;
		aten_.setCurrentGrid(currentGrid->next ? currentGrid->next : currentGrid->prev);
		parentModel->removeGrid(currentGrid);
		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
	}
	else if (menuResult == pasteAction)
	{
		Model* parentModel = currentGrid->parent();
		if (!parentModel) return;
		Grid* clipGrid = aten_.gridClipboard();
		if (!clipGrid) return;
		Grid* newGrid = parentModel->addGrid();
		*newGrid = *clipGrid;
		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
	}
	else if (menuResult == duplicateAction)
	{
		Model* parentModel = currentGrid->parent();
		if (!parentModel) return;
		Grid* newGrid = parentModel->addGrid();
		*newGrid = *currentGrid;
		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
	}
}

void AtenWindow::recreateGridsForView()
{
	// For all models, log a change so that we recreate all grids
	for (Model* m = aten_.models(); m != NULL; m = m->next)
	{
		for (Grid* g = m->grids(); g != NULL; g = g->next)
		{
			g->logChange();
		}
	}

	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Primary Surface
 */

void AtenWindow::on_GridsPrimaryLowerCutoffSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	CommandNode::run(Commands::GridCutoff, "d", value);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_GridsPrimaryUpperCutoffSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	CommandNode::run(Commands::GridCutoff, "dd", ui.GridsPrimaryLowerCutoffSpin->value(), value);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_GridsPrimaryColourButton_popupChanged()
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	ReturnValue rv;
	bool success;
	if (!ui.GridsPrimaryColourButton->callPopupMethod("currentColour", rv)) return;
	CommandNode::run(Commands::GridColour, "dddd", rv.asDouble(0, success), rv.asDouble(1, success), rv.asDouble(2, success), rv.asDouble(3, success));

	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Secondary Surface
 */


void AtenWindow::on_GridsSecondaryActiveButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	CommandNode::run(Commands::GridSecondary, "i", checked);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_GridsSecondaryLowerCutoffSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	CommandNode::run(Commands::GridCutoffSecondary, "d", value);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_GridsSecondaryUpperCutoffSpin_valueChanged(double value)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	CommandNode::run(Commands::GridCutoffSecondary, "dd", ui.GridsSecondaryLowerCutoffSpin->value(), value);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_GridsSecondaryColourButton_popupChanged()
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	ReturnValue rv;
	bool success;
	if (!ui.GridsSecondaryColourButton->callPopupMethod("currentColour", rv)) return;
	CommandNode::run(Commands::GridColourSecondary, "dddd", rv.asDouble(0, success), rv.asDouble(1, success), rv.asDouble(2, success), rv.asDouble(3, success));

	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Options
 */

void AtenWindow::on_GridsOptionsOutlineButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	// Run command
	CommandNode::run(Commands::GridOutline, "i", checked);

	// Update
	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_GridsOptionsPeriodicButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current grid
	Grid* currentGrid;
	if (!aten().currentGrid(currentGrid)) return;

	// Run command
	CommandNode::run(Commands::GridPeriodic, "i", checked);

	// Update
	updateWidgets(AtenWindow::MainViewTarget);
}
