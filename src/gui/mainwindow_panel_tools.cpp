/*
	*** Main Window - Tools Panel Functions
	*** src/gui/mainwindow_panel_tools.cpp
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
#include "templates/variantpointer.h"
#include <QFileDialog>

// Update tools panel
void AtenWindow::updateToolsPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateToolsPanel");

	// Refresh scripts
	int currentRow = 0;
	QListWidgetItem* item;
	Program* scriptItem;
	Program* object = aten_.scripts();
	while (object)
	{
		// For the current object, see if the item data at the currentRow matches it.
		// If there is no item at all, create one and set/update the data
		// If the item data does not match, delete the item and loop

		// Get list widget item at the current row
		item = ui.ScriptsList->item(currentRow);
		if (!item)
		{
			// No more items in the list, so create a new, empty one
			item = new QListWidgetItem();
			ui.ScriptsList->insertItem(currentRow, item);
		}
		else
		{
			// Existing item - does it correspond to our current object?
			scriptItem = (Program*) VariantPointer<Program>(item->data(Qt::UserRole));
			if (scriptItem != object)
			{
				// Delete this item, since it is 'out of order' w.r.t. our main list
				item = ui.ScriptsList->takeItem(currentRow);
				delete item;
				continue;
			}
		}

		// Set / update the data in our item
		item->setText(object->name());
		item->setToolTip(QString("Loaded from '%1'").arg(object->filename()));
		item->setData(Qt::UserRole, VariantPointer<Program>(object));
		item->setForeground(object->generatedSuccessfully() ? Qt::black : Qt::red);
// 		QFont font = item->font();
// 		item->setFont(item->font().setItalic());

		// Move on to next object / row
		object = object->next;
		++currentRow;
	}
	// Clearup - delete any items remaining in the list past the currentRow
	while (ui.ScriptsList->item(currentRow))
	{
		item = ui.ScriptsList->takeItem(currentRow);
		delete item;
	}

	// Is there a current item?
	if ((!ui.ScriptsList->currentItem()) && (ui.ScriptsList->count() > 0)) ui.ScriptsList->setCurrentRow(0);

	// Enable / disable controls
	ui.ToolsScriptsRemoveButton->setEnabled(ui.ScriptsList->currentItem());
	ui.ToolsScriptsReloadButton->setEnabled(ui.ScriptsList->currentItem());
	ui.ToolsScriptsRunButton->setEnabled(ui.ScriptsList->currentItem());

	Messenger::exit("AtenWindow::updateToolsPanel");
}

/*
 * Scripts
 */

void AtenWindow::on_ToolsScriptsOpenButton_clicked(bool checked)
{
	static QDir currentDirectory_(aten_.workDir());
	QString filename = QFileDialog::getOpenFileName(this, "Load Script", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ExpressionImport));
	if (!filename.isEmpty())
	{
		Program* script = aten_.addScript();
		if (script->generateFromFile(filename, filename))
		{
			Messenger::print("Successfully loaded script.");
	
			updateWidgets(AtenWindow::ToolsPanelTarget);
		}
		else aten_.removeScript(script);

		// Store path for next use
		currentDirectory_.setPath(filename);
	}

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ToolsScriptsRemoveButton_clicked(bool checked)
{
	// Get currently-selected script
	QListWidgetItem* item = ui.ScriptsList->currentItem();
	if (!item) return;

	Program* script = (Program*) VariantPointer<Program>(item->data(Qt::UserRole));
	if (!script) return;

	aten_.removeScript(script);

	updateWidgets(AtenWindow::MainViewTarget + AtenWindow::ToolsPanelTarget);
}

void AtenWindow::on_ToolsScriptsReloadButton_clicked(bool checked)
{
	// Get currently-selected script
	QListWidgetItem* item = ui.ScriptsList->currentItem();
	if (!item) return;

	Program* script = (Program*) VariantPointer<Program>(item->data(Qt::UserRole));
	if (!script) return;

	script->reload();

	updateWidgets(AtenWindow::MainViewTarget + AtenWindow::ToolsPanelTarget);
}

void AtenWindow::on_ToolsScriptsRunButton_clicked(bool checked)
{
	// Get currently-selected script
	QListWidgetItem* item = ui.ScriptsList->currentItem();
	if (!item) return;

	Program* script = (Program*) VariantPointer<Program>(item->data(Qt::UserRole));
	if (!script) return;

	// Run it
	ReturnValue rv;
	if (!script->execute(rv)) Messenger::print("Script execution failed.");

	updateWidgets(AtenWindow::AllTarget);
}

/*
 * Atoms
 */

void AtenWindow::on_ToolsAtomsReorderButton_clicked(bool checked)
{
	CommandNode::run(Commands::ReOrder, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ToolsAtomsZMatrixButton_clicked(bool checked)
{
	zMatrixDialog_.refresh(true);
	zMatrixDialog_.show();
}

/*
 * Disorder
 */

void AtenWindow::on_ToolsDisorderCreateButton_clicked(bool checked)
{
	disorderWizard_.run();
}

/*
 * Surface
 */

void AtenWindow::on_ToolsSurfaceTerminateButton_clicked(bool checked)
{
	// First check - are any atoms selected
	Model* currentModel = aten().currentModelOrFrame();
	if (currentModel->nSelected() == 0)
	{
		Messenger::print("No atoms selected in current model, so nothing to terminate.");
		return;
	}

	// Run the command
	CommandNode::run(Commands::Terminate, "");

	// Update the main window
	updateWidgets(AtenWindow::MainViewTarget);
}
