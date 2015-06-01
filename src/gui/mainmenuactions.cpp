/*
	*** Main Menu Actions
	*** src/gui/mainmenuactions.cpp
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

#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QInputDialog>
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/saveimage.h"
#include "gui/prefs.h"
#include "gui/forcefields.h"
#include "base/sysfunc.h"

/*
 * File Menu
 */


// Modify export options for current model's associated filter
void AtenWindow::on_actionExportOptions_triggered(bool checked)
{
	Model* m = aten_.currentModelOrFrame();
	if (m->filter() == NULL) Messenger::print("No filter currently assigned to model '%s', so there are no export options.", qPrintable(m->name()));
	else m->filter()->defaultDialog().execute();
}



/*
 * Edit Actions
 */


void AtenWindow::on_actionEditPasteTranslated_triggered(bool checked)
{
	// Static tree containing a single tree with variables and dialog control definitions
	Tree dialog;
	TreeGui &ui = dialog.defaultDialog();
	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Paste Translated");
	ui.addLabel("","Center of geometry of pasted atoms:", 1, 1);
	ui.addDoubleSpin("newx", "New X", -1e6, 1e6, 1, 0.0 ,1,2);
	ui.addDoubleSpin("newy", "New Y", -1e6, 1e6, 1, 0.0 ,1,3);
	ui.addDoubleSpin("newz", "New Z", -1e6, 1e6, 1, 0.0 ,1,4);
	
	// Run the custom dialog
	if (dialog.defaultDialog().execute())
	{
		Vec3<double> r = ui.asVec3("newx", "newy", "newz");
		CommandNode::run(Commands::Paste, "ddd", r.x, r.y, r.z);

		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
	}
}

void AtenWindow::on_actionEditQuickCommand_triggered(bool checked)
{
	// Raise an edit box to get the user command
	bool ok;
	QString command;
	do
	{
		QString text = QInputDialog::getText(this, tr("Quick Command "), tr("Command:"), QLineEdit::Normal, command, &ok);
		if (ok && !text.isEmpty())
		{
			// Store command and attempt to 'compile' it
			command = text;
			Program program;;
			if (program.generateFromString(command, "Quick Command", "QuickCommand"))
			{
				ReturnValue rv;
				program.execute(rv);
				updateWidgets(AtenWindow::AllTarget);
				break;
			}
			else
			{
				QMessageBox::StandardButton but = QMessageBox::warning(this, "Quick Command", "Command could not be executed (error in syntax?). Re-edit command?", QMessageBox::Cancel | QMessageBox::Retry, QMessageBox::Cancel);
				if (but == QMessageBox::Retry) ok = false;
				else break;
			}
		}
		else break;
	} while (!ok);
}

/*
 * Model Actions
 */

// Rename model
void AtenWindow::on_actionModelRename_triggered(bool checked)
{
	Model* m = aten_.currentModelOrFrame();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		CommandNode::run(Commands::SetName, "c", qPrintable(text));
		updateWidgets(AtenWindow::ModelsListTarget);
	}
}

// Fold atoms in model
void AtenWindow::on_actionModelFoldAtoms_triggered(bool checked)
{
	CommandNode::run(Commands::Fold, "");
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Fold molecules in model
void AtenWindow::on_actionModelFoldMolecules_triggered(bool checked)
{
	CommandNode::run(Commands::FoldMolecules, "");
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Move to next model in list
void AtenWindow::on_actionModelNext_triggered(bool checked)
{
	// If multiple models are visible, step along to next visible model. Otherwise, just next in list
	if (aten_.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int>* ri;
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
void AtenWindow::on_actionModelPrevious_triggered(bool checked)
{
	// If multiple models are visible, step back to previous visible model. Otherwise, just previous in list
	if (aten_.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int>* ri;
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

// Show all atoms in current model
void AtenWindow::on_actionModelShowAll_triggered(bool checked)
{
	CommandNode::run(Commands::ShowAll, "");
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// List all measurements in model
void AtenWindow::on_actionListMeasurements_triggered(bool checked)
{
	aten_.currentModelOrFrame()->listMeasurements();
}

/*
 * Forcefield Actions
 */

// Open forcefield file
void AtenWindow::on_actionOpenForcefield_triggered(bool checked)
{
	// Call routine in forcefields window...
	forcefieldsWidget->loadForcefield();
}

// Open expression file
void AtenWindow::on_actionOpenExpression_triggered(bool checked)
{
	Tree* filter;
	static QDir currentDirectory_(aten_.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Expression", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ExpressionImport), &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		
		// Find the filter that was selected
		filter = aten_.findFilterByDescription(FilterData::ExpressionImport, qPrintable(selFilter));
		if (filter == NULL) filter = aten_.probeFile(qPrintable(filename), FilterData::ExpressionImport);
		if (filter != NULL)
		{
			if (!filter->executeRead(qPrintable(filename))) return;
		}
	}

	updateWidgets(AtenWindow::MainViewTarget);
}

// Save expression
void AtenWindow::on_actionSaveExpression_triggered(bool checked)
{
	Tree* filter;
	static QString selectedFilter(aten_.filters(FilterData::ExpressionExport)->item->filter.name());
	static QDir currentDirectory_(aten_.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Expression", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ExpressionExport));
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
	
		// Get file info
		QFileInfo fileInfo(filename);
	
		// Does this extension uniquely identify a specific filter?
		Reflist<Tree,int> filters;
		if (fileInfo.suffix().isEmpty())
		{
			// Does this filename uniquely identify a specific filter?
			for (Refitem<Tree,int>* ri = aten_.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesNameMatch(qPrintable(fileInfo.fileName()))) filters.add(ri->item);
			}
			Messenger::print(Messenger::Verbose, "Exact filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());

			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			AtenSelectFilter selectFilter(*this);
			if (filters.nItems() != 0) filter = selectFilter.selectFilter("Exact name matches one or more known expression export filters.", &filters, aten_.filterList(FilterData::ExpressionExport));
			else
			{
				filter = selectFilter.selectFilter("Couldn't determine format to save expression in.", NULL, aten_.filterList(FilterData::ExpressionExport), true);
				if ((filter != NULL) && selectFilter.appendExtension())
				{
					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
				}
			}
		}
		else
		{
			for (Refitem<Tree,int>* ri = aten_.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesExtensionMatch(fileInfo.suffix())) filters.add(ri->item);
			}
			Messenger::print(Messenger::Verbose, "Extension of filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());

			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() == 1) filter = filters.first()->item;
			else if (filters.nItems() > 1)
			{
				AtenSelectFilter selectFilter(*this);
				filter = selectFilter.selectFilter("Extension matches two or more known expression export filters.", &filters, aten_.filterList(FilterData::ExpressionExport));
			}
			else
			{
				AtenSelectFilter selectFilter(*this);
				filter = selectFilter.selectFilter("Extension doesn't match any in known expression export filters.", NULL, aten_.filterList(FilterData::ExpressionExport), true);
				if ((filter != NULL) && selectFilter.appendExtension())
				{
					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
				}
			}
		}
		Model* m = aten_.currentModelOrFrame();
		if (filter == NULL) Messenger::print("No filter selected to save file '%s'. Not saved.", qPrintable(filename));
		else
		{
			// Temporarily disable undo/redo for the model, save expression, and re-enable
			m->disableUndoRedo();
			if (filter->executeWrite(qPrintable(filename))) Messenger::print("Expression for model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(filename), qPrintable(filter->filter.name()));
			else Messenger::print("Failed to save expression for model '%s'.", qPrintable(m->name()));
			m->enableUndoRedo();
		}
	}
}

// Create patterns for model
void AtenWindow::on_actionModelCreatePatterns_triggered(bool checked)
{
	aten_.currentModelOrFrame()->createPatterns();
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Remove patterns from model
void AtenWindow::on_actionModelRemovePatterns_triggered(bool checked)
{
	aten_.currentModelOrFrame()->clearPatterns();
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// List patterns in model
void AtenWindow::on_actionModelListPatterns_triggered(bool checked)
{
	aten_.currentModelOrFrame()->printPatterns();
}

// Perform forcefield typing in model
void AtenWindow::on_actionModelFFType_triggered(bool checked)
{
	aten_.currentModelOrFrame()->typeAll(aten_.currentForcefield());
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Remove typing from model
void AtenWindow::on_actionModelFFUntype_triggered(bool checked)
{
	aten_.currentModelOrFrame()->removeTyping();
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Create energy expression for model
void AtenWindow::on_actionModelCreateExpression_triggered(bool checked)
{
	aten_.currentModelOrFrame()->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield(), aten_.combinationRules());
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

// Add default pattern
void AtenWindow::on_actionModelAddDefaultPattern_triggered(bool checked)
{
	aten_.currentModelOrFrame()->createDefaultPattern();
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

/*
 * Settings Actions
 */

// Show preferences window
void AtenWindow::on_actionPreferences_triggered(bool checked)
{
	AtenPrefs prefsDialog(*this);
	prefsDialog.setControls();
	prefsDialog.exec();
}

// Reload all filters
void AtenWindow::on_actionReloadFilters_triggered(bool checked)
{
	if (aten_.reloadFilters() > 0)
	{
		QMessageBox::warning(this, "Aten", "Errors encountered while reloading filters - see message box for details.", QMessageBox::Ok);
	}
}

// Toggle manualswapbuffers option
void AtenWindow::on_actionManualSwapBuffers_triggered(bool checked)
{
	prefs.setManualSwapBuffers(checked);
	updateWidgets(AtenWindow::MainViewTarget);
}

