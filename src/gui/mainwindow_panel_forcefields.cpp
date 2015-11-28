/*
	*** Main Window - Forcefields Panel Functions
	*** src/gui/mainwindow_panel_forcefields.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"
#include "ff/forcefield.h"
#include "gui/selectfilter.h"
#include "gui/ffeditor.h"
#include <QFileDialog>

// Update forcefields panel
void AtenWindow::updateForcefieldsPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateForcefieldsPanel");

	Forcefield* currentForcefield = aten_.currentForcefield();

	// Forcefields list
	ui.ForcefieldsList->clear();
	int n = 0;
	for (Forcefield* ff = aten_.forcefields(); ff != NULL; ff = ff->next)
	{
		ui.ForcefieldsList->addItem(ff->name());
		if (ff == aten_.currentForcefield()) ui.ForcefieldsList->setCurrentRow(n);
		++n;
	}
	ui.ForcefieldsManageCloseButton->setEnabled(currentForcefield);
	ui.ForcefieldsManageEditButton->setEnabled(currentForcefield);
	ui.ForcefieldsManageAssignButton->setEnabled(currentForcefield);
// 	ui.AutomaticTypingGroup->setEnabled(ffselected);  // ATEN2 TODO
// 	ui.ManualTypingGroup->setEnabled(ffselected);

	// Calculate
	ui.ForcefieldsCalculateMinimiseButton->setEnabled(sourceModel);
	ui.ForcefieldsCalculateEnergyButton->setEnabled(sourceModel);
	ui.ForcefieldsCalculateForcesButton->setEnabled(sourceModel);

	Messenger::exit("AtenWindow::updateForcefieldsPanel");
}

/*
 * Manage
 */

void AtenWindow::on_ForcefieldsList_currentRowChanged(int row)
{
	if (refreshing_) return;

	// Set the new default forcefield in the master and refresh the forcefields page
	aten_.setCurrentForcefield(row);
}

void AtenWindow::on_ForcefieldsManageLoadButton_clicked(bool checked)
{
	static QDir currentDirectory_(aten_.workDir());
	QString filename = QFileDialog::getOpenFileName(this, "Open Forcefield", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::ExpressionImport));
	if (!filename.isEmpty())
	{
		Tree* filter = aten_.probeFile(qPrintable(filename), FilterData::ExpressionImport);
		if (filter)
		{
			if (!filter->executeRead(qPrintable(filename))) return;
		}

		updateWidgets(AtenWindow::ForcefieldsPanelTarget);

		// Store path for next use
		currentDirectory_.setPath(filename);
	}

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ForcefieldsManageCloseButton_clicked(bool checked)
{
	aten_.removeForcefield(aten_.currentForcefield());

	updateWidgets(AtenWindow::ForcefieldsPanelTarget);
}

void AtenWindow::on_ForcefieldsManageEditButton_clicked(bool checked)
{
	AtenForcefieldEditor ffEditor(this);
	ffEditor.populate(aten_.currentForcefield());
	ffEditor.show();
}

void AtenWindow::on_ForcefieldsManageAssignButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->setForcefield(aten_.currentForcefield());
}

/*
 * Calculate
 */

void AtenWindow::on_ForcefieldsCalculateMinimiseButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.ForcefieldsCalculateMinimiseButton->callPopupMethod("minimise", rv);
}

void AtenWindow::on_ForcefieldsCalculateEnergyButton_clicked(bool checked)
{
	ReturnValue result;
	if (aten_.current().rs() == aten_.currentModel()) result = CommandNode::run(Commands::ModelEnergy, "");
	else result = CommandNode::run(Commands::FrameEnergy, "");

	// Print energy
	aten_.currentModel()->renderSourceModel()->energy.print();

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ForcefieldsCalculateForcesButton_clicked(bool checked)
{
	ReturnValue result;
	if (aten_.current().rs() == aten_.currentModel()) result = CommandNode::run(Commands::ModelForces, "");
	else result = CommandNode::run(Commands::FrameForces, "");

	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Patterns
 */

void AtenWindow::on_ForcefieldsPatternsCreateButton_clicked(bool checked)
{
	CommandNode::run(Commands::CreatePatterns, "");
	CommandNode::run(Commands::ListPatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsClearButton_clicked(bool checked)
{
	CommandNode::run(Commands::ClearPatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsListButton_clicked(bool checked)
{
	CommandNode::run(Commands::ListPatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsDefaultButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->createDefaultPattern(); // ATEN2 TODO Make this into a command?

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

/*
 * Expression
 */

void AtenWindow::on_ForcefieldsExpressionTypeButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->typeAll(aten_.currentForcefield());

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ForcefieldsExpressionDescribeButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->createExpression(Choice::Default, Choice::Default, ui.ForcefieldsExpressionChargesButton->isChecked() ? Choice::Yes : Choice::No, aten_.currentForcefield(), aten_.combinationRules());
}

void AtenWindow::on_ForcefieldsExpressionExportButton_clicked(bool checked)
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
		RefList<Tree,int> filters;
		if (fileInfo.suffix().isEmpty())
		{
			// Does this filename uniquely identify a specific filter?
			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
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
			for (RefListItem<Tree,int>* ri = aten_.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
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

void AtenWindow::on_ForcefieldsExpressionClearButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	
	currentModel->removeTyping();

	updateWidgets(AtenWindow::MainViewTarget);
}
