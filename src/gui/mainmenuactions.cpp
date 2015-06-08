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
#include "gui/forcefields.h"
#include "base/sysfunc.h"

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
