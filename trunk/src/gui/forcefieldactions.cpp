/*
	*** Forcefield Menu Actions
	*** src/gui/forcefieldactions.cpp
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
#include "gui/mainwindow.h"
#include "gui/forcefields.h"
#include "gui/selectfilter.h"
#include "model/model.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Open forcefield file
void AtenForm::on_actionOpenForcefield_triggered(bool checked)
{
	// Call routine in forcefields window...
	gui.forcefieldsWidget->loadForcefield();
}

// Open expression file
void AtenForm::on_actionOpenExpression_triggered(bool checked)
{
	Tree *filter;
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Expression", currentDirectory_.path(), gui.mainWindow()->loadExpressionFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		filter = aten.findFilterByDescription(FilterData::ExpressionImport, qPrintable(selFilter));
		if (filter == NULL) filter = aten.probeFile(qPrintable(filename), FilterData::ExpressionImport);
		if (filter != NULL)
		{
			// Run any import options in the filter
			if (!filter->executeCustomDialog()) return;
			if (filter != NULL) filter->executeRead(qPrintable(filename));
		}
	}
	gui.mainWidget()->postRedisplay();
}

// Save expression
void AtenForm::on_actionSaveExpression_triggered(bool checked)
{
	Tree *filter;
	static QString selectedFilter(aten.filters(FilterData::ExpressionExport)->item->filter.name());
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Expression", currentDirectory_.path(), saveExpressionFilters);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Grab file extension and search for it in our current lists...
		Dnchar ext = afterLastChar(qPrintable(filename), '.');
		// Does this extension uniquely identify a specific filter?
		Reflist<Tree,int> filters;
		if (ext.isEmpty())
		{
			QFileInfo fileInfo( filename );
			// Does this filename uniquely identify a specific filter?
			for (Refitem<Tree,int> *ri = aten.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesNameMatch(qPrintable(fileInfo.fileName()))) filters.add(ri->item);
			}
			msg.print(Messenger::Verbose, "Exact filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() != 0) filter = gui.selectFilterDialog->selectFilter("Exact name matches one or more known expression export filters.", &filters, aten.filterList(FilterData::ExpressionExport));
			else
			{
				filter = gui.selectFilterDialog->selectFilter("Couldn't determine format to save expression in.", NULL, aten.filterList(FilterData::ExpressionExport), TRUE);
				if ((filter != NULL) && gui.selectFilterDialog->appendExtension())
				{
					if (filter->filter.extensions() != NULL) filename += QString(".") + filter->filter.extensions()->get();
				}
			}
		}
		else
		{
			for (Refitem<Tree,int> *ri = aten.filters(FilterData::ExpressionExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesExtensionMatch(ext.get())) filters.add(ri->item);
			}
			msg.print(Messenger::Verbose, "Extension of filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() == 1) filter = filters.first()->item;
			else if (filters.nItems() > 1) filter = gui.selectFilterDialog->selectFilter("Extension matches two or more known expression export filters.", &filters, aten.filterList(FilterData::ExpressionExport));
			else
			{
				filter = gui.selectFilterDialog->selectFilter("Extension doesn't match any in known expression export filters.", NULL, aten.filterList(FilterData::ExpressionExport), TRUE);
				if ((filter != NULL) && gui.selectFilterDialog->appendExtension())
				{
					if (filter->filter.extensions() != NULL) filename += QString(".") + filter->filter.extensions()->get();
				}
			}
		}
		Model *m = aten.currentModelOrFrame();
		if (filter == NULL) msg.print("No filter selected to save file '%s'. Not saved.\n", qPrintable(filename));
		else
		{
			// Run any export options in the filter
			if (!filter->executeCustomDialog()) return;
			if (filter->executeWrite(qPrintable(filename))) msg.print("Expression for model '%s' saved to file '%s' (%s)\n", m->name(), qPrintable(filename), filter->filter.name());
			else msg.print("Failed to save expression for model '%s'.\n", m->name());
		}
	}
}

// Create patterns for model
void AtenForm::on_actionModelCreatePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->autocreatePatterns();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Remove patterns from model
void AtenForm::on_actionModelRemovePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->clearPatterns();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// List patterns in model
void AtenForm::on_actionModelListPatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->printPatterns();
}

// Perform forcefield typing in model
void AtenForm::on_actionModelFFType_triggered(bool checked)
{
	aten.currentModelOrFrame()->typeAll();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// // Remove typing from model
void AtenForm::on_actionModelFFUntype_triggered(bool checked)
{
	aten.currentModelOrFrame()->removeTyping();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

// Create energy expression for model
void AtenForm::on_actionModelCreateExpression_triggered(bool checked)
{
	aten.currentModelOrFrame()->createExpression();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

