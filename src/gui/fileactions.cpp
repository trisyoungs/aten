/*
	*** File Actions
	*** src/gui/fileactions.cpp
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
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/customdialog.h"
#include "gui/forcefields.h"
#include "gui/grids.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

// Add new model to workspace
void AtenForm::on_actionFileNew_triggered(bool checked)
{
	Model *m = aten.addModel();
	m->enableUndoRedo();
	m->regenerateIcon();
	// Update GUI
	gui.update(GuiQt::AllTarget);
}

// Open existing file
void AtenForm::on_actionFileOpen_triggered(bool checked)
{
	Tree *filter;
	if (gui.loadModelDialog->exec() == 1)
	{
		filter = gui.loadModelDialog->selectedFormat();
		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten.probeFile(gui.loadModelDialog->selectedFilename(), FilterData::ModelImport);
		if (filter != NULL)
		{
			// Run any import options in the filter
			if (!filter->executeCustomDialog()) return;
			filter->executeRead(gui.loadModelDialog->selectedFilename());
			addRecent(gui.loadModelDialog->selectedFilename());
			aten.currentModelOrFrame()->changeLog.add(Log::Visual);
			gui.update(GuiQt::AllTarget);
		}
	}
}

// Local save function
bool AtenForm::runSaveModelDialog()
{
	saveModelFilter = NULL;
	saveModelFilename.clear();
	Tree *filter = NULL;
	static QString selectedFilter(aten.filters(FilterData::ModelExport)->item->filter.name());
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Model", currentDirectory_.path(), saveModelFilters, &selectedFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Grab file extension and search for it in our current lists...
		Dnchar ext = afterLastChar(qPrintable(filename), '.');
		Reflist<Tree,int> filters;
		if (ext.isEmpty())
		{
			QFileInfo fileInfo( filename );
			// Does this filename uniquely identify a specific filter?
			for (Refitem<Tree,int> *ri = aten.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesNameMatch(qPrintable(fileInfo.fileName()))) filters.add(ri->item);
			}
			msg.print(Messenger::Verbose, "Exact filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() != 0) filter = gui.selectFilterDialog->selectFilter("Name matches one or more model export filters.", &filters, aten.filterList(FilterData::ModelExport));
			else
			{
				filter = gui.selectFilterDialog->selectFilter("Couldn't determine format to save expression in.", NULL, aten.filterList(FilterData::ModelExport), TRUE);
				if ((filter != NULL) && gui.selectFilterDialog->appendExtension())
				{
					if (filter->filter.extensions() != NULL) filename += QString(".") + filter->filter.extensions()->get();
				}
			}
		}
		else
		{
			// Does this extension uniquely identify a specific filter?
			for (Refitem<Tree,int> *ri = aten.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
			{
				if (ri->item->filter.doesExtensionMatch(ext.get())) filters.add(ri->item);
			}
			msg.print(Messenger::Verbose, "Extension of filename '%s' matches %i filters...", qPrintable(filename), filters.nItems());
			// If only one filter matched the filename extension, use it. Otherwise, ask for confirmation *or* list all filters.
			if (filters.nItems() == 1) filter = filters.first()->item;
			else if (filters.nItems() > 1) filter = gui.selectFilterDialog->selectFilter("Extension matches one or more model export filters.", &filters, aten.filterList(FilterData::ModelExport));
			else
			{
				filter = gui.selectFilterDialog->selectFilter("Extension doesn't match any in known model export filters.", NULL, aten.filterList(FilterData::ModelExport), TRUE);
				if ((filter != NULL) && gui.selectFilterDialog->appendExtension())
				{
					if (filter->filter.extensions() != NULL) filename += QString(".") + filter->filter.extensions()->get();
				}
			}
		}
		saveModelFilter = filter;
		saveModelFilename = qPrintable(filename);
		if (filter == NULL) msg.print("No filter selected to save file '%s'. Not saved.\n", saveModelFilename.get());
		return (saveModelFilter == NULL ? FALSE : TRUE);
	}
	else return FALSE;
}

// Save current model under a different name
void AtenForm::on_actionFileSaveAs_triggered(bool checked)
{
	Model *m;
	if (runSaveModelDialog())
	{
		// Run options dialog
		if (!saveModelFilter->executeCustomDialog())
		{
			msg.print("Not saved.\n");
			return;
		}
		m = aten.currentModelOrFrame();
		m->setFilter(saveModelFilter);
		m->setFilename(saveModelFilename.get());
		if (saveModelFilter->executeWrite(saveModelFilename.get()))
		{
			m->changeLog.updateSavePoint();
			msg.print("Model '%s' saved to file '%s' (%s)\n", m->name(), saveModelFilename.get(), saveModelFilter->filter.name());
		}
		else msg.print("Failed to save model '%s'.\n", m->name());
		gui.update();
	}
}

// Save current model
void AtenForm::on_actionFileSave_triggered(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	Model *m = aten.currentModelOrFrame();
	Tree *t = m->filter();
	if ((t != NULL) && (t->filter.type() != FilterData::ModelExport)) t = NULL;
	Dnchar filename;
	filename = m->filename();
	if (filename.isEmpty() || (t == NULL))
	{
		if (runSaveModelDialog())
		{
			// Run options dialog
			if (!saveModelFilter->executeCustomDialog())
			{
				msg.print("Not saved.\n");
				return;
			}
			m->setFilter(saveModelFilter);
			m->setFilename(saveModelFilename.get());
			if (saveModelFilter->executeWrite(saveModelFilename.get()))
			{
				m->changeLog.updateSavePoint();
				msg.print("Model '%s' saved to file '%s' (%s)\n", m->name(), saveModelFilename.get(), saveModelFilter->filter.name());
			}
			else msg.print("Failed to save model '%s'.\n", m->name());
			//refreshModelTabs();
		}
	}
	else
	{
		if (!t->executeCustomDialog(TRUE))
		{
			msg.print("Not saved.\n");
			return;
		}
		t->executeWrite(filename.get());
		m->changeLog.updateSavePoint();
	}	
	gui.update();
}

// Modify export options for current model's associated filter
void AtenForm::on_actionExportOptions_triggered(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	if (m->filter() == NULL) msg.print("No filter currently assigned to model '%s', so there are no export options.\n", m->name());
	else m->filter()->executeCustomDialog();
}

// Close current model
void AtenForm::on_actionFileClose_triggered(bool checked)
{
	Model *m = aten.currentModel();
	aten.closeModel(m);
	gui.update(GuiQt::AllTarget);
}

// Save the current view as a bitmap image.
void AtenForm::on_actionFileSaveImage_triggered(bool checked)
{
	// Get geometry from user - initial setup is to use current canvas geometry
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainWidget->width(), (int) gui.mainWidget->height());
	int width, height;
	static bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView();
	bool currentframemodel, currentframeview, viewglobe;
	
	static Tree dialog("Save Image Options","option('Size', 'edit', '10x10'); option('choices', 'radiogroup'); option('No frames', 'radio', 'choices', 1, 'newline', 'span=2'); option('Frame current model', 'radio', 'choices', 0, 'newline', 'span=2'); option('Frame whole view', 'radio', 'choices', 0, 'newline', 'span=2'); option('Frame current model and whole view', 'radio', 'choices', 0, 'newline', 'span=2'); ");

	// Poke values into dialog widgets and execute
	dialog.setWidgetValue("Size", ReturnValue(geometry.get()));
	dialog.setWidgetValue("choices", ReturnValue(framemodel ? (frameview ? 4 : 2) : (frameview ? 3 : 1) ));
	if (!dialog.executeCustomDialog(FALSE)) return;

	// Get values from dialog
	geometry = dialog.widgetValuec("Size");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int choice = dialog.widgetValuei("choices");
	framemodel = choice%2 == 0;
	frameview = choice > 2;
	currentframemodel = prefs.frameCurrentModel();
	currentframeview = prefs.frameWholeView();
	viewglobe = prefs.viewRotationGlobe();
	
	// Get filename from user
	GuiQt::BitmapFormat bf;
	static QString selectedFilter("Windows Bitmap (*.bmp)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Bitmap", currentDirectory_.path(), saveBitmapFilters, &selectedFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Grab filename extension and search for it
		Dnchar ext = afterLastChar(qPrintable(filename), '.');
		bf = GuiQt::bitmapFormat(ext.get());
		// If we didn't recognise the extension, complain and quit
		if (bf == GuiQt::nBitmapFormats) 
		{
			Dnchar message(-1, "Bitmap format not recognised - '%s'.\n", ext.get());
			QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		}
		else
		{
			prefs.setFrameCurrentModel(framemodel);
			prefs.setFrameWholeView(frameview);
			prefs.setViewRotationGlobe(FALSE);
			if (!gui.saveImage(qPrintable(filename), bf, width, height, -1)) msg.print("Failed to save image.\n");
			prefs.setFrameCurrentModel(currentframemodel);
			prefs.setFrameWholeView(currentframeview);
			prefs.setViewRotationGlobe(viewglobe);
		}
	}
}

// Open grid file
void AtenForm::on_actionFileOpenGrid_triggered(bool checked)
{
	// Call routine in grids window...
	gui.gridsWidget->loadGrid();
}

// Quit program
void AtenForm::on_actionFileQuit_triggered(bool checked)
{
	if (!gui.saveBeforeClose()) return;
	saveSettings();
	gui.app->exit(0);
}

