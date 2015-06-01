/*
	*** Main Window - Home Panel Functions
	*** src/gui/mainwindow_panel_home.cpp
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
#include "gui/selectfilter.h"
#include "gui/loadmodel.h"
#include "gui/saveimage.h"
#include "model/undostate.h"
#include <QFileDialog>

// Update home panel
void AtenWindow::updateHomePanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateHomePanel");

	ui.HomeFileOpenButton->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.HomeFileSaveButton->setEnabled((!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty()) && sourceModel && sourceModel->isModified());
	ui.HomeFileSaveAsButton->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());

	// Update undo / redo state
	if (sourceModel && sourceModel->currentUndoState())
	{
		ui.HomeEditUndoButton->setToolTip("Undo (" + sourceModel->currentUndoState()->description() + ")");
		ui.HomeEditUndoButton->setEnabled(true);
	}
	else
	{
		ui.HomeEditUndoButton->setToolTip("(Nothing to Undo)");
		ui.HomeEditUndoButton->setEnabled(false);
	}
	if (sourceModel && sourceModel->currentRedoState())
	{
		ui.HomeEditRedoButton->setToolTip("Redo (" + sourceModel->currentRedoState()->description() + ")");
		ui.HomeEditRedoButton->setEnabled(true);
	}
	else
	{
		ui.HomeEditRedoButton->setToolTip("(Nothing to Redo)");
		ui.HomeEditRedoButton->setEnabled(false);
	}

	Messenger::exit("AtenWindow::updateHomePanel");
}

/*
 * File
 */

void AtenWindow::on_HomeFileNewButton_clicked(bool checked)
{
	Model* m = aten_.addModel();
	m->enableUndoRedo();

	// Update GUI
	aten_.setCurrentModel(m);

	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_HomeFileOpenButton_clicked(bool checked)
{
	AtenLoadModel loadModelDialog(*this);
	Tree* filter;
	if (loadModelDialog.exec() == 1)
	{
		filter = loadModelDialog.selectedFormat();
		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten_.probeFile(loadModelDialog.selectedFilename(), FilterData::ModelImport);
		if (filter != NULL)
		{
			if (!filter->executeRead(loadModelDialog.selectedFilename())) return;
			addRecent(loadModelDialog.selectedFilename());
			updateWidgets(AtenWindow::AllTarget);
		}
	}
}

void AtenWindow::on_HomeFileSaveButton_clicked(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	Model* m = aten_.currentModelOrFrame();
	Tree* t = m->filter();
	if ((t != NULL) && (t->filter.type() != FilterData::ModelExport)) t = NULL;
	QString filename;
	filename = m->filename();
	if (filename.isEmpty() || (t == NULL))
	{
		if (runSaveModelDialog())
		{
			m->setFilter(saveModelFilter_);
			m->setFilename(saveModelFilename_);
			// Temporarily disable undo/redo for the model, save, and re-enable
			m->disableUndoRedo();
			if (saveModelFilter_->executeWrite(saveModelFilename_))
			{
				m->updateSavePoint();
				Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(saveModelFilename_), qPrintable(saveModelFilter_->filter.name()));
			}
			else Messenger::print("Failed to save model '%s'.", qPrintable(m->name()));
			m->enableUndoRedo();
		}
	}
	else
	{
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		t->executeWrite(filename);
		m->updateSavePoint();
		m->enableUndoRedo();
	}
	updateWidgets();
}

void AtenWindow::on_HomeFileSaveAsButton_clicked(bool checked)
{
	Model* m;
	if (runSaveModelDialog())
	{
		m = aten_.currentModelOrFrame();
		if (m == NULL)
		{
			printf("Internal Error: Model pointer is NULL in AtenWindow::on_actionFileSaveAs_triggered.\n");
			return;
		}
		m->setFilter(saveModelFilter_);
		m->setFilename(saveModelFilename_);
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		
		if (saveModelFilter_->executeWrite(saveModelFilename_))
		{
			m->updateSavePoint();
			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(saveModelFilename_), qPrintable(saveModelFilter_->filter.name()));
		}
		else Messenger::print("Failed to save model '%s'.", qPrintable(m->name()));
		m->enableUndoRedo();
		updateWidgets();
	}
}

void AtenWindow::on_HomeFileCloseButton_clicked(bool checked)
{
	closeModel(aten_.currentModel());
	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_HomeFileImageButton_clicked(bool checked)
{
	static SaveImageDialog saveImageDialog(this);
	if (!saveImageDialog.getImageDetails(ui.MainView->width(), ui.MainView->height())) return;

	// Get values from dialog
	int imageWidth = saveImageDialog.ui.ImageWidthSpin->value();
	int imageHeight = saveImageDialog.ui.ImageHeightSpin->value();
	AtenWindow::BitmapFormat bf = AtenWindow::bitmapFormatFromFilter(qPrintable(saveImageDialog.ui.ImageFormatCombo->currentText()));
	QString fileName = saveImageDialog.ui.FileNameEdit->text();
	if (fileName.isEmpty()) return;

	QPixmap pixmap = ui.MainView->generateImage(imageWidth, imageHeight);
	pixmap.save(fileName, AtenWindow::bitmapFormatExtension(bf), -1);
}

/*
 * Edit
 */

void AtenWindow::on_HomeEditCopyButton_clicked(bool checked)
{
	CommandNode::run(Commands::Copy, "");

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeEditCutButton_clicked(bool checked)
{
	CommandNode::run(Commands::Cut, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditPasteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Paste, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditDeleteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Delete, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditUndoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Undo, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_HomeEditRedoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Redo, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}

// Local save function
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
		Reflist<Tree,int> filters;
		if (ext.isEmpty())
		{
			QFileInfo fileInfo( filename );
			// Does this filename uniquely identify a specific filter?
			for (Refitem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
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
			for (Refitem<Tree,int>* ri = aten_.filters(FilterData::ModelExport); ri != NULL; ri = ri->next)
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
