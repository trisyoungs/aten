/*
	*** Qt file actions
	*** src/gui/fileactions.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/master.h"
#include "classes/grid.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/loadmodel.h"
#include <QtGui/QFileDialog>
#include "model/model.h"

/*
// File Actions
*/

void AtenForm::on_actionFileNew_triggered(bool checked)
{
	master.addModel();
}

void AtenForm::on_actionFileOpen_triggered(bool checked)
{
	Filter *f;
	if (gui.loadModelDialog->exec() == 1)
	{
		f = gui.loadModelDialog->selectedFilter();
		// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (f == NULL) f = master.probeFile(gui.loadModelDialog->selectedFilename(), FT_MODEL_IMPORT);
		if (f != NULL)
		{
			f->execute(gui.loadModelDialog->selectedFilename());
			addRecent(gui.loadModelDialog->selectedFilename());
		}
		refreshModelTabs();
		master.currentModel()->logChange(Change::VisualLog);
		gui.modelChanged();
	}
}

bool AtenForm::runSaveModelDialog()
{
	saveModelFilter = NULL;
	saveModelFilename.clear();
	Filter *f;
	int result = dialog[FT_MODEL_EXPORT]->exec();
	//printf("Save model dialog result = %i\n",result);
	if (result == 1)
	{
		// Get selected filename (only grab first
		//QString filename = savemodeldialog->selectedFiles().first();
		saveModelFilename = qPrintable(dialog[FT_MODEL_EXPORT]->selectedFiles().first());
		// Get selected filter
		QString filter = dialog[FT_MODEL_EXPORT]->selectedFilter();
		// Find the filter that was selected
		for (f = master.filters(FT_MODEL_EXPORT); f != NULL; f = f->next)
			if (strcmp(f->description(),qPrintable(filter)) == 0) break;
		if (f == NULL) printf("AtenForm::run_savemodel_dialog <<<< Didn't recognise selected file filter '%s' >>>>\n", qPrintable(filter));
		saveModelFilter = f;
		return (saveModelFilter == NULL ? FALSE : TRUE);
	}
	else return FALSE;
}

void AtenForm::on_actionFileSaveAs_triggered(bool checked)
{
	Model *m;
	if (runSaveModelDialog())
	{
		m = master.currentModel();
		m->setFilter(saveModelFilter);
		m->setFilename(saveModelFilename.get());
		saveModelFilter->execute(saveModelFilename.get());
		//refreshModelTabs();
	}
}

void AtenForm::on_actionFileSave_triggered(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	Model *m = master.currentModel();
	Filter *f = m->filter();
	if ((f != NULL) && (f->type() != FT_MODEL_EXPORT)) f = NULL;
	Dnchar filename;
	filename = m->filename();
	if (filename.empty() || (f == NULL))
	{
		if (runSaveModelDialog())
		{
			m = master.currentModel();
			m->setFilter(saveModelFilter);
			m->setFilename(saveModelFilename.get());
			saveModelFilter->execute(saveModelFilename.get());
			//refreshModelTabs();
		}
	}
	else f->execute(saveModelFilename.get());
	gui.modelChanged(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionFileClose_triggered(bool checked)
{
	// If the current model has been modified, ask for confirmation before we close it
	char text[512];
	Filter *f;
	Model *m = master.currentModel();
	if (m->isModified())
	{
		// Create a model message dialog
		sprintf(text, "Model '%s' has been modified.\n", m->name());
		int returnvalue = QMessageBox::warning(this, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				master.removeModel(m);
				break;
			// Cancel quit and return to app
			case (QMessageBox::Cancel):
				return;
			// Save model before quit
			case (QMessageBox::Save):
				// If model has a filter set, just save it
				f = m->filter();
				if (f != NULL) f->execute(m->filename());
				else if (runSaveModelDialog())
				{
					m->setFilter(saveModelFilter);
					m->setFilename(saveModelFilename.get());
					saveModelFilter->execute(saveModelFilename.get());
				}
				else return;
				master.removeModel(m);
				break;
		}
	}
	else master.removeModel(m);
}

void AtenForm::on_actionFileSaveImage_triggered(bool checked)
{
	// Save the current view as a bitmap image.
	// Get filename from user
	int n;
	if (saveBitmapDialog->exec() == 1)
	{
		// Flag any surfaces to be rerendered for use in this context
		for (Grid *g = master.grids(); g != NULL; g = g->next) g->requestRerender();
		// Create a QPixmap of the current scene
		QPixmap pixmap = ui.ModelView->renderPixmap(0,0,FALSE);
		// Flag any surfaces to be rerendered so they are redisplayed in the original context
		for (Grid *g = master.grids(); g != NULL; g = g->next) g->requestRerender();

		// Get selected filename
		QStringList filenames = saveBitmapDialog->selectedFiles();
		QString filename = filenames.first();
		// Get selected filter
		QString filter = saveBitmapDialog->selectedFilter();
		// Find the filter that was selected
		for (n=0; n<BIF_NITEMS; n++)
			if (strcmp(filter_from_BIF( (bitmap_format) n), qPrintable(filter)) == 0) break;
		if (n != BIF_NITEMS)
		{
			pixmap.save(filename, extension_from_BIF( (bitmap_format) n), -1);
			msg(Debug::None,"Saved current view as '%s'\n", qPrintable(filename));
		}
	}
}

void AtenForm::on_actionFileAddTrajectory_triggered(bool checked)
{
	Filter *f;
	Model *m = master.currentModel();
	if (dialog[FT_TRAJECTORY_IMPORT]->exec() == 1)
	{
		// Get selected filename
		QStringList filenames = dialog[FT_TRAJECTORY_IMPORT]->selectedFiles();
		QString filename = filenames.first();
		// Get selected filter
		QString filter = dialog[FT_TRAJECTORY_IMPORT]->selectedFilter();
		// Find the filter that was selected
		for (f = master.filters(FT_TRAJECTORY_IMPORT); f != NULL; f = f->next)
			if (strcmp(f->description(),qPrintable(filter)) == 0) break;
		// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (f == NULL) f = master.probeFile(qPrintable(filename), FT_TRAJECTORY_IMPORT);
		if (f != NULL)
		{
			m->initialiseTrajectory(qPrintable(filename), f);
			// Ensure trajectory toolbar is visible and View->Trajectory is selected
			ui.TrajectoryToolBar->setVisible(TRUE);
			ui.actionViewTrajectory->setChecked(TRUE);
			gui.updateTrajControls();
		}
		else msg(Debug::None, "Couldn't determine trajectory file format.\n");
		gui.modelChanged();
	}
}

void AtenForm::on_actionFileOpenForcefield_triggered(bool checked)
{
	QString filename;
	if (openForcefieldDialog->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = openForcefieldDialog->selectedFilter();
		filename = openForcefieldDialog->selectedFiles().first();
		master.loadForcefield(qPrintable(filename));
		refreshForcefieldPage();
	}
}

// void AtenForm::on_actionFileSaveForcefield_triggered(bool checked)
// {
// 	printf("Not yet done...\n");
// }

void AtenForm::on_actionFileOpenGrid_triggered(bool checked)
{
	Filter *f;
	Grid *g;
	QString filename;
	QStringList filenames;
	if (dialog[FT_GRID_IMPORT]->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = dialog[FT_GRID_IMPORT]->selectedFilter();
		// Find the corresponding Aten filter that was selected
		for (f = master.filters(FT_GRID_IMPORT); f != NULL; f = f->next)
			if (strcmp(f->description(),qPrintable(filter)) == 0) break;
		// Get selected filename list
		filenames = dialog[FT_GRID_IMPORT]->selectedFiles();
		// Loop over selected files
		for (int i = 0; i < filenames.count(); ++i)
		{
			filename = filenames.at(i);
			// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
			if (f != NULL) f->execute(qPrintable(filename));
			else
			{
				f = master.probeFile(qPrintable(filename), FT_GRID_IMPORT);
				if (f != NULL) f->execute(qPrintable(filename));
			}
		}
		refreshGridsPage();
		gui.mainView.postRedisplay();
	}
}

// Save expression
void AtenForm::on_actionFileSaveExpression_triggered(bool checked)
{
	Filter *f;
	if (dialog[FT_EXPRESSION_EXPORT]->exec() == 1)
	{
		// Get selected filename (only grab first
		QString filename = dialog[FT_EXPRESSION_EXPORT]->selectedFiles().first();
		// Get selected filter
		QString filter = dialog[FT_EXPRESSION_EXPORT]->selectedFilter();
		// Find the filter that was selected
		for (f = master.filters(FT_EXPRESSION_EXPORT); f != NULL; f = f->next)
			if (strcmp(f->description(),qPrintable(filter)) == 0) break;
		if (f == NULL) printf("AtenForm::actionFileSaveExpression dialog <<<< Didn't recognise selected file filter '%s' >>>>\n", qPrintable(filter));
		else f->execute(qPrintable(filename));
	}
}

void AtenForm::on_actionFileQuit_triggered(bool checked)
{
	if (!gui.saveBeforeClose()) return;
	saveSettings();
	gui.app->exit(0);
}
