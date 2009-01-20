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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/loadmodel.h"
#include "gui/forcefields.h"
#include "gui/grids.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "base/sysfunc.h"
#include "command/staticcommand.h"

// Add new model to workspace
void AtenForm::on_actionFileNew_triggered(bool checked)
{
	Model *m = aten.addModel();
	m->enableUndoRedo();
}

// Open existing file
void AtenForm::on_actionFileOpen_triggered(bool checked)
{
	Filter *f;
	if (gui.loadModelDialog->exec() == 1)
	{
		f = gui.loadModelDialog->selectedFilter();
		// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (f == NULL) f = aten.probeFile(gui.loadModelDialog->selectedFilename(), Filter::ModelImport);
		if (f != NULL)
		{
			f->execute(gui.loadModelDialog->selectedFilename());
			addRecent(gui.loadModelDialog->selectedFilename());
			refreshModelTabs();
			aten.currentModel()->changeLog.add(Log::Visual);
			gui.modelChanged();
		}
	}
}

// Local save function
bool AtenForm::runSaveModelDialog()
{
	saveModelFilter = NULL;
	saveModelFilename.clear();
	Filter *f;
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getSaveFileName(this, "Save Model", currentDirectory_.path(),saveModelFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		for (f = aten.filters(Filter::ModelExport); f != NULL; f = f->next) if (selFilter == f->description()) break;
		if (f == NULL) printf("CRITICAL: runSaveModelDialog <<<< Didn't recognise selected file filter '%s' >>>>\n", qPrintable(selFilter));
		saveModelFilter = f;
		saveModelFilename = qPrintable(filename);
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
		m = aten.currentModel();
		m->setFilter(saveModelFilter);
		m->setFilename(saveModelFilename.get());
		saveModelFilter->execute(saveModelFilename.get());
		m->changeLog.updateSavePoint();
		gui.modelChanged(FALSE,FALSE,FALSE);
	}
}

// Save current model
void AtenForm::on_actionFileSave_triggered(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	Model *m = aten.currentModel();
	Filter *f = m->filter();
	if ((f != NULL) && (f->type() != Filter::ModelExport)) f = NULL;
	Dnchar filename;
	filename = m->filename();
	if (filename.isEmpty() || (f == NULL))
	{
		if (runSaveModelDialog())
		{
			m = aten.currentModel();
			m->setFilter(saveModelFilter);
			m->setFilename(saveModelFilename.get());
			// Model Export filters take the filename from the model itself, so pass dummy filename
			saveModelFilter->execute("DUMMY");
			//refreshModelTabs();
		}
	}
	else f->execute("DUMMY");
	gui.modelChanged(FALSE,FALSE,FALSE);
}

// Close current model
void AtenForm::on_actionFileClose_triggered(bool checked)
{
	// If the current model has been modified, ask for confirmation before we close it
	char text[512];
	Filter *f;
	Model *m = aten.currentModel();
	if (m->changeLog.isModified())
	{
		// Create a model message dialog
		sprintf(text, "Model '%s' has been modified.\n", m->name());
		int returnvalue = QMessageBox::warning(this, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				aten.removeModel(m);
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
				aten.removeModel(m);
				break;
		}
	}
	else aten.removeModel(m);
}

// Save the current view as a bitmap image.
void AtenForm::on_actionFileSaveImage_triggered(bool checked)
{
	// Get geometry from user - initial setup is to use current canvas geometry
	char geometry[128];
	int width, height;
	bool ok;
	sprintf(geometry,"%ix%i\n", (int) gui.mainView.width(), (int) gui.mainView.height());
	QString text = QInputDialog::getText(this, tr("Image Size"), tr("Size of bitmap image (width x height) in pixels:"), QLineEdit::Normal, geometry, &ok);
	if (ok && !text.isEmpty())
	{
		strcpy(geometry,qPrintable(text));
		width = atoi(beforeChar(geometry,'x'));
		height = atoi(afterChar(geometry,'x'));
		if ((width < 1) || (height < 1))
		{
			char text[512];
			sprintf(text, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry);
			QMessageBox::warning(this, "Aten", text, QMessageBox::Ok);
			return;
		}
	}
	else return;
	// Get filename from user
	GuiQt::BitmapFormat bf;
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getSaveFileName(this, "Save Bitmap", currentDirectory_.path(), saveBitmapFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		bf = GuiQt::bitmapFormatFromFilter(qPrintable(selFilter));
		// Save the image
		gui.saveImage(qPrintable(filename), bf, width, height, 100);
	}
}

// Add trajectory to model
void AtenForm::on_actionFileAddTrajectory_triggered(bool checked)
{
	Filter *f;
	Model *m = aten.currentModel();
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Trajectory", currentDirectory_.path(), loadTrajectoryFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		for (f = aten.filters(Filter::TrajectoryImport); f != NULL; f = f->next) if (selFilter == f->description()) break;
		// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (f == NULL) f = aten.probeFile(qPrintable(filename), Filter::TrajectoryImport);
		if (f != NULL)
		{
			m->initialiseTrajectory(qPrintable(filename), f);
			// Ensure trajectory toolbar is visible and View->Trajectory is selected
			ui.TrajectoryToolbar->setVisible(TRUE);
			ui.actionViewTrajectory->setChecked(TRUE);
			gui.updateTrajControls();
		}
		else msg.print( "Couldn't determine trajectory file format.\n");
		gui.modelChanged();
	}
}

// Save expression
void AtenForm::on_actionFileSaveExpression_triggered(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SAVEEXPRESSION, "cc", "none", "none");
	Filter *f;
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getSaveFileName(this, "Save Expression", currentDirectory_.path(), saveExpressionFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		for (f = aten.filters(Filter::ExpressionExport); f != NULL; f = f->next) if (selFilter == f->description()) break;
		if (f == NULL) printf("AtenForm::actionFileSaveExpression dialog <<<< Didn't recognise selected file filter '%s' >>>>\n", qPrintable(selFilter));
		else
		{
			cmd.pokeArguments("cc", f->nickname(), qPrintable(filename));
			cmd.execute();
		}
	}
}

// Open grid file
void AtenForm::on_actionFileOpenGrid_triggered(bool checked)
{
	// Call routine in grids window...
	gui.gridsWindow->loadGrid();
}

// Open forcefield file
void AtenForm::on_actionFileOpenForcefield_triggered(bool checked)
{
	// Call routine in forcefields window...
	gui.forcefieldsWindow->loadForcefield();
}

// Quit program
void AtenForm::on_actionFileQuit_triggered(bool checked)
{
	if (!gui.saveBeforeClose()) return;
	saveSettings();
	gui.app->exit(0);
}

