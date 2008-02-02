/*
	*** Qt mainwindow file action functions
	*** src/gui-qt/action_funcs.cpp
	Copyright T. Youngs 2007

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
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"
#include "gui-qt/prefs.h"
#include <QtGui/QFileDialog>
//#include <QtGui/QPixmap>
//#include <QtGui/QMessageBox>

/*
// File Actions
*/

void AtenForm::on_actionFileNew_triggered(bool checked)
{
	dbg_begin(DM_CALLS,"AtenForm::on_actionFileNew_triggered");
	master.add_model();
	dbg_end(DM_CALLS,"AtenForm::on_actionFileNew_triggered");
}

void AtenForm::on_actionFileOpen_triggered(bool checked)
{
	filter *f;
	model *m;
	QString filename;
	QStringList filenames;
	if (dialog[FT_MODEL_IMPORT]->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = dialog[FT_MODEL_IMPORT]->selectedFilter();
		// Find the corresponding Aten filter that was selected
		for (f = master.filters[FT_MODEL_IMPORT].first(); f != NULL; f = f->next)
			if (strcmp(f->get_description(),qPrintable(filter)) == 0) break;
		// Get selected filename list
		filenames = dialog[FT_MODEL_IMPORT]->selectedFiles();
		// Loop over selected files
		for (int i = 0; i < filenames.size(); ++i)
		{
			filename = filenames.at(i);
			// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
			if (f == NULL) f = master.probe_file(qPrintable(filename), FT_MODEL_IMPORT);
			if (f != NULL)
			{
				f->execute(qPrintable(filename));
				add_recent(qPrintable(filename));
			}
		}
		refresh_modeltabs();
		master.get_currentmodel()->log_change(LOG_VISUAL);
		gui.refresh();
	}
}

bool AtenForm::run_savemodel_dialog()
{
	savemodelfilter = NULL;
	savemodelfilename.clear();
	filter *f;
	if (dialog[FT_MODEL_EXPORT]->exec() == 1)
	{
		// Get selected filename (only grab first
		//QString filename = savemodeldialog->selectedFiles().first();
		savemodelfilename = qPrintable(dialog[FT_MODEL_EXPORT]->selectedFiles().first());
		// Get selected filter
		QString filter = dialog[FT_MODEL_EXPORT]->selectedFilter();
		// Find the filter that was selected
		for (f = master.filters[FT_MODEL_EXPORT].first(); f != NULL; f = f->next)
			if (strcmp(f->get_description(),qPrintable(filter)) == 0) break;
		if (f == NULL) printf("AtenForm::run_savemodel_dialog <<<< Didn't recognise selected file filter '%s' >>>>\n", qPrintable(filter));
		savemodelfilter = f;
		return (savemodelfilter == NULL ? FALSE : TRUE);
	}
	else return FALSE;
}

void AtenForm::on_actionFileSaveAs_triggered(bool checked)
{
	model *m;
	if (run_savemodel_dialog())
	{
		m = master.get_currentmodel();
		m->set_filter(savemodelfilter);
		m->set_filename(savemodelfilename.get());
		savemodelfilter->execute(savemodelfilename.get());
		refresh_modeltabs();
		gui.refresh();
	}
}

void AtenForm::on_actionFileSave_triggered(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	model *m = master.get_currentmodel();
	filter *f = m->get_filter();
	if ((f != NULL) && (f->get_type() != FT_MODEL_EXPORT)) f = NULL;
	dnchar filename;
	filename = m->get_filename();
	if (filename.empty() || (f == NULL))
	{
		if (run_savemodel_dialog())
		{
			m = master.get_currentmodel();
			m->set_filter(savemodelfilter);
			m->set_filename(savemodelfilename.get());
			savemodelfilter->execute(savemodelfilename.get());
			refresh_modeltabs();
		}
	}
	else f->execute(savemodelfilename.get());
	gui.refresh();
}

void AtenForm::on_actionFileClose_triggered(bool checked)
{
	// If the current model has been modified, ask for confirmation before we close it
	char text[512];
	filter *f;
	model *m = master.get_currentmodel();
	if (m->is_modified())
	{
		// Create a model message dialog
		sprintf(text, "Model '%s' has been modified.\n", m->get_name());
		int returnvalue = QMessageBox::warning(this, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				master.remove_model(m);
				break;
			// Cancel quit and return to app
			case (QMessageBox::Cancel):
				return;
			// Save model before quit
			case (QMessageBox::Save):
				// If model has a filter set, just save it
				f = m->get_filter();
				if (f != NULL) f->execute(m->get_filename());
				else if (run_savemodel_dialog())
				{
					m->set_filter(savemodelfilter);
					m->set_filename(savemodelfilename.get());
					savemodelfilter->execute(savemodelfilename.get());
				}
				else return;
				master.remove_model(m);
				break;
		}
	}
	else master.remove_model(m);
}

void AtenForm::on_actionFileSaveImage_triggered(bool checked)
{
	// Save the current view as a bitmap image.
	// Create a QPixmap of the current scene
	QPixmap pixmap = ui.ModelView->renderPixmap(0,0,FALSE);
	// Get filename from user
	int n;
	if (saveimagedialog->exec() == 1)
	{
		// Get selected filename
		QStringList filenames = saveimagedialog->selectedFiles();
		QString filename = filenames.first();
		// Get selected filter
		QString filter = saveimagedialog->selectedFilter();
		// Find the filter that was selected
		for (n=0; n<PF_NITEMS; n++)
			if (strcmp(filter_from_PF( (pixmap_format) n), qPrintable(filter)) == 0) break;
		if (n != PF_NITEMS) pixmap.save(filename, extension_from_PF( (pixmap_format) n), -1);
	}
}

void AtenForm::on_actionFileAddTrajectory_triggered(bool checked)
{
	filter *f;
	model *m = master.get_currentmodel();
	if (dialog[FT_TRAJECTORY_IMPORT]->exec() == 1)
	{
		// Get selected filename
		QStringList filenames = dialog[FT_TRAJECTORY_IMPORT]->selectedFiles();
		QString filename = filenames.first();
		// Get selected filter
		QString filter = dialog[FT_TRAJECTORY_IMPORT]->selectedFilter();
		// Find the filter that was selected
		for (f = master.filters[FT_TRAJECTORY_IMPORT].first(); f != NULL; f = f->next)
			if (strcmp(f->get_description(),qPrintable(filter)) == 0) break;
		// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (f == NULL) f = master.probe_file(qPrintable(filename), FT_TRAJECTORY_IMPORT);
		if (f != NULL)
		{
			m->initialise_trajectory(qPrintable(filename), f);
			// Ensure trajectory toolbar is visible and View->Trajectory is selected
			ui.TrajectoryToolBar->setVisible(TRUE);
			ui.actionViewTrajectory->setChecked(TRUE);
		}
		else msg(DM_NONE, "Couldn't determine trajectory file format.\n");
		gui.refresh();
	}
}

void AtenForm::on_actionFileLoadForcefield_triggered(bool checked)
{
	QString filename;
	if (dialog[FT_FIELD_IMPORT]->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = dialog[FT_FIELD_IMPORT]->selectedFilter();
		filename = dialog[FT_FIELD_IMPORT]->selectedFiles().first();
		master.load_ff(qPrintable(filename));
		refresh_forcefieldpage();
	}
}

void AtenForm::on_actionFileLoadGridData_triggered(bool checked)
{
	filter *f;
	grid *g;
	QString filename;
	QStringList filenames;
	if (dialog[FT_GRID_IMPORT]->exec() == 1)
	{
		// Get selected filter in file dialog
		QString filter = dialog[FT_GRID_IMPORT]->selectedFilter();
		// Find the corresponding Aten filter that was selected
		for (f = master.filters[FT_GRID_IMPORT].first(); f != NULL; f = f->next)
			if (strcmp(f->get_description(),qPrintable(filter)) == 0) break;
		// Get selected filename list
		filenames = dialog[FT_GRID_IMPORT]->selectedFiles();
		// Loop over selected files
		for (int i = 0; i < filenames.size(); ++i)
		{
			filename = filenames.at(i);
			// If f == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
			if (f != NULL) f->execute(qPrintable(filename));
			else
			{
				f = master.probe_file(qPrintable(filename), FT_GRID_IMPORT);
				if (f != NULL) f->execute(qPrintable(filename));
			}
		}
		refresh_gridspage();
		gui.refresh();
	}
}

void AtenForm::on_actionFileQuit_triggered(bool checked)
{
	if (!gui.save_before_close()) return;
	save_settings();
	gui.app->exit(0);
}
