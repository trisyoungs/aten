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

#include <QtGui/QFileDialog>
#include <QtGui/QMessageBox>
#include <QtGui/QInputDialog>
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/saveimage.h"
#include "gui/trajectory.h"
#include "gui/prefs.h"
#include "gui/forcefields.h"
#include "gui/grids.h"
#include "base/sysfunc.h"

/*
// File Menu
*/

// Add new model to workspace
void AtenWindow::on_actionFileNew_triggered(bool checked)
{
	Model* m = aten_.addModel();
	m->enableUndoRedo();

	// Update GUI
	aten_.setCurrentModel(m, TRUE);
	updateWidgets(AtenWindow::AllTarget);
}

// Open existing file
void AtenWindow::on_actionFileOpen_triggered(bool checked)
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
				filter = selectFilter.selectFilter("Extension doesn't match any in known model export filters.", NULL, aten_.filterList(FilterData::ModelExport), TRUE);
				if ((filter != NULL) && selectFilter.appendExtension())
				{
					if (filter->filter.extensions().count() != 0) filename += QString(".") + filter->filter.extensions().at(0);
				}
			}
		}
		saveModelFilter_ = filter;
		saveModelFilename_ = qPrintable(filename);
		if (filter == NULL) Messenger::print("No filter selected to save file '%s'. Not saved.", qPrintable(saveModelFilename_));
		return (saveModelFilter_ == NULL ? FALSE : TRUE);
	}
	else return FALSE;
}

// Save current model under a different name
void AtenWindow::on_actionFileSaveAs_triggered(bool checked)
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

// Save current model
void AtenWindow::on_actionFileSave_triggered(bool checked)
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

// Modify export options for current model's associated filter
void AtenWindow::on_actionExportOptions_triggered(bool checked)
{
	Model* m = aten_.currentModelOrFrame();
	if (m->filter() == NULL) Messenger::print("No filter currently assigned to model '%s', so there are no export options.", qPrintable(m->name()));
	else m->filter()->defaultDialog().execute();
}

// Close current model
void AtenWindow::on_actionFileClose_triggered(bool checked)
{
	closeModel(aten_.currentModel());
	updateWidgets(AtenWindow::AllTarget);
}

// Save the current view as a bitmap image.
void AtenWindow::on_actionFileSaveImage_triggered(bool checked)
{
	static SaveImageDialog saveImageDialog(this);
	if (!saveImageDialog.getImageDetails(ui.MainView->width(), ui.MainView->height())) return;

	// Get values from dialog
	int imageWidth = saveImageDialog.ui.ImageWidthSpin->value();
	int imageHeight = saveImageDialog.ui.ImageHeightSpin->value();
	Aten::BitmapFormat bf = Aten::bitmapFormatFromFilter(qPrintable(saveImageDialog.ui.ImageFormatCombo->currentText()));
	QString fileName = saveImageDialog.ui.FileNameEdit->text();
	if (fileName.isEmpty()) return;

	QPixmap pixmap = aten_.currentViewAsPixmap(imageWidth, imageHeight);
	pixmap.save(fileName, Aten::bitmapFormatExtension(bf), -1);
}

// Open grid file
void AtenWindow::on_actionFileOpenGrid_triggered(bool checked)
{
	// Call routine in grids window...
	gridsWidget->loadGrid();
}

// Quit program
void AtenWindow::on_actionFileQuit_triggered(bool checked)
{
	if (!saveBeforeClose()) return;
	saveSettings();
	QApplication::exit(0);
}

/*
// Edit Actions
*/

void AtenWindow::on_actionEditUndo_triggered(bool checked)
{
	CommandNode::run(Commands::Undo, "");
	postRedisplay();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::CellTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_actionEditRedo_triggered(bool checked)
{
	CommandNode::run(Commands::Redo, "");
	postRedisplay();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::CellTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_actionEditCut_triggered(bool checked)
{
	CommandNode::run(Commands::Cut, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionEditCopy_triggered(bool checked)
{
	CommandNode::run(Commands::Copy, "");
	updateWidgets(AtenWindow::CanvasTarget);
}

void AtenWindow::on_actionEditPaste_triggered(bool checked)
{
	CommandNode::run(Commands::Paste, "");
	postRedisplay();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

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
		postRedisplay();
		updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
	}
}

void AtenWindow::on_actionEditDelete_triggered(bool checked)
{
	CommandNode::run(Commands::Delete, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionAll_triggered(bool checked)
{
	CommandNode::run(Commands::SelectAll, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionNone_triggered(bool checked)
{
	CommandNode::run(Commands::SelectNone, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionInvert_triggered(bool checked)
{
	CommandNode::run(Commands::Invert, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionExpand_triggered(bool on)
{
	CommandNode::run(Commands::Expand, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionEditQuickCommand_triggered(bool on)
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
				if (but == QMessageBox::Retry) ok = FALSE;
				else break;
			}
		}
		else break;
	} while (!ok);
}

/*
// View Actions
*/

// Zoom in
void AtenWindow::on_actionViewZoomIn_triggered(bool checked)
{
	aten_.currentModelOrFrame()->adjustCamera(0.0,0.0,5.0);
	postRedisplay();
}

// Zoom out
void AtenWindow::on_actionViewZoomOut_triggered(bool checked)
{
	aten_.currentModelOrFrame()->adjustCamera(0.0,0.0,-5.0);
	postRedisplay();
}

// Reset view
void AtenWindow::on_actionViewReset_triggered(bool checked)
{
	aten_.currentModelOrFrame()->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());
	postRedisplay();
}

// Set perspective view
void AtenWindow::on_actionViewPerspective_triggered(bool checked)
{
	if (!checked) return;
	prefs.setPerspective(TRUE);
	postRedisplay();
}

// Set orthographic view
void AtenWindow::on_actionViewOrthographic_triggered(bool checked)
{
	prefs.setPerspective(FALSE);
	postRedisplay();
}

// Set view along cartesian axis supplied
void AtenWindow::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	aten_.currentModelOrFrame()->viewAlong(x,y,z);
	postRedisplay();
}

// Set view along Cell axis supplied
void AtenWindow::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified cell axis
	aten_.currentModelOrFrame()->viewAlongCell(x,y,z);
	postRedisplay();
}

void AtenWindow::on_actionViewSetCartesianPosX_triggered(bool checked)
{
	 setCartesianView(1,0,0);
}

void AtenWindow::on_actionViewSetCartesianPosY_triggered(bool checked)
{
	 setCartesianView(0,1,0);
}

void AtenWindow::on_actionViewSetCartesianPosZ_triggered(bool checked)
{
	 setCartesianView(0,0,1);
}

void AtenWindow::on_actionViewSetCartesianNegX_triggered(bool checked)
{
	 setCartesianView(-1,0,0);
}

void AtenWindow::on_actionViewSetCartesianNegY_triggered(bool checked)
{
	 setCartesianView(0,-1,0);
}

void AtenWindow::on_actionViewSetCartesianNegZ_triggered(bool checked)
{
	 setCartesianView(0,0,-1);
}

void AtenWindow::on_actionViewSetCellNegX_triggered(bool checked)
{
	 setCellView(1,0,0);
}

void AtenWindow::on_actionViewSetCellNegY_triggered(bool checked)
{
	 setCellView(0,1,0);
}

void AtenWindow::on_actionViewSetCellNegZ_triggered(bool checked)
{
	 setCellView(0,0,1);
}

void AtenWindow::on_actionViewSetCellPosX_triggered(bool checked)
{
	 setCellView(-1,0,0);
}

void AtenWindow::on_actionViewSetCellPosY_triggered(bool checked)
{
	 setCellView(0,-1,0);
}

void AtenWindow::on_actionViewSetCellPosZ_triggered(bool checked)
{
	 setCellView(0,0,-1);
}

// Set current colouring scheme to elemental colours
void AtenWindow::on_actionSchemeElement_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ElementScheme);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current colouring scheme to charge
void AtenWindow::on_actionSchemeCharge_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ChargeScheme);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current colouring scheme to force
void AtenWindow::on_actionSchemeForce_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ForceScheme);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current colouring scheme to velocity
void AtenWindow::on_actionSchemeVelocity_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::VelocityScheme);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current colouring scheme to custom
void AtenWindow::on_actionSchemeCustom_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::CustomScheme);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set scheme actions to reflect supplied Prefs::ColouringScheme
void AtenWindow::setActiveSchemeAction(Prefs::ColouringScheme cs)
{
	if (cs == Prefs::ChargeScheme) ui.actionSchemeCharge->setChecked(TRUE);
	else if (cs == Prefs::ElementScheme) ui.actionSchemeElement->setChecked(TRUE);
	else if (cs == Prefs::ForceScheme) ui.actionSchemeForce->setChecked(TRUE);
	else if (cs == Prefs::VelocityScheme) ui.actionSchemeVelocity->setChecked(TRUE);
	else if (cs == Prefs::CustomScheme) ui.actionSchemeCustom->setChecked(TRUE);
	prefs.setColourScheme(cs);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Toggle detection and siaply of hydrogen bonds in models
void AtenWindow::on_actionDetectDisplayHBonds_triggered(bool checked)
{
	prefs.setDrawHydrogenBonds(checked);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

/*
// Model Actions
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
		updateWindowTitle();
	}
}

// Fold atoms in model
void AtenWindow::on_actionModelFoldAtoms_triggered(bool checked)
{
	CommandNode::run(Commands::Fold, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Fold molecules in model
void AtenWindow::on_actionModelFoldMolecules_triggered(bool checked)
{
	CommandNode::run(Commands::FoldMolecules, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
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
		aten_.setCurrentModel(m->next == NULL ? aten_.models() : m->next, TRUE);
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
		aten_.setCurrentModel(m->prev == NULL ? aten_.model(aten_.nModels()-1) : m->prev, TRUE);
	}
	updateWidgets(AtenWindow::AllTarget);
}

// Show all atoms in current model
void AtenWindow::on_actionModelShowAll_triggered(bool checked)
{
	CommandNode::run(Commands::ShowAll, "");
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// List all measurements in model
void AtenWindow::on_actionListMeasurements_triggered(bool on)
{
	aten_.currentModelOrFrame()->listMeasurements();
}

/*
// Trajectory Actions
*/

// Add trajectory to model
void AtenWindow::on_actionTrajectoryOpen_triggered(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	trajectoryWidget->stopTrajectoryPlayback();
	ui.actionTrajectoryModel->trigger();

	Tree* filter;
	Model* m = aten_.currentModel();
	static QDir currentDirectory_(aten_.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Trajectory", currentDirectory_.path(), aten_.fileDialogFilters(FilterData::TrajectoryImport), &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		
		// Find the filter that was selected
		filter = aten_.findFilterByDescription(FilterData::TrajectoryImport, qPrintable(selFilter));

		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten_.probeFile(qPrintable(filename), FilterData::TrajectoryImport);
		if (filter != NULL)
		{
			m->initialiseTrajectory(qPrintable(filename), filter);
			updateTrajectoryMenu();
		}
		else Messenger::print("Couldn't determine trajectory file format.");
		updateWidgets(AtenWindow::AllTarget);
	}
}

// Remove associated trajectory to model
void AtenWindow::on_actionTrajectoryRemove_triggered(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	trajectoryWidget->stopTrajectoryPlayback();
	ui.actionTrajectoryModel->trigger();
	
	Model* m = aten_.currentModel();
	m->clearTrajectory();
	updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model's trajectory to the model.
void AtenWindow::on_actionTrajectoryModel_triggered(bool checked)
{
	aten_.currentModel()->setRenderSource(Model::ModelSource);
	trajectoryWidget->refresh();
	updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model to the model's trajectory
void AtenWindow::on_actionTrajectoryFrames_triggered(bool checked)
{
	aten_.currentModel()->setRenderSource(Model::TrajectorySource);
	trajectoryWidget->refresh();
	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryFirstFrame_triggered(bool checked)
{
	aten_.currentModel()->seekFirstTrajectoryFrame();
	trajectoryWidget->refresh();
	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryLastFrame_triggered(bool checked)
{
	aten_.currentModel()->seekLastTrajectoryFrame();
	trajectoryWidget->refresh();
	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryPlayPause_triggered(bool checked)
{
	trajectoryWidget->ui.TrajectoryPlayPauseButton->setChecked(checked);
// 	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectorySaveMovie_triggered(bool checked)
{
	QString geometry;
	geometry.sprintf("%ix%i", ui.MainView->width(), ui.MainView->height());
	int width, height;
	
	Model* m = aten_.currentModel();
	Tree dialog;
	TreeGui &ui = dialog.defaultDialog();
	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Movie Options");
	ui.addEdit("geometry", "Image Geometry", geometry ,1,1);
	ui.addIntegerSpin("firstframe", "First Frame", 1, m->nTrajectoryFrames(), 1, 1 ,1,2);
	ui.addIntegerSpin("lastframe", "Last Frame", 1, m->nTrajectoryFrames(), 1, m->nTrajectoryFrames(),1,3);
	ui.addIntegerSpin("frameskip", "Frame Skip", 0, 1e6, 1, 0 ,1,4);
	ui.addIntegerSpin("fps", "Movie FPS", 1, 200, 1, 25 ,1,5);
	
	if (!dialog.defaultDialog().execute()) return;

	// Retrieve widget values
	geometry = ui.asString("geometry");
// 	width = atoi(beforeChar(geometry,'x'));	ATEN2 TODO
// 	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		QString message = "The geometry '" + geometry + "' is not valid since one (or both) components are less than 1.";
		QMessageBox::warning(this, "Aten", message, QMessageBox::Ok);
		return;
	}
	int firstframe = ui.asInteger("firstframe");
	int lastframe = ui.asInteger("lastframe");
	int frameskip = ui.asInteger("frameskip");
	int fps = ui.asInteger("fps");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory_(aten_.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Movie", currentDirectory_.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;

	// Store path for next use
	currentDirectory_.setPath(filename);
	
	// Generate movie file...
	CommandNode::run(Commands::SaveMovie, "ciiiiiii", qPrintable(filename), width, height, -1, firstframe, lastframe, frameskip, fps);
}

void AtenWindow::on_actionTrajectoryInheritParentStyle_triggered(bool checked)
{
	if (!checked) return;
	// If a trajectory is already associated, change its style now
	Model* m = aten_.currentModel();
	if (m->nTrajectoryFrames() == 0) return;
	else m->trajectoryCopyAtomStyle(m);
}

void AtenWindow::on_actionTrajectoryCopyStyleToParent_triggered(bool checked)
{
	Model* m = aten_.currentModel();
	Model* frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->copyAtomStyle(frame);
}

void AtenWindow::on_actionTrajectoryPropagateStyleFromHere_triggered(bool checked)
{
	Model* m = aten_.currentModel();
	Model* frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->trajectoryCopyAtomStyle(frame);
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
	postRedisplay();
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
				filter = selectFilter.selectFilter("Couldn't determine format to save expression in.", NULL, aten_.filterList(FilterData::ExpressionExport), TRUE);
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
				filter = selectFilter.selectFilter("Extension doesn't match any in known expression export filters.", NULL, aten_.filterList(FilterData::ExpressionExport), TRUE);
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
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Remove patterns from model
void AtenWindow::on_actionModelRemovePatterns_triggered(bool checked)
{
	aten_.currentModelOrFrame()->clearPatterns();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
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
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Remove typing from model
void AtenWindow::on_actionModelFFUntype_triggered(bool checked)
{
	aten_.currentModelOrFrame()->removeTyping();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Create energy expression for model
void AtenWindow::on_actionModelCreateExpression_triggered(bool checked)
{
	aten_.currentModelOrFrame()->createExpression(Choice(), Choice(), Choice(), aten_.currentForcefield(), aten_.combinationRules());
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Add default pattern
void AtenWindow::on_actionModelAddDefaultPattern_triggered(bool checked)
{
	aten_.currentModelOrFrame()->createDefaultPattern();
	updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
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
	aten_.createFileDialogFilters();
}

// Toggle manualswapbuffers option
void AtenWindow::on_actionManualSwapBuffers_triggered(bool checked)
{
	prefs.setManualSwapBuffers(checked);
	postRedisplay();
}

