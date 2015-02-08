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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/trajectory.h"
#include "gui/prefs.h"
#include "gui/forcefields.h"
#include "gui/grids.h"
#include "base/sysfunc.h"
#include "parser/commandnode.h"

/*
// File Menu
*/

// Add new model to workspace
void AtenWindow::on_actionFileNew_triggered(bool checked)
{
	Model *m = aten.addModel();
	m->enableUndoRedo();
	m->regenerateIcon();

	// Update GUI
	aten.setCurrentModel(m, TRUE);
	updateWidgets(AtenWindow::AllTarget);
}

// Open existing file
void AtenWindow::on_actionFileOpen_triggered(bool checked)
{
	AtenLoadModel loadModelDialog(this);
	Tree* filter;
	if (loadModelDialog.exec() == 1)
	{
		filter = loadModelDialog.selectedFormat();
		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten.probeFile(loadModelDialog.selectedFilename(), FilterData::ModelImport);
		if (filter != NULL)
		{
			if (!filter->executeRead(loadModelDialog.selectedFilename())) return;
			addRecent(loadModelDialog.selectedFilename());
			aten.currentModelOrFrame()->changeLog.add(Log::Camera);
			aten.currentModelOrFrame()->regenerateIcon();
			updateWidgets(AtenWindow::AllTarget);
		}
	}
}

// Local save function
bool AtenWindow::runSaveModelDialog()
{
	saveModelFilename.clear();
	saveModelFilter = NULL;
	Tree* filter = NULL;
	static QString selectedFilter(aten.filters(FilterData::ModelExport) == NULL ? NULL : aten.filters(FilterData::ModelExport)->item->filter.name());
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
void AtenWindow::on_actionFileSaveAs_triggered(bool checked)
{
	Model *m;
	if (runSaveModelDialog())
	{
		m = aten.currentModelOrFrame();
		if (m == NULL)
		{
			printf("Internal Error: Model pointer is NULL in AtenWindow::on_actionFileSaveAs_triggered.\n");
			return;
		}
		m->setFilter(saveModelFilter);
		m->setFilename(saveModelFilename.get());
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		
		if (saveModelFilter->executeWrite(saveModelFilename.get()))
		{
			m->changeLog.updateSavePoint();
			msg.print("Model '%s' saved to file '%s' (%s)\n", m->name(), saveModelFilename.get(), saveModelFilter->filter.name());
		}
		else msg.print("Failed to save model '%s'.\n", m->name());
		m->enableUndoRedo();
		gui.update();
	}
}

// Save current model
void AtenWindow::on_actionFileSave_triggered(bool checked)
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
			m->setFilter(saveModelFilter);
			m->setFilename(saveModelFilename.get());
			// Temporarily disable undo/redo for the model, save, and re-enable
			m->disableUndoRedo();
			if (saveModelFilter->executeWrite(saveModelFilename.get()))
			{
				m->changeLog.updateSavePoint();
				msg.print("Model '%s' saved to file '%s' (%s)\n", m->name(), saveModelFilename.get(), saveModelFilter->filter.name());
			}
			else msg.print("Failed to save model '%s'.\n", m->name());
			m->enableUndoRedo();
		}
	}
	else
	{
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		t->executeWrite(filename.get());
		m->changeLog.updateSavePoint();
		m->enableUndoRedo();
	}
	gui.update();
}

// Modify export options for current model's associated filter
void AtenWindow::on_actionExportOptions_triggered(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	if (m->filter() == NULL) msg.print("No filter currently assigned to model '%s', so there are no export options.\n", m->name());
	else m->filter()->defaultDialog().execute();
}

// Close current model
void AtenWindow::on_actionFileClose_triggered(bool checked)
{
	Model *m = aten.currentModel();
	aten.closeModel(m);
	parent_.updateWidgets(AtenWindow::AllTarget);
}

// Save the current view as a bitmap image.
void AtenWindow::on_actionFileSaveImage_triggered(bool checked)
{
	// Get geometry from user - initial setup is to use current canvas geometry
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainCanvas()->width(), (int) gui.mainCanvas()->height());
	int width, height;
	static bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView();
	bool currentframemodel, currentframeview, viewglobe;
	
	Tree dialog;
	TreeGuiWidget *group, *w;
	TreeGui &ui = dialog.defaultDialog();
	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Save Image Options");
	ui.addEdit("geometry", "Image Size", geometry.get(),1,1);
	group = ui.addRadioGroup("framechoice");
	ui.addRadioButton("noframes", "No Frames", "framechoice", 1, 1, 2, 1);
	ui.addRadioButton("framemodel", "Frame Current Model", "framechoice", 0, 1, 3, 1);
	ui.addRadioButton("frameview", "Frame Whole View", "framechoice", 0, 1, 4, 1);
	ui.addRadioButton("frameboth", "Frame Current Model and View", "framechoice", 0, 1, 5, 1);
	
	// Poke values into dialog widgets and execute
	ui.setWidgetValue("framechoice", framemodel ? (frameview ? 4 : 2) : (frameview ? 3 : 1) );
	if (!dialog.defaultDialog().execute()) return;

	// Get values from dialog
	geometry = ui.asCharacter("geometry");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int choice = ui.asInteger("framechoice");
	framemodel = choice%2 == 0;
	frameview = choice > 2;
	currentframemodel = prefs.frameCurrentModel();
	currentframeview = prefs.frameWholeView();
	viewglobe = prefs.viewRotationGlobe();
	
	// Get filename from user
	RenderEngine::BitmapFormat bf;
	static QString selectedFilter("Windows Bitmap (*.bmp)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Bitmap", currentDirectory_.path(), saveBitmapFilters, &selectedFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Grab filename extension and search for it
		Dnchar ext = afterLastChar(qPrintable(filename), '.');
		bf = RenderEngine::bitmapFormat(ext.get());
		// If we didn't recognise the extension, complain and quit
		if (bf == RenderEngine::nBitmapFormats) 
		{
			Dnchar message(-1, "Bitmap format not recognised - '%s'.\n", ext.get());
			QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		}
		else
		{
			prefs.setFrameCurrentModel(framemodel);
			prefs.setFrameWholeView(frameview);
			prefs.setViewRotationGlobe(FALSE);
			if (!engine().saveImage(qPrintable(filename), bf, width, height, -1)) msg.print("Failed to save image.\n");
			prefs.setFrameCurrentModel(currentframemodel);
			prefs.setFrameWholeView(currentframeview);
			prefs.setViewRotationGlobe(viewglobe);
		}
	}
}

// Open grid file
void AtenWindow::on_actionFileOpenGrid_triggered(bool checked)
{
	// Call routine in grids window...
	gui.gridsWidget->loadGrid();
}

// Quit program
void AtenWindow::on_actionFileQuit_triggered(bool checked)
{
	if (!gui.saveBeforeClose()) return;
	saveSettings();
	gui.application()->exit(0);
}

/*
// Edit Actions
*/

void AtenWindow::on_actionEditUndo_triggered(bool checked)
{
	CommandNode::run(Command::Undo, "");
	parent_.postRedisplay();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::CellTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_actionEditRedo_triggered(bool checked)
{
	CommandNode::run(Command::Redo, "");
	parent_.postRedisplay();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget+AtenWindow::CellTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_actionEditCut_triggered(bool checked)
{
	CommandNode::run(Command::Cut, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionEditCopy_triggered(bool checked)
{
	CommandNode::run(Command::Copy, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void AtenWindow::on_actionEditPaste_triggered(bool checked)
{
	CommandNode::run(Command::Paste, "");
	parent_.postRedisplay();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
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
		CommandNode::run(Command::Paste, "ddd", r.x, r.y, r.z);
		parent_.postRedisplay();
		parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
	}
}

void AtenWindow::on_actionEditDelete_triggered(bool checked)
{
	CommandNode::run(Command::Delete, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionAll_triggered(bool checked)
{
	CommandNode::run(Command::SelectAll, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionNone_triggered(bool checked)
{
	CommandNode::run(Command::SelectNone, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionInvert_triggered(bool checked)
{
	CommandNode::run(Command::Invert, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionSelectionExpand_triggered(bool on)
{
	CommandNode::run(Command::Expand, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::SelectTarget);
}

void AtenWindow::on_actionEditQuickCommand_triggered(bool on)
{
	// Raise an edit box to get the user command
	bool ok;
	static Dnchar command;
	do
	{
		QString text = QInputDialog::getText(this, tr("Quick Command "), tr("Command:"), QLineEdit::Normal, command.get(), &ok);
		if (ok && !text.isEmpty())
		{
			// Store command and attempt to 'compile' it
			command = qPrintable(text);
			Program program;;
			if (program.generateFromString(command, "Quick Command", "QuickCommand"))
			{
				ReturnValue rv;
				program.execute(rv);
				parent_.updateWidgets(AtenWindow::AllTarget);
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
	aten.currentModelOrFrame()->adjustCamera(0.0,0.0,5.0);
	parent_.postRedisplay();
}

// Zoom out
void AtenWindow::on_actionViewZoomOut_triggered(bool checked)
{
	aten.currentModelOrFrame()->adjustCamera(0.0,0.0,-5.0);
	parent_.postRedisplay();
}

// Reset view
void AtenWindow::on_actionViewReset_triggered(bool checked)
{
	aten.currentModelOrFrame()->resetView();
	parent_.postRedisplay();
}

// Set perspective view
void AtenWindow::on_actionViewPerspective_triggered(bool checked)
{
	if (!checked) return;
	prefs.setPerspective(TRUE);
	parent_.postRedisplay();
}

// Set orthographic view
void AtenWindow::on_actionViewOrthographic_triggered(bool checked)
{
	prefs.setPerspective(FALSE);
	parent_.postRedisplay();
}

// Set view along cartesian axis supplied
void AtenWindow::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	aten.currentModelOrFrame()->viewAlong(x,y,z);
	parent_.postRedisplay();
}

// Set view along Cell axis supplied
void AtenWindow::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified cell axis
	aten.currentModelOrFrame()->viewAlongCell(x,y,z);
	parent_.postRedisplay();
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
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current colouring scheme to charge
void AtenWindow::on_actionSchemeCharge_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ChargeScheme);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current colouring scheme to force
void AtenWindow::on_actionSchemeForce_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ForceScheme);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current colouring scheme to velocity
void AtenWindow::on_actionSchemeVelocity_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::VelocityScheme);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current colouring scheme to custom
void AtenWindow::on_actionSchemeCustom_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::CustomScheme);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
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
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Toggle detection and siaply of hydrogen bonds in models
void AtenWindow::on_actionDetectDisplayHBonds_triggered(bool checked)
{
	prefs.setDrawHydrogenBonds(checked);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

/*
// Model Actions
*/

// Rename model
void AtenWindow::on_actionModelRename_triggered(bool checked)
{
	Model *m = aten.currentModelOrFrame();
	bool ok;
	QString text = QInputDialog::getText(this, tr("Rename Model: ") + m->name(), tr("New name:"), QLineEdit::Normal, m->name(), &ok);
	if (ok && !text.isEmpty())
	{
		CommandNode::run(Command::SetName, "c", qPrintable(text));
		updateWindowTitle();
	}
}

// Fold atoms in model
void AtenWindow::on_actionModelFoldAtoms_triggered(bool checked)
{
	CommandNode::run(Command::Fold, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Fold molecules in model
void AtenWindow::on_actionModelFoldMolecules_triggered(bool checked)
{
	CommandNode::run(Command::FoldMolecules, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Move to next model in list
void AtenWindow::on_actionModelNext_triggered(bool checked)
{
	// If multiple models are visible, step along to next visible model. Otherwise, just next in list
	if (aten.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int> *ri;
		for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		aten.setCurrentModel(ri->next == NULL ? aten.visibleModels()->item : ri->next->item);
	}
	else
	{
		Model *m = aten.currentModel();
		aten.setCurrentModel(m->next == NULL ? aten.models() : m->next, TRUE);
	}
	parent_.updateWidgets(AtenWindow::AllTarget);
}

// Move to previous model in list
void AtenWindow::on_actionModelPrevious_triggered(bool checked)
{
	// If multiple models are visible, step back to previous visible model. Otherwise, just previous in list
	if (aten.nVisibleModels() > 1)
	{
		// Find current model in visible models list...
		Refitem<Model,int> *ri;
		for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->item == aten.currentModel()) break;
		if (ri == NULL)
		{
			printf("Internal Error : Failed to find current model in visible models list.\n");
			return;
		}
		// If previous pointer is NULL, need to get the last item in the list by hand
		if (ri->prev != NULL) aten.setCurrentModel(ri->prev->item);
		else for (ri = aten.visibleModels(); ri != NULL; ri = ri->next) if (ri->next == NULL) aten.setCurrentModel(ri->item);
	}
	else
	{
		Model *m = aten.currentModel();
		aten.setCurrentModel(m->prev == NULL ? aten.model(aten.nModels()-1) : m->prev, TRUE);
	}
	parent_.updateWidgets(AtenWindow::AllTarget);
}

// Show all atoms in current model
void AtenWindow::on_actionModelShowAll_triggered(bool checked)
{
	CommandNode::run(Command::ShowAll, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// List all measurements in model
void AtenWindow::on_actionListMeasurements_triggered(bool on)
{
	aten.currentModelOrFrame()->listMeasurements();
}

/*
// Trajectory Actions
*/

// Add trajectory to model
void AtenWindow::on_actionTrajectoryOpen_triggered(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	gui.trajectoryWidget->stopTrajectoryPlayback();
	ui.actionTrajectoryModel->trigger();

	Tree *filter;
	Model *m = aten.currentModel();
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Trajectory", currentDirectory_.path(), loadTrajectoryFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		
		// Find the filter that was selected
		filter = aten.findFilterByDescription(FilterData::TrajectoryImport, qPrintable(selFilter));
		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten.probeFile(qPrintable(filename), FilterData::TrajectoryImport);
		if (filter != NULL)
		{
			m->initialiseTrajectory(qPrintable(filename), filter);
			updateTrajectoryMenu();
		}
		else msg.print( "Couldn't determine trajectory file format.\n");
		parent_.updateWidgets(AtenWindow::AllTarget);
	}
}

// Remove associated trajectory to model
void AtenWindow::on_actionTrajectoryRemove_triggered(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	gui.trajectoryWidget->stopTrajectoryPlayback();
	ui.actionTrajectoryModel->trigger();
	
	Model *m = aten.currentModel();
	m->clearTrajectory();
	parent_.updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model's trajectory to the model.
void AtenWindow::on_actionTrajectoryModel_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::ModelSource);
	gui.trajectoryWidget->refresh();
	parent_.updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model to the model's trajectory
void AtenWindow::on_actionTrajectoryFrames_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::TrajectorySource);
	gui.trajectoryWidget->refresh();
	parent_.updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryFirstFrame_triggered(bool checked)
{
	aten.currentModel()->seekFirstTrajectoryFrame();
	gui.trajectoryWidget->refresh();
	parent_.updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryLastFrame_triggered(bool checked)
{
	aten.currentModel()->seekLastTrajectoryFrame();
	gui.trajectoryWidget->refresh();
	parent_.updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectoryPlayPause_triggered(bool checked)
{
	gui.trajectoryWidget->ui.TrajectoryPlayPauseButton->setChecked(checked);
// 	parent_.updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_actionTrajectorySaveMovie_triggered(bool checked)
{
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainCanvas()->width(), (int) gui.mainCanvas()->height());
	int width, height;
	
// 	static Tree dialog("Save Movie","option('Image Size', 'edit', '10x10'); option('First Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Last Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Frame Interval', 'intspin', 1, 9999999, 0, 1, 'newline'); option('Movie FPS', 'intspin', 1, 100, 25, 1, 'newline'); ");
	Model *m = aten.currentModel();
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
	geometry = ui.asCharacter("geometry");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int firstframe = ui.asInteger("firstframe");
	int lastframe = ui.asInteger("lastframe");
	int frameskip = ui.asInteger("frameskip");
	int fps = ui.asInteger("fps");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Movie", currentDirectory_.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	// Store path for next use
	currentDirectory_.setPath(filename);
	
	// Generate movie file...
	CommandNode::run(Command::SaveMovie, "ciiiiiii", qPrintable(filename), width, height, -1, firstframe, lastframe, frameskip, fps);
}

void AtenWindow::on_actionTrajectoryInheritParentStyle_triggered(bool checked)
{
	if (!checked) return;
	// If a trajectory is already associated, change its style now
	Model *m = aten.currentModel();
	if (m->nTrajectoryFrames() == 0) return;
	else m->trajectoryCopyAtomStyle(m);
}

void AtenWindow::on_actionTrajectoryCopyStyleToParent_triggered(bool checked)
{
	Model *m = aten.currentModel();
	Model *frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->copyAtomStyle(frame);
}

void AtenWindow::on_actionTrajectoryPropagateStyleFromHere_triggered(bool checked)
{
	
	Model *m = aten.currentModel();
	Model *frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->trajectoryCopyAtomStyle(frame);
}

/*
// Forcefield Actions
*/

// Open forcefield file
void AtenWindow::on_actionOpenForcefield_triggered(bool checked)
{
	// Call routine in forcefields window...
	gui.forcefieldsWidget->loadForcefield();
}

// Open expression file
void AtenWindow::on_actionOpenExpression_triggered(bool checked)
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
			if (!filter->executeRead(qPrintable(filename))) return;
		}
	}
	parent_.postRedisplay();
}

// Save expression
void AtenWindow::on_actionSaveExpression_triggered(bool checked)
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
			// Temporarily disable undo/redo for the model, save expression, and re-enable
			m->disableUndoRedo();
			if (filter->executeWrite(qPrintable(filename))) msg.print("Expression for model '%s' saved to file '%s' (%s)\n", m->name(), qPrintable(filename), filter->filter.name());
			else msg.print("Failed to save expression for model '%s'.\n", m->name());
			m->enableUndoRedo();
		}
	}
}

// Create patterns for model
void AtenWindow::on_actionModelCreatePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->createPatterns();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Remove patterns from model
void AtenWindow::on_actionModelRemovePatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->clearPatterns();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// List patterns in model
void AtenWindow::on_actionModelListPatterns_triggered(bool checked)
{
	aten.currentModelOrFrame()->printPatterns();
}

// Perform forcefield typing in model
void AtenWindow::on_actionModelFFType_triggered(bool checked)
{
	aten.currentModelOrFrame()->typeAll();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Remove typing from model
void AtenWindow::on_actionModelFFUntype_triggered(bool checked)
{
	aten.currentModelOrFrame()->removeTyping();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Create energy expression for model
void AtenWindow::on_actionModelCreateExpression_triggered(bool checked)
{
	aten.currentModelOrFrame()->createExpression();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

// Add default pattern
void AtenWindow::on_actionModelAddDefaultPattern_triggered(bool checked)
{
	aten.currentModelOrFrame()->createDefaultPattern();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

/*
// Settings Actions
*/

// Show preferences window
void AtenWindow::on_actionPreferences_triggered(bool checked)
{
	gui.prefsDialog->setControls();
	gui.prefsDialog->exec();
}

// Reload all filters
void AtenWindow::on_actionReloadFilters_triggered(bool checked)
{
	if (aten.reloadFilters() > 0)
	{
		QMessageBox::warning(this, "Aten", "Errors encountered while reloading filters - see message box for details.", QMessageBox::Ok);
	}
	createDialogFilters();
	gui.loadModelDialog->setControls();
}

// Show main ToolBox
void AtenWindow::on_actionShowToolBox_triggered(bool checked)
{
	gui.toolBoxWidget->setVisible(TRUE);
	gui.toolBoxWidget->setFloating(TRUE);
	gui.toolBoxWidget->move(200,200);
}

// Toggle manualswapbuffers option
void AtenWindow::on_actionManualSwapBuffers_triggered(bool checked)
{
	prefs.setManualSwapBuffers(checked);
	parent_.postRedisplay();
}

