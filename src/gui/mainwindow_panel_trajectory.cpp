/*
	*** Main Window - Trajectory Panel Functions
	*** src/gui/mainwindow_panel_trajectory.cpp
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
#include <QFileDialog>
#include <QMessageBox>

// Update trajectory panel
void AtenWindow::updateTrajectoryPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateTrajectoryPanel");

	// Enable / disable controls
	bool hasTraj = sourceModel ? sourceModel->hasTrajectory() : false;
	ui.TrajectorySourceFramesButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlFirstButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlPreviousButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlPlayButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlNextButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlLastButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlFrameSpin->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlFrameSlider->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryControlDelaySpin->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryStyleInheritButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryStylePromoteButton->setEnabled(sourceModel && hasTraj);
	ui.TrajectoryStylePropagateButton->setEnabled(sourceModel && sourceModel->trajectoryIsCached());

	if (hasTraj)
	{
		ui.TrajectorySourceFramesButton->setChecked(sourceModel->renderSource() == Model::TrajectorySource);
		ui.TrajectoryControlFrameSlider->setRange(1, sourceModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSlider->setValue(sourceModel->trajectoryFrameIndex()+1);
		ui.TrajectoryControlFrameSpin->setRange(1, sourceModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSpin->setValue(sourceModel->trajectoryFrameIndex()+1);
	}

	Messenger::exit("AtenWindow::updateTrajectoryPanel");
}

// Stop trajectory playback
void AtenWindow::stopTrajectoryPlayback()
{
	if (ui.TrajectoryControlPlayButton->isChecked())
	{
		ui.TrajectoryControlPlayButton->click();
		updateWidgets(AtenWindow::MainViewTarget+AtenWindow::TrajectoryPanelTarget);
	}
}

/*
 * Switch
 */

void AtenWindow::on_TrajectorySourceOpenButton_clicked(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	stopTrajectoryPlayback();

	// Get current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	Tree* filter;
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
			currentModel->initialiseTrajectory(qPrintable(filename), filter);
		}
		else Messenger::print("Couldn't determine trajectory file format.");
	}

	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_TrajectorySourceRemoveButton_clicked(bool checked)
{
	// Stop playback, and set view to be the parent model before we do anything
	stopTrajectoryPlayback();

	// Get current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	
	currentModel->clearTrajectory();

	updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model to the trajectory (or vice versa)
void AtenWindow::on_TrajectorySourceFramesButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->setRenderSource(checked ? Model::TrajectorySource : Model::ModelSource);

	updateWidgets();
}

/*
 * Control
 */

// Skip to first frame in trajectory
void AtenWindow::on_TrajectoryControlFirstButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->seekFirstTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

// Skip to previous frame in trajectory
void AtenWindow::on_TrajectoryControlPreviousButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->seekPreviousTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

// Play/pause trajectory playback
void AtenWindow::on_TrajectoryControlPlayButton_clicked(bool checked)
{
	// If button is depressed, begin playback
	if (checked)
	{
		trajectoryTimerId_ = startTimer(ui.TrajectoryControlDelaySpin->value());
		ui.MainView->setEditable(false);
	}
	else
	{
		killTimer(trajectoryTimerId_);
		trajectoryTimerId_ = -1;
		ui.MainView->setEditable(true);
	}
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::TrajectoryPanelTarget);
}

// Skip to next frame in trajectory
void AtenWindow::on_TrajectoryControlNextButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->seekNextTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

// Skip to last frame in trajectory
void AtenWindow::on_TrajectoryControlLastButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->seekLastTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_TrajectoryControlFrameSpin_valueChanged(int value)
{
	if (refreshing_) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	currentModel->seekTrajectoryFrame(value-1);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryControlFrameSlider_positionChanged(int position)
{
	if (refreshing_) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	currentModel->seekTrajectoryFrame(position-1);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryControlDelaySpin_valueCHanged(int value)
{
}

/*
 * Style
 */

void AtenWindow::on_TrajectoryStyleInheritButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;
	if (currentModel->nTrajectoryFrames() == 0) return;

	currentModel->trajectoryCopyAtomStyle(currentModel);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePropagateButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	Model* frame = currentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	currentModel->trajectoryCopyAtomStyle(frame);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePromoteButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	Model* frame = currentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	currentModel->copyAtomStyle(frame);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTarget+AtenWindow::GridsPanelTarget);
}

/*
 * Tools
 */

void AtenWindow::on_TrajectoryToolsMovietButton_clicked(bool checked)
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
