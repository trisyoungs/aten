/*
	*** Main Window - Trajectory Panel Functions
	*** src/gui/mainwindow_panel_trajectory.cpp
	Copyright T. Youngs 2007-2016

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

	// Get parent model, if there is one...
	bool hasTraj = false, trajSource = false;
	Model* parentModel = sourceModel ? (sourceModel->parent() ? sourceModel->parent() : sourceModel) : NULL;
	if (parentModel)
	{
		hasTraj = parentModel->hasTrajectory();
		trajSource = parentModel->renderSource() == Model::TrajectorySource;
	}

	// Enable / disable controls
	ui.TrajectorySourceOpenButton->setEnabled(!aten_.fileDialogFilters(FilterData::TrajectoryImport).isEmpty());
	ui.TrajectorySourceRemoveButton->setEnabled(parentModel && hasTraj);
	ui.TrajectorySourceFramesButton->setEnabled(parentModel && hasTraj);
	ui.TrajectoryControlFirstButton->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlPreviousButton->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlPlayButton->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlNextButton->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlLastButton->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlFrameSpin->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlFrameSlider->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryControlDelaySpin->setEnabled(parentModel && hasTraj && trajSource);
	ui.TrajectoryStyleInheritButton->setEnabled(parentModel && hasTraj);
	ui.TrajectoryStylePromoteButton->setEnabled(parentModel && hasTraj);
	ui.TrajectoryStylePropagateButton->setEnabled(parentModel && parentModel->trajectoryIsCached());

	if (hasTraj)
	{
		ui.TrajectorySourceFramesButton->setChecked(parentModel->renderSource() == Model::TrajectorySource);
		ui.TrajectoryControlFrameSlider->setRange(1, parentModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSlider->setValue(parentModel->trajectoryFrameIndex()+1);
		ui.TrajectoryControlFrameSpin->setRange(1, parentModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSpin->setValue(parentModel->trajectoryFrameIndex()+1);
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
	Model* currentModel = aten_.currentModel();
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
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;
	
	currentModel->clearTrajectory();

	updateWidgets(AtenWindow::AllTarget);
}

// Switch render focus from the model to the trajectory (or vice versa)
void AtenWindow::on_TrajectorySourceFramesButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current model
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	currentModel->setRenderSource(checked ? Model::TrajectorySource : Model::ModelSource);

	updateWidgets(AtenWindow::AllTarget);
}

/*
 * Control
 */

// Skip to first frame in trajectory
void AtenWindow::on_TrajectoryControlFirstButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	currentModel->seekFirstTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

// Skip to previous frame in trajectory
void AtenWindow::on_TrajectoryControlPreviousButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
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
		editable_ = false;
	}
	else
	{
		killTimer(trajectoryTimerId_);
		trajectoryTimerId_ = -1;
		editable_ = true;
	}
	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::TrajectoryPanelTarget);
}

// Skip to next frame in trajectory
void AtenWindow::on_TrajectoryControlNextButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	currentModel->seekNextTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

// Skip to last frame in trajectory
void AtenWindow::on_TrajectoryControlLastButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	currentModel->seekLastTrajectoryFrame();

	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_TrajectoryControlFrameSpin_valueChanged(int value)
{
	if (refreshing_) return;

	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	currentModel->seekTrajectoryFrame(value-1);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryControlFrameSlider_valueChanged(int position)
{
	if (refreshing_) return;

	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	currentModel->seekTrajectoryFrame(position-1);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
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

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePropagateButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	Model* frame = currentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	currentModel->trajectoryCopyAtomStyle(frame);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePromoteButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModel();
	if (!currentModel) return;

	Model* frame = currentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	currentModel->copyAtomStyle(frame);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

