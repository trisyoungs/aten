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
#include "gui/opentrajectory.h"
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
	ui.TrajectoryManageOpenButton->setEnabled(aten_.pluginStore().nFilePlugins(PluginTypes::TrajectoryFilePlugin, PluginTypes::ImportPlugin) > 0);
	ui.TrajectoryManageRemoveButton->setEnabled(parentModel && hasTraj);
	ui.TrajectoryManageFramesButton->setEnabled(parentModel && hasTraj);
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
	ui.TrajectoryStylePropagateButton->setEnabled(parentModel && parentModel->isTrajectoryCached());

	if (hasTraj)
	{
		ui.TrajectoryManageFramesButton->setChecked(parentModel->renderSource() == Model::TrajectorySource);
		ui.TrajectoryControlFrameSlider->setRange(1, parentModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSlider->setValue(parentModel->trajectoryFrameIndex()+1);
		ui.TrajectoryControlFrameSpin->setRange(1, parentModel->nTrajectoryFrames());
		ui.TrajectoryControlFrameSpin->setValue(parentModel->trajectoryFrameIndex()+1);
		ui.TrajectoryStyleInheritButton->setCheckable((!parentModel->isTrajectoryCached()) && parentModel->trajectoryPlugin());
	}

	Messenger::exit("AtenWindow::updateTrajectoryPanel");
}

// Stop trajectory playback
void AtenWindow::stopTrajectoryPlayback()
{
	if (ui.TrajectoryControlPlayButton->isChecked())
	{
		ui.TrajectoryControlPlayButton->click();
		updateWidgets(AtenWindow::TrajectoryPanelTarget);
	}
}

/*
 * Switch
 */

void AtenWindow::on_TrajectoryManageOpenButton_clicked(bool checked)
{
	// Stop playback before we do anything
	stopTrajectoryPlayback();

	// Get current model
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	if (openTrajectoryDialog_.execute(aten_.pluginStore().logPoint()))
	{
		// Open model(s) selected in dialog
		QStringList filesToLoad = openTrajectoryDialog_.selectedFilenames();
		FilePluginInterface* plugin = openTrajectoryDialog_.selectedPlugin();
		aten_.importTrajectory(parentModel, filesToLoad.at(0), plugin, openTrajectoryDialog_.standardImportOptions());

		updateWidgets(AtenWindow::AllTargets);
	}
}

void AtenWindow::on_TrajectoryManageRemoveButton_clicked(bool checked)
{
	// Stop playbac before we do anything
	stopTrajectoryPlayback();

	// Get current model
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;
	
	parentModel->clearTrajectory();

	updateWidgets(AtenWindow::AllTargets);
}

// Switch render focus from the model to the trajectory (or vice versa)
void AtenWindow::on_TrajectoryManageFramesButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get current model
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	parentModel->setRenderSource(checked ? Model::TrajectorySource : Model::ModelSource);

	updateWidgets(AtenWindow::AllTargets);
}

/*
 * Control
 */

// Skip to first frame in trajectory
void AtenWindow::on_TrajectoryControlFirstButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	parentModel->seekFirstTrajectoryFrame();

	updateWidgets(AtenWindow::AllTargets);
}

// Skip to previous frame in trajectory
void AtenWindow::on_TrajectoryControlPreviousButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	parentModel->seekPreviousTrajectoryFrame();

	updateWidgets(AtenWindow::AllTargets);
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
	updateWidgets(AtenWindow::TrajectoryPanelTarget);
}

// Skip to next frame in trajectory
void AtenWindow::on_TrajectoryControlNextButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	parentModel->seekNextTrajectoryFrame();

	updateWidgets(AtenWindow::AllTargets);
}

// Skip to last frame in trajectory
void AtenWindow::on_TrajectoryControlLastButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	parentModel->seekLastTrajectoryFrame();

	updateWidgets(AtenWindow::AllTargets);
}

void AtenWindow::on_TrajectoryControlFrameSpin_valueChanged(int value)
{
	if (refreshing_) return;

	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	parentModel->seekTrajectoryFrame(value-1);

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryControlFrameSlider_valueChanged(int position)
{
	if (refreshing_) return;

	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	parentModel->seekTrajectoryFrame(position-1);

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

/*
 * Style
 */

void AtenWindow::on_TrajectoryStyleInheritButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;
	if (parentModel->nTrajectoryFrames() == 0) return;

	// Exactly what we do here depends on if the trajectory is cached or not
	if (parentModel->isTrajectoryCached())
	{
		// It is cached, so apply the parent model's style to all the cached frames
		parentModel->trajectoryCopyAtomStyle(parentModel);
	}
	else
	{
		// Not cached - which means the button is checkable
		if (parentModel->trajectoryPlugin()) parentModel->trajectoryPlugin()->setStandardOption(FilePluginStandardImportOptions::InheritStyleSwitch, checked);

		// If checked, apply the style to the current frame - if not, reload it
		if (checked) parentModel->trajectoryCurrentFrame()->setDrawStyle(prefs.defaultDrawStyle());
		else parentModel->trajectoryCopyAtomStyle(parentModel);
	}

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePropagateButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	Model* frame = parentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	parentModel->trajectoryCopyAtomStyle(frame);

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

void AtenWindow::on_TrajectoryStylePromoteButton_clicked(bool checked)
{
	Model* parentModel = aten_.currentModel();
	if (!parentModel) return;

	Model* frame = parentModel->trajectoryCurrentFrame();
	if (frame == NULL) return;

	parentModel->copyAtomStyle(frame);

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::GridsPanelTarget);
}

