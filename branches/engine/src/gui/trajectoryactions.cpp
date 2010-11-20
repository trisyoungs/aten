/*
	*** Qt trajectory actions
	*** src/gui/action_funcs.cpp
	Copyright T. Youngs 2007-2010

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
#include "gui/tcanvas.uih"
#include "model/model.h"

/*
// Trajectory Actions
*/

void AtenForm::on_actionTrajectoryViewTrajectory_triggered(bool checked)
{
	// Switch render focus from the model to the trajectory (or vice versa)
	if (checked) aten.currentModel()->setRenderSource(Model::TrajectorySource);
	else aten.currentModel()->setRenderSource(Model::ModelSource);
	Model *m = aten.currentModelOrFrame();
	m->calculateViewMatrix();
	m->changeLog.add(Log::Camera);
	gui.update(TRUE, TRUE, FALSE, TRUE, TRUE);
}

void AtenForm::on_actionTrajectoryNextFrame_triggered(bool checked)
{
	aten.currentModel()->seekNextTrajectoryFrame();
	aten.currentModelOrFrame()->changeLog.add(Log::Camera);
	gui.update(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionTrajectoryPreviousFrame_triggered(bool checked)
{
	aten.currentModel()->seekPreviousTrajectoryFrame();
	aten.currentModelOrFrame()->changeLog.add(Log::Camera);
	gui.update(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionTrajectoryFirstFrame_triggered(bool checked)
{
	aten.currentModel()->seekFirstTrajectoryFrame();
	aten.currentModelOrFrame()->changeLog.add(Log::Camera);
	gui.update(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionTrajectoryLastFrame_triggered(bool checked)
{
	aten.currentModel()->seekLastTrajectoryFrame();
	aten.currentModelOrFrame()->changeLog.add(Log::Camera);
	gui.update(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionTrajectoryPlayPause_triggered(bool checked)
{
	// If button is depressed, begin playback
	if (checked)
	{
		gui.setTrajectoryTimerId(gui.mainWidget->startTimer(100));
		gui.setTrajectoryPlaying(TRUE);
		gui.mainWidget->setEditable(FALSE);
	}
	else
	{
		gui.mainWidget->killTimer(gui.trajectoryTimerId());
		gui.setTrajectoryPlaying(FALSE);
		gui.mainWidget->setEditable(TRUE);
	}
	updateTrajectoryControls();
}

void AtenForm::trajectorySlider_sliderMoved(int i)
{
	if (trajectoryToolbarRefreshing_) return;
	trajectoryToolbarRefreshing_ = TRUE;
	// Slider range is from 1-NFrames, so pass (N-1) to the seekFrame function
	aten.current.m->seekTrajectoryFrame(i-1);
	// Set corresponding value in Spin control
	trajectorySpin_->setValue(i);
	trajectoryToolbarRefreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}

void AtenForm::trajectorySpin_valueChanged(int i)
{
	if (trajectoryToolbarRefreshing_) return;
	trajectoryToolbarRefreshing_ = TRUE;
	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	aten.current.m->seekTrajectoryFrame(i-1);
	// Set corresponding value in Spin control
	trajectorySlider_->setValue(i);
	trajectoryToolbarRefreshing_ = FALSE;
	gui.mainWidget->postRedisplay();
}
