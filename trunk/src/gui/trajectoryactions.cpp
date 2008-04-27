/*
	*** Qt trajectory actions
	*** src/gui/action_funcs.cpp
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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

/*
// Trajectory Actions
*/

void AtenForm::on_actionFrameNext_triggered(bool checked)
{
	master.currentModel()->seekNextFrame();
	master.currentModel()->renderSource()->logChange(Change::CameraLog);
	gui.modelChanged(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionFramePrevious_triggered(bool checked)
{
	master.currentModel()->seekPreviousFrame();
	master.currentModel()->renderSource()->logChange(Change::CameraLog);
	gui.modelChanged(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionFrameFirst_triggered(bool checked)
{
	master.currentModel()->seekFirstFrame();
	master.currentModel()->renderSource()->logChange(Change::CameraLog);
	gui.modelChanged(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionFrameLast_triggered(bool checked)
{
	master.currentModel()->seekLastFrame();
	master.currentModel()->renderSource()->logChange(Change::CameraLog);
	gui.modelChanged(TRUE,TRUE,FALSE);
}

void AtenForm::on_actionPlayPause_triggered(bool checked)
{
	// If button is depressed, begin playback
	if (checked)
	{
		gui.setTrajectoryTimerId(ui.ViewFrame->startTimer(100));
		gui.setTrajectoryPlaying(TRUE);
	}
	else
	{
		ui.ViewFrame->killTimer(gui.trajectoryTimerId());
		gui.setTrajectoryPlaying(FALSE);
	}
	gui.updateTrajControls();
}
