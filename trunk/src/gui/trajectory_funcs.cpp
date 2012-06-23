/*
	*** Trajectory Dock Widget Functions
	*** src/gui/trajectory_funcs.cpp
	Copyright T. Youngs 2007-2012

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

#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/toolbox.h"
#include "gui/trajectory.h"
#include "main/aten.h"

// Constructor
TrajectoryWidget::TrajectoryWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
	QObject::connect(this, SIGNAL(dockLocationChanged(Qt::DockWidgetArea)), this, SLOT(widgetLocationChanged(Qt::DockWidgetArea)));
	
	// Private variables
	refreshing_ = FALSE;
	DONTDRAW = FALSE;
	trajectoryPlaying_ = FALSE;
	trajectoryTimerId_ = -1;
}

void TrajectoryWidget::showWidget()
{
	refresh();
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.TrajectoryButton->setChecked(TRUE);
}

// Refresh
void TrajectoryWidget::refresh()
{
	if (refreshing_) return;
	refreshing_ = TRUE;
	Model *m = aten.currentModel();
	bool hastrj = m->nTrajectoryFrames() > 0;
	ui.ControlsWidget->setEnabled(hastrj);
	ui.FrameSelectWidget->setEnabled(hastrj);
	if (hastrj)
	{
		ui.TrajectoryFrameSlider->setRange(1, m->nTrajectoryFrames());
		ui.TrajectoryFrameSlider->setValue(m->trajectoryFrameIndex()+1);
		ui.TrajectoryFrameSpin->setRange(1, m->nTrajectoryFrames());
		ui.TrajectoryFrameSpin->setValue(m->trajectoryFrameIndex()+1);
	}
	else
	{
		ui.TrajectoryFrameSlider->setRange(1, 1);
		ui.TrajectoryFrameSlider->setValue(1);
		ui.TrajectoryFrameSpin->setRange(1, 1);
		ui.TrajectoryFrameSpin->setValue(1);
	}
	refreshing_ = FALSE;
}

// Switch render focus from the model to the trajectory (or vice versa)
void TrajectoryWidget::on_TrajectorySwitchButton_clicked(bool checked)
{
	if (checked) aten.currentModel()->setRenderSource(Model::TrajectorySource);
	else aten.currentModel()->setRenderSource(Model::ModelSource);
	gui.update(GuiQt::AllTarget);
}

// Skip to next frame in trajectory
void TrajectoryWidget::on_TrajectoryNextFrameButton_clicked(bool checked)
{
	aten.currentModel()->seekNextTrajectoryFrame();
	gui.update(GuiQt::AllTarget);
}

// Skip to previous frame in trajectory
void TrajectoryWidget::on_TrajectoryPreviousFrameButton_clicked(bool checked)
{
	aten.currentModel()->seekPreviousTrajectoryFrame();
	gui.update(GuiQt::AllTarget);
}

// Skip to first frame in trajectory
void TrajectoryWidget::on_TrajectoryFirstFrameButton_clicked(bool checked)
{
	aten.currentModel()->seekFirstTrajectoryFrame();
	gui.update(GuiQt::AllTarget);
}

// Skip to last frame in trajectory
void TrajectoryWidget::on_TrajectoryLastFrameButton_clicked(bool checked)
{
	aten.currentModel()->seekLastTrajectoryFrame();
	gui.update(GuiQt::AllTarget);
}

// Play/pause trajectory playback
void TrajectoryWidget::on_TrajectoryPlayPauseButton_clicked(bool checked)
{
	// If button is depressed, begin playback
	if (checked)
	{
		trajectoryTimerId_ = startTimer(ui.TrajectoryDelaySpin->value());
		trajectoryPlaying_ = TRUE;
		gui.mainCanvas()->setEditable(FALSE);
	}
	else
	{
		killTimer(trajectoryTimerId_);
		trajectoryPlaying_ = FALSE;
		gui.mainCanvas()->setEditable(TRUE);
	}
	gui.mainWindow()->updateTrajectoryMenu();
}

// Frame position slider adjusted
void TrajectoryWidget::on_TrajectoryFrameSlider_valueChanged(int value)
{
	if (refreshing_) return;
	refreshing_ = TRUE;
	// Slider range is from 1-NFrames, so pass (N-1) to the seekFrame function
	aten.current.m->seekTrajectoryFrame(value-1);
	// Set corresponding value in Spin control
// 	trajectorySpin_->setValue(value);
	refreshing_ = FALSE;
	gui.mainCanvas()->postRedisplay();
}

// Frame spinbox value adjusted
void TrajectoryWidget::on_TrajectoryFrameSpin_valueChanged(int value)
{
	if (refreshing_) return;
	refreshing_ = TRUE;
	// Slider range is from 1-NFrames, so pass (N-1) to the seekTrajectoryFrame function
	aten.current.m->seekTrajectoryFrame(value-1);
	// Set corresponding value in Spin control
// 	trajectorySlider_->setValue(value);
	refreshing_ = FALSE;
	gui.mainCanvas()->postRedisplay();
}

void TrajectoryWidget::timerEvent(QTimerEvent *event)
{
	// Move on to the next frame in the trajectory
	// Check that we're not still drawing the last frame from the last timerEvent
	if (DONTDRAW) printf("Still drawing previous frame.\n");
	else
	{
		DONTDRAW = TRUE;
		Model *m = aten.currentModel();
		m->seekNextTrajectoryFrame();
		if (m->trajectoryFrameIndex() == m->nTrajectoryFrames()-1) ui.TrajectoryPlayPauseButton->click();
		gui.update(GuiQt::CanvasTarget);
		DONTDRAW = FALSE;
	}
}

// Location of dock widget changed - change layout to reflect
void TrajectoryWidget::widgetLocationChanged(Qt::DockWidgetArea area)
{
	// If in the top or bottom dock areas, set the layout to be horizontal, otherwise make it vertical
	if (isFloating() || (area == Qt::LeftDockWidgetArea) || (area == Qt::RightDockWidgetArea)) ui.VerticalLayout->addWidget(ui.FrameSelectWidget, 0);
	else ui.HorizontalLayout->addWidget(ui.FrameSelectWidget, 10);
}

// Window closed
void TrajectoryWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.TrajectoryButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainCanvas()->postRedisplay();
	event->accept();
}

// Stop trajectory playback
void TrajectoryWidget::stopTrajectoryPlayback()
{
	if (ui.TrajectoryPlayPauseButton->isChecked())
	{
		ui.TrajectoryPlayPauseButton->click();
		refresh();
	}
}
