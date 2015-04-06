/*
	*** Trajectory Dock Widget
	*** src/gui/trajectory.h
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

#ifndef ATEN_TRAJECTORYWIDGET_H
#define ATEN_TRAJECTORYWIDGET_H

#include "gui/ui_trajectory.h"

// Forward Declarations (Qt)
class AtenWindow;

class TrajectoryWidget : public QWidget
{
	// All Qt declarations must include this macro
	Q_OBJECT

	public:
	// Constructor
	TrajectoryWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::TrajectoryWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local Variables
	 */
	private:
	// Whether widget is currently refreshing
	bool refreshing_;
	// Whether the trajectory is currently playing
	bool trajectoryPlaying_;
	// ID of rtrajectory timer
	int trajectoryTimerId_;
	// Flag to prevent overdrawing
	bool DONTDRAW;

	public:
	// Stop trajectory playback
	void stopTrajectoryPlayback();


	/*
	 * Window Functions
	 */
	public:
	void showWidget();
	void refresh();
	private slots:
	void on_TrajectorySwitchButton_clicked(bool checked);
	void on_TrajectoryFirstFrameButton_clicked(bool checked);
	void on_TrajectoryPreviousFrameButton_clicked(bool checked);
	void on_TrajectoryPlayPauseButton_clicked(bool checked);
	void on_TrajectoryNextFrameButton_clicked(bool checked);
	void on_TrajectoryLastFrameButton_clicked(bool checked);
	void on_TrajectoryFrameSlider_valueChanged(int value);
	void on_TrajectoryFrameSpin_valueChanged(int value);
	void widgetLocationChanged(Qt::DockWidgetArea area); 
	protected:
	void closeEvent(QCloseEvent* event);
	void timerEvent(QTimerEvent* event);
	void hideEvent(QHideEvent* event);
};

#endif
