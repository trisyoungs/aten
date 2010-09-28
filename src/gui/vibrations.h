/*
	*** Qt GUI: Vibrations Window
	*** src/gui/vibrations.h
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

#ifndef ATEN_VIBRATIONSWINDOW_H
#define ATEN_VIBRATIONSWINDOW_H

#include "gui/ui_vibrations.h"

// Vibrations window
class AtenVibrations : public QDialog
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	void showWindow();
	void refresh();
	private slots:
	void dialogFinished(int result);
	void on_VibrationsList_currentRowChanged(int row);
	void on_PlayPauseVibration_clicked(bool checked);
	void on_ShowVectorsCheck_clicked(bool checked);
	private:
	// Refresh displacements table
	void refreshDisplacements();
	protected:
	void timerEvent(QTimerEvent*);


	/*
	// Local variables
	*/
	private:
	// Whether the window is currently refreshing
	bool refreshing_;
	// Whether the window should be refreshed when next shown
	bool shouldRefresh_;
	// Whether the trajectory is currently playing
	bool vibrationPlaying_;
	// ID of rtrajectory timer
	int vibrationTimerId_;
	// Flag to prevent overdrawing
	bool DONTDRAW;

	/*
	// Dialog
	*/
	public:
	// Constructor / Destructor
	AtenVibrations(QWidget *parent = 0, Qt::WindowFlags flags = 0);
	~AtenVibrations();
	// Main form declaration
	Ui::VibrationsDialog ui;
};

#endif
