/*
	*** Vibrations Dock Widget
	*** src/gui/vibrations.h
	Copyright T. Youngs 2007-2017

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

#ifndef ATEN_VIBRATIONSWIDGET_H
#define ATEN_VIBRATIONSWIDGET_H

#include "gui/ui_vibrations.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class TreeGui;
class TreeGuiWidget;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Vibrations window
class VibrationsWidget : public QDockWidget
{
	// All Qt declarations derived from QObject must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	VibrationsWidget(AtenWindow& parent, Qt::WindowFlags flags = 0);
	// Main form declaration
	Ui::VibrationsWidget ui;

	private:
	// Reference to main window
	AtenWindow& parent_;


	/*
	 * Local variables
	 */
	private:
	// Whether the window is currently refreshing
	bool refreshing_;
	// Whether the trajectory is currently playing
	bool vibrationPlaying_;
	// ID of rtrajectory timer
	int vibrationTimerId_;
	// Flag to prevent overdrawing
	bool DONTDRAW;


	/*
	 * Window Functions
	 */
	public:
	void showWidget();
	void refresh();
	private slots:
	void on_VibrationsList_currentRowChanged(int row);
	void on_PlayPauseVibration_clicked(bool checked);
	void on_ShowVectorsCheck_clicked(bool checked);
	void on_VectorScaleSpin_valueChanged(double value);
	void on_FrameSlider_valueChanged(int value);
	void on_DelaySpin_valueChanged(int value);
	void on_SaveImageButton_clicked(bool checked);
	void on_SaveMovieButton_clicked(bool checked);
	private:
	// Refresh displacements table
	void refreshDisplacements();
	// Stop current timer (if any)
	void stopTimer();
	// (Re)start timer event with specified delay
	void resetTimer(int delay);
	protected:
	void timerEvent(QTimerEvent*);
	void closeEvent(QCloseEvent* event);
};

#endif
