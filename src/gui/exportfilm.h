/*
	*** Export Film Dialog
	*** src/gui/exportfilm.h
	Copyright T. Youngs 2013-2017

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

#ifndef ATEN_ATENEXPORTFILM_H
#define ATEN_ATENEXPORTFILM_H

#include "gui/ui_exportfilm.h"
#include "base/encoderdefinition.h"
#include "templates/vector3.h"
#include <QDialog>
#include <QDir>

// Forward Declarations (Qt)
class AtenWindow;

ATEN_USING_NAMESPACE

class AtenExportFilm : public QDialog
{
	// All Qt declarations must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenExportFilm(AtenWindow& parent);
	~AtenExportFilm();
	// Main form declaration
	Ui::AtenExportFilm ui;
	// Reference to main window
	AtenWindow& atenWindow_;

	private:
	// Whether the widget is currently refreshing
	bool refreshing_;
	// Whether the dialog is still awaiting it's first show
	bool firstShow_;
	// Current aspect ratio
	double aspectRatio_;
	// Custom encoder definition
	EncoderDefinition customEncoder_;

	public:
	// Call dialog to get/update image save information
	bool getFilmDetails();
	// Update controls
	void updateControls();


	/*
	 * Definition
	 */
	private slots:
	void on_FilmWidthSpin_valueChanged(int value);
	void on_MaintainAspectRatioCheck_toggled(bool checked);
	void on_FramesPerSecondSpin_valueChanged(int value);

	public:
	// Return film width
	int filmWidth();
	// Return film height
	int filmHeight();
	// Return frames per second
	int fps();


	/*
	 * Source
	 */
	private:
	// Enable / Disable relevant controls
	void setSourceControlsEnabled();

	private slots:
	// View Only
	void on_SourceViewOnlyRadio_clicked(bool checked);
	// Trajectory
	void on_SourceTrajectoryRadio_clicked(bool checked);
	// Vibration
	void on_SourceVibrationRadio_clicked(bool checked);
	// Rotate View
	void on_RotateViewXCheck_clicked(bool checked);
	void on_RotateViewYCheck_clicked(bool checked);
	void on_RotateViewZCheck_clicked(bool checked);	

	public:
	// Return if source is view only
	bool viewSource();
	// Return number of frames to write for view only
	int viewNFrames();
	// Return if source is trajectory
	bool trajectorySource();
	// Return start frame of trajectory to use
	int trajectoryStartFrame();
	// Return end frame of trajectory to use
	int trajectoryEndFrame();
	// Return if source is vibration
	bool vibrationSource();
	// Return if view should be rotated during the course of the film
	bool rotateView();
	// Return view axes flagged for rotation
	Vec3<bool> rotateViewAxes();
	// Return view axes whole rotation flags
	Vec3<bool> rotateViewWhole();
	// Return view rotation deltas
	Vec3<double> rotateViewDeltas();
	// Return view rotation number of whole rotations
	Vec3<double> rotateViewWholeRotations();


	/*
	 * Output
	 */
	private:
	// Enable / Disable relevant controls
	void setOutputControlsEnabled();

	private slots:
	// Output -- Images Only
	void on_OutputImagesOnlyRadio_clicked(bool checked);
	void on_ImagesSelectBasenameButton_clicked(bool checked);
	// Output -- Encoder
	void on_OutputSelectFilenameButton_clicked(bool checked);
	void on_OutputFilmRadio_clicked(bool checked);
	void on_EncodersCombo_currentIndexChanged(int index);
	void on_EncoderStepCombo_currentIndexChanged(int index);

	public:
	// Return if image output only is requested
	bool outputImages();
	// Return image basename
	QString imageBasename();
	// Return image bitmap format extension
	QString imageExtension();
	// Return if film output is requested
	bool outputFilm();
	// Return output filename
	QString outputFilename();
	// Return selected encoder
	EncoderDefinition* encoder();


	/*
	 * Dialog Button
	 */
	private slots:
	// Dialog Buttons
	void on_SaveFilmButton_clicked(bool checked);
	void on_CancelButton_clicked(bool checked);
};

#endif
