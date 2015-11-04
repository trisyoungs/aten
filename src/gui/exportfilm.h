/*
	*** Export Film Dialog
	*** src/gui/exportfilm.h
	Copyright T. Youngs 2013-2015

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
// 	// Return selected filename
// 	QString fileName();


	/*
	 * Slots
	 */
	private:
	// Enable / Disable relevant controls
	void setOutputControlsEnabled();
	void setSourceControlsEnabled();

	private slots:
	// Definition Group
	void on_FilmWidthSpin_valueChanged(int value);
	void on_MaintainAspectRatioCheck_toggled(bool checked);
	void on_FramesPerSecondSpin_valueChanged(int value);
	// Source -- View Only
	void on_SourceViewOnlyRadio_clicked(bool checked);
	// Source -- Trajectory
	void on_SourceTrajectoryRadio_clicked(bool checked);
	// Source -- Vibration
	void on_SourceVibrationRadio_clicked(bool checked);
	// Output -- Images Only
	void on_OutputImagesOnlyRadio_clicked(bool checked);
	void on_ImagesSelectBasenameButton_clicked(bool checked);
	// Output -- Encoder
	void on_OutputFilmRadio_clicked(bool checked);
	void on_EncodersCombo_currentIndexChanged(int index);
	void on_EncoderStepCombo_currentIndexChanged(int index);
	// Dialog Buttons
	void on_SaveFilmButton_clicked(bool checked);
	void on_CancelButton_clicked(bool checked);
};

#endif
