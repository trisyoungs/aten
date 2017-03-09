/*
	*** Export Film Functions
	*** src/gui/exportfilm_funcs.cpp
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

#include "gui/exportfilm.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "templates/variantpointer.h"
#include <QMessageBox>
#include <QFileDialog>

// Constructor
AtenExportFilm::AtenExportFilm(AtenWindow& parent) : atenWindow_(parent), QDialog(&parent)
{
	ui.setupUi(this);

	firstShow_ = true;
	refreshing_ = false;

	// Populate format combo
	for (int n=0; n<AtenWindow::nBitmapFormats; ++n) ui.ImageFormatCombo->addItem( AtenWindow::bitmapFormatFilter((AtenWindow::BitmapFormat) n) );
	ui.ImageFormatCombo->setCurrentIndex(AtenWindow::BitmapPNG);

	// Setup bsic custom encoder information
	customEncoder_.setName("Custom Encoder");
}

// Destructor
AtenExportFilm::~AtenExportFilm()
{
}

// Call dialog to get film save information
bool AtenExportFilm::getFilmDetails()
{
	// If this is the first show, set the defaults in the controls
	if (firstShow_)
	{
		// Size
		ui.FilmWidthSpin->setValue(atenWindow_.ui.MainView->contextWidth());
		ui.FilmHeightSpin->setValue(atenWindow_.ui.MainView->contextHeight());
		ui.FilmHeightSpin->setValue(ui.MaintainAspectRatioCheck->checkState() == Qt::Checked ? ui.FilmHeightSpin->value() / aspectRatio_ : ui.FilmHeightSpin->value());
		// Output
		ui.ImageBasenameEdit->setText(atenWindow_.aten().workDir().absoluteFilePath("image"));
		ui.OutputFilenameEdit->setText(atenWindow_.aten().workDir().absoluteFilePath("movie"));
		// Populate encoder combo
		for (EncoderDefinition* encoder = atenWindow_.aten().encoders(); encoder != NULL; encoder = encoder->next) ui.EncodersCombo->addItem(encoder->name(), VariantPointer<EncoderDefinition>(encoder));
		ui.EncodersCombo->addItem(customEncoder_.name(), VariantPointer<EncoderDefinition>(&customEncoder_));
	}

	aspectRatio_ = double(ui.FilmWidthSpin->value()) / double(ui.FilmHeightSpin->value());

	firstShow_ = false;

	// Disable / enable controls based on Model contents
	Model* currentModel = atenWindow_.aten().currentModel();
	if (!currentModel) return false;
	ui.SourceTrajectoryRadio->setEnabled(currentModel->hasTrajectory());
	if (currentModel->hasTrajectory())
	{
		ui.TrajectoryFirstFrameSpin->setRange(1, currentModel->nTrajectoryFrames());
		ui.TrajectoryLastFrameSpin->setRange(1, currentModel->nTrajectoryFrames());
		ui.TrajectoryLastFrameSpin->setValue(currentModel->nTrajectoryFrames());
	}

	setOutputControlsEnabled();
	setSourceControlsEnabled();

	int result = exec();
	return (result == 1);
}

// Update controls
void AtenExportFilm::updateControls()
{
	if (refreshing_) return;
	
	refreshing_ = true;

	// Update the command info for the current encoder
	ui.CommandArgumentsEdit->setEnabled(ui.EncodersCombo->currentIndex() != -1);
	ui.CommandExecutableEdit->setEnabled(ui.EncodersCombo->currentIndex() != -1);
	ui.CommandSearchPathsEdit->setEnabled(ui.EncodersCombo->currentIndex() != -1);
	ExternalCommand* command = (ExternalCommand*) VariantPointer<ExternalCommand>(ui.EncoderStepCombo->currentData());
	if (command)
	{
		ui.CommandArgumentsEdit->setText(command->arguments());
		ui.CommandExecutableEdit->setText(command->executable());
		QString absoluteExe = command->absoluteExecutable();
		QPalette redPalette = ui.CommandArgumentsLabel->palette();
		if (absoluteExe.isEmpty()) redPalette.setColor(QPalette::Text, Qt::red);
		ui.CommandExecutableEdit->setPalette(redPalette);
		ui.CommandSearchPathsEdit->setText(command->searchPaths().join(","));
	}
	else
	{
		ui.CommandArgumentsEdit->clear();
		ui.CommandExecutableEdit->clear();
		ui.CommandSearchPathsEdit->clear();
	}

	refreshing_ = false;
}

// Return film width
int AtenExportFilm::filmWidth()
{
	return ui.FilmWidthSpin->value();
}

// Return film height
int AtenExportFilm::filmHeight()
{
	return ui.FilmHeightSpin->value();
}

// Return frames per second
int AtenExportFilm::fps()
{
	return ui.FramesPerSecondSpin->value();
}

/*
 * Definition
 */

void AtenExportFilm::on_FilmWidthSpin_valueChanged(int value)
{
	if (ui.MaintainAspectRatioCheck->isChecked()) ui.FilmHeightSpin->setValue(value / aspectRatio_);
}

void AtenExportFilm::on_MaintainAspectRatioCheck_toggled(bool checked)
{
	ui.FilmHeightSpin->setDisabled(checked);
	if (checked) ui.FilmHeightSpin->setValue(ui.FilmWidthSpin->value() / aspectRatio_);
}

void AtenExportFilm::on_FramesPerSecondSpin_valueChanged(int value)
{
	updateControls();
}

/*
 * Source
 */

// Enable / disable controls
void AtenExportFilm::setSourceControlsEnabled()
{
	refreshing_ = true;

	// Make sure that, if trajectory or vibration sources are disabled, that they are not checked
	if ((!ui.SourceTrajectoryRadio->isEnabled()) && ui.SourceTrajectoryRadio->isChecked()) ui.SourceViewOnlyRadio->setChecked(true);
	else if ((!ui.SourceVibrationRadio->isEnabled()) && ui.SourceVibrationRadio->isChecked()) ui.SourceViewOnlyRadio->setChecked(true);

	// Trajectory Source
	ui.TrajectoryFirstFrameSpin->setEnabled(ui.SourceTrajectoryRadio->isEnabled() ? ui.SourceTrajectoryRadio->isChecked() : false);
	ui.TrajectoryLastFrameSpin->setEnabled(ui.SourceTrajectoryRadio->isEnabled() ? ui.SourceTrajectoryRadio->isChecked() : false);

	refreshing_ = false;
}

void AtenExportFilm::on_SourceViewOnlyRadio_clicked(bool checked)
{
	setSourceControlsEnabled();
}

void AtenExportFilm::on_SourceTrajectoryRadio_clicked(bool checked)
{
	setSourceControlsEnabled();
}

void AtenExportFilm::on_SourceVibrationRadio_clicked(bool checked)
{
	setSourceControlsEnabled();
}

void AtenExportFilm::on_RotateViewXCheck_clicked(bool checked)
{
	ui.RotateViewXDeltaRadio->setEnabled(checked);
	ui.RotateViewXDeltaSpin->setEnabled(checked);
	ui.RotateViewXWholeRadio->setEnabled(checked);
	ui.RotateViewXWholeSpin->setEnabled(checked);
}

void AtenExportFilm::on_RotateViewYCheck_clicked(bool checked)
{
	ui.RotateViewYDeltaRadio->setEnabled(checked);
	ui.RotateViewYDeltaSpin->setEnabled(checked);
	ui.RotateViewYWholeRadio->setEnabled(checked);
	ui.RotateViewYWholeSpin->setEnabled(checked);
}

void AtenExportFilm::on_RotateViewZCheck_clicked(bool checked)
{
	ui.RotateViewZDeltaRadio->setEnabled(checked);
	ui.RotateViewZDeltaSpin->setEnabled(checked);
	ui.RotateViewZWholeRadio->setEnabled(checked);
	ui.RotateViewZWholeSpin->setEnabled(checked);
}

// Return if source is view manipulaton only
bool AtenExportFilm::viewSource()
{
	return ui.SourceViewOnlyRadio->isChecked();
}

// Return number of frames to write for view only
int AtenExportFilm::viewNFrames()
{
	return ui.ViewNFramesSpin->value();
}

// Return if source is trajectory
bool AtenExportFilm::trajectorySource()
{
	return ui.SourceTrajectoryRadio->isChecked();
}

// Return start frame of trajectory to use
int AtenExportFilm::trajectoryStartFrame()
{
	return ui.TrajectoryFirstFrameSpin->value();
}

// Return end frame of trajectory to use
int AtenExportFilm::trajectoryEndFrame()
{
	return ui.TrajectoryLastFrameSpin->value();
}

// Return if source is vibration
bool AtenExportFilm::vibrationSource()
{
	return ui.SourceVibrationRadio->isChecked();
}

// Return if view should be rotated during the course of the film
bool AtenExportFilm::rotateView()
{
	return ui.ViewRotationGroup->isChecked();
}

// Return view axes flagged for rotation
Vec3<bool> AtenExportFilm::rotateViewAxes()
{
	return Vec3<bool>(ui.RotateViewXCheck->isChecked(), ui.RotateViewYCheck->isChecked(), ui.RotateViewZCheck->isChecked());
}

// Return view axes whole rotation flags
Vec3<bool> AtenExportFilm::rotateViewWhole()
{
	return Vec3<bool>(ui.RotateViewXWholeRadio->isChecked(), ui.RotateViewYWholeRadio->isChecked(), ui.RotateViewZWholeRadio->isChecked());
}

// Return view rotation deltas
Vec3<double> AtenExportFilm::rotateViewDeltas()
{
	return Vec3<double>(ui.RotateViewXDeltaSpin->value(), ui.RotateViewYDeltaSpin->value(), ui.RotateViewZDeltaSpin->value());
}

// Return view rotation number of whole rotations
Vec3<double> AtenExportFilm::rotateViewWholeRotations()
{
	return Vec3<double>(ui.RotateViewXWholeSpin->value(), ui.RotateViewYWholeSpin->value(), ui.RotateViewZWholeSpin->value());
}

/*
 * Output - Images Only
 */

// Enable / Disable relevant output controls
void AtenExportFilm::setOutputControlsEnabled()
{
	// Images Only
	ui.ImageBasenameEdit->setEnabled(ui.OutputImagesOnlyRadio->isChecked());
	ui.ImageFormatCombo->setEnabled(ui.OutputImagesOnlyRadio->isChecked());
	ui.ImagesSelectBasenameButton->setEnabled(ui.OutputImagesOnlyRadio->isChecked());

	// Film Output
	ui.OutputFilenameEdit->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.OutputSelectFilenameButton->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.EncodersCombo->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.EncoderStepCombo->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.CommandArgumentsEdit->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.CommandExecutableEdit->setEnabled(ui.OutputFilmRadio->isChecked());
	ui.CommandSearchPathsEdit->setEnabled(ui.OutputFilmRadio->isChecked());
}

void AtenExportFilm::on_OutputImagesOnlyRadio_clicked(bool checked)
{
	setOutputControlsEnabled();
}

void AtenExportFilm::on_ImagesSelectBasenameButton_clicked(bool checked)
{
	QString newFile = QFileDialog::getSaveFileName(this, "Choose image base file name", ui.ImageBasenameEdit->text(), QString(AtenWindow::bitmapFormatFilter((AtenWindow::BitmapFormat) ui.ImageFormatCombo->currentIndex())) + ";;All files (*.*)");
	if (!newFile.isEmpty()) ui.ImageBasenameEdit->setText(newFile);
	updateControls();
}

/*
 * Output - Film
 */

void AtenExportFilm::on_OutputSelectFilenameButton_clicked(bool checked)
{
	QString newFile = QFileDialog::getSaveFileName(this, "Choose output film file name", ui.OutputFilenameEdit->text(), "All files (*.*)");
	if (!newFile.isEmpty()) ui.OutputFilenameEdit->setText(newFile);
	updateControls();
}

void AtenExportFilm::on_OutputFilmRadio_clicked(bool checked)
{
	setOutputControlsEnabled();
}

void AtenExportFilm::on_EncodersCombo_currentIndexChanged(int index)
{
	// Repopulate steps
	ui.EncoderStepCombo->clear();

	if (index == -1) return;

	EncoderDefinition* definition = (EncoderDefinition*) VariantPointer<EncoderDefinition>(ui.EncodersCombo->currentData());
	if (!definition) return;
	for (ExternalCommand* command = definition->commands(); command != NULL; command = command->next) ui.EncoderStepCombo->addItem(command->name(), VariantPointer<ExternalCommand>(command));
}

void AtenExportFilm::on_EncoderStepCombo_currentIndexChanged(int index)
{
	updateControls();
}

// Return if image output only is requested
bool AtenExportFilm::outputImages()
{
	return ui.OutputImagesOnlyRadio->isChecked();
}

// Return image basename
QString AtenExportFilm::imageBasename()
{
	return ui.ImageBasenameEdit->text();
}

// Return image bitmap format extension
QString AtenExportFilm::imageExtension()
{
	return AtenWindow::bitmapFormatExtension( (AtenWindow::BitmapFormat) ui.ImageFormatCombo->currentIndex() );
}

// Return if film output is requested
bool AtenExportFilm::outputFilm()
{
	return ui.OutputFilmRadio->isChecked();
}

// Return output filename
QString AtenExportFilm::outputFilename()
{
	return ui.OutputFilenameEdit->text();
}

// Return selected encoder
EncoderDefinition* AtenExportFilm::encoder()
{
	EncoderDefinition* definition = (EncoderDefinition*) VariantPointer<EncoderDefinition>(ui.EncodersCombo->currentData());
	return definition;
}

/*
 * Dialog Functions
 */

void AtenExportFilm::on_SaveFilmButton_clicked(bool checked)
{
	accept();
}

void AtenExportFilm::on_CancelButton_clicked(bool checked)
{
	reject();
}
