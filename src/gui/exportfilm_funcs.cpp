/*
	*** Export Film Functions
	*** src/gui/exportfilm_funcs.cpp
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
		// Populate encoder combo
		for (EncoderDefinition* encoder = atenWindow_.aten().encoders(); encoder != NULL; encoder = encoder->next) ui.EncodersCombo->addItem(encoder->name(), VariantPointer<EncoderDefinition>(encoder));
		ui.EncodersCombo->addItem("Custom");
	}

	aspectRatio_ = double(ui.FilmWidthSpin->value()) / double(ui.FilmHeightSpin->value());

	firstShow_ = false;

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
 * Output - Images Only
 */

// Enable / Disable relevant controls
void AtenExportFilm::setControlsEnabled(bool imagesOnly)
{
	// Images Only
	ui.ImageBasenameEdit->setEnabled(imagesOnly);
	ui.ImageFormatCombo->setEnabled(imagesOnly);
	ui.SelectBasenameButton->setEnabled(imagesOnly);

	// Film Output
	ui.EncodersCombo->setDisabled(imagesOnly);
	ui.EncoderStepCombo->setDisabled(imagesOnly);
	ui.CommandArgumentsEdit->setDisabled(imagesOnly);
	ui.CommandExecutableEdit->setDisabled(imagesOnly);
	ui.CommandSearchPathsEdit->setDisabled(imagesOnly);
}

void AtenExportFilm::on_ImagesOnlyRadio_clicked(bool checked)
{
	setControlsEnabled(checked);
}

void AtenExportFilm::on_SelectBasenameButton_clicked(bool checked)
{
	QString newFile = QFileDialog::getSaveFileName(this, "Choose image base file name", ui.ImageBasenameEdit->text(), QString(AtenWindow::bitmapFormatFilter((AtenWindow::BitmapFormat) ui.ImageFormatCombo->currentIndex())) + ";;All files (*.*)");
	if (!newFile.isEmpty()) ui.ImageBasenameEdit->setText(newFile);
	updateControls();
}

// Output -- Encoder
void AtenExportFilm::on_FilmRadio_clicked(bool checked)
{
	setControlsEnabled(!checked);
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

/*
 * Output - Film
 */

void AtenExportFilm::on_SaveFilmButton_clicked(bool checked)
{
	// What to do?
	// We will always save the images, regardless of the type of output we're doing - the only difference will be the basename
	QString imageBasename;
	if (ui.ImagesOnlyRadio->isChecked())
	{
		// Construct the image basename based upon the text in the lineedit
		QFileInfo fileInfo(imageBasename);
// 		if (fileInfo.
	}
	else
	{
		
	}

	accept();
}

void AtenExportFilm::on_CancelButton_clicked(bool checked)
{
	reject();
}


// void ScriptMovieWidget::on_SaveScriptedMovieButton_clicked(bool checked)
// {
// 	// First, attempt to generate script from supplied code
// 	Program script;
// 	if (!script.generateFromString(qPrintable(ui.ScriptTextEdit->toPlainText()), "ScriptedMovie", "Scripted Movie Command"))
// 	{
// 		QMessageBox::warning(NULL, "Aten", "Couldn't compile script for movie generation.\nCheck message box for errors.", QMessageBox::Ok, QMessageBox::Ok);
// 		return;
// 	}
// 	
// 	QString geometry;
// 	geometry.sprintf("%ix%i", (int) parent_.ui.MainView->width(), (int) parent_.ui.MainView->height());
// 	int width, height;
// 	
// 	Tree dialog;
// 	TreeGui& ui = dialog.defaultDialog();
// 	ui.setProperty(TreeGuiWidgetEvent::TextProperty, "Movie Options");
// 	ui.addEdit("geometry", "Film Geometry", geometry,1,1);
// 	ui.addIntegerSpin("maxframes", "Maximum Frames", 1, 1e6, 100, 1000 ,1,2);
// 	ui.addIntegerSpin("fps", "Movie FPS", 1, 200, 1, 25 ,1,2);
// 
// 	if (!dialog.defaultDialog().execute()) return;
// 
// 	// Retrieve widget values
// 	geometry = ui.asString("geometry");
// // 	width = atoi(beforeChar(geometry,'x'));	// ATEN2 TODO
// // 	height = atoi(afterChar(geometry,'x'));
// 	if ((width < 1) || (height < 1))
// 	{
// 		QMessageBox::warning(this, "Aten", "The geometry '" + geometry + "' is not valid since one (or both) components are less than 1.", QMessageBox::Ok);
// 		return;
// 	}
// 	int maxframes = ui.asInteger("maxframes");
// 	int fps = ui.asInteger("fps");
// 	
// 	// Get movie filename
// 	static QString selectedFilter("All Files (*.*)");
// 	static QDir currentDirectory(parent_.aten().workDir());
// 	QString filename = QFileDialog::getSaveFileName(this, "Save Scripted Movie", currentDirectory.path(), "All Files (*.*)", &selectedFilter);
// 	if (filename.isEmpty()) return;
// 	// Store path for next use
// 	currentDirectory.setPath(filename);
// 	
// 	// Check that defined encoder exe exists
// 	QFileInfo fileInfo(prefs.encoderExe());
// 	if (!fileInfo.exists())
// 	{
// 		QString message = "Error: Encoder executable doesn't appear to exist ('" + prefs.encoderExe() + "').";
// 		QMessageBox::warning(this, "Aten", message, QMessageBox::Ok);
// 		return;
// 	}
// 	
// 	// Set offscreen rendering and save some current view preferences
// 	bool viewglobe = prefs.viewRotationGlobe();
// 	prefs.setViewRotationGlobe(false);
// 	
// 	// Generate unique file basename and initialise image redirection
// 	int runid;
// 	QString basename;
// 	do
// 	{
// 		runid = AtenMath::randomimax();
// 		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.arc").arg(QApplication::applicationPid(), runid).arg(0, 9, 10, QChar('0'));
// 		fileInfo.setFile(basename);
// 	} while (fileInfo.exists());
// // 	basename.sprintf("%s%caten-movie-%i-%i-%%09i.png", qPrintable(prefs.tempDir()), PATHSEP, parent_.pid(), runid); ATEN2 TODO
// 	parent_.aten().initialiseFilmRedirect(basename, maxframes);
// 	
// 	int progid = progress.initialise("Saving scripted movie frames...", -1);
// 	bool canceled = false;
// 	ReturnValue rv;
// 	script.execute(rv);
// 
// 	progress.terminate(progid);
// 	prefs.setViewRotationGlobe(viewglobe);
// 
// 	// Now run external program to create movie
// 	TProcess encoderProcess;
// 
// 	// Grab encoder command and replace
// 	basename = prefs.tempDir().filePath("aten-movie-%1-%2-*.png").arg(QApplication::applicationPid(), runid);
// 	QString encoderArgs = prefs.encoderArguments();
// 	encoderArgs.replace("OUTPUT", qPrintable(filename));
// 	encoderArgs.replace("FILES", basename);
// 	encoderArgs.replace("FPS", QString::number(fps));
// 	Messenger::print("Command to run will be '%s %s'", qPrintable(prefs.encoderExe()), qPrintable(encoderArgs));
// 	if (!encoderProcess.execute(prefs.encoderExe(), qPrintable(encoderArgs), NULL))
// 	{
// 		Messenger::print("Error: Failed to run encoder command.");
// 		return;
// 	}
// 
// 	// Follow output here...
// 	while (!encoderProcess.finished())
// 	{
// 		// Is output file already present?
// 		while (encoderProcess.outputAvailable()) encoderProcess.printLineToMessages();
// 		QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
// 	}
// 
// 	// Run secondary, post-process command (if one was given)
// 	if (prefs.encoderPostExe() != NULL)
// 	{
// // 		printf("Post encoder command given is [%s]\n", prefs.encoderPostExe());
// 		TProcess postProcess;
// 		// Grab encoder command and replace
// 		QString encoderArgs = prefs.encoderPostArguments();
// 		encoderArgs.replace("OUTPUT", qPrintable(filename));
// 		encoderArgs.replace("FILES", basename);
// 		encoderArgs.replace("FPS", QString::number(fps));
// 		Messenger::print("Command to run will be '%s %s'", qPrintable(prefs.encoderPostExe()), qPrintable(encoderArgs));
// 		if (!postProcess.execute(prefs.encoderPostExe(), encoderArgs, NULL))
// 		{
// 			Messenger::print("Error: Failed to run encoder post-processing command.");
// 		}
// 		else while (!postProcess.finished())
// 		{
// 			// Is output file already present?
// 			while (postProcess.outputAvailable()) postProcess.printLineToMessages();
// 			QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
// 		}
// 	}
// 
// 	// Cancel image redirection and perform cleanup
// 	int nframes = parent_.aten().cancelFilmRedirect();
// 	bool pid = progress.initialise("Cleaning up...", nframes);
// 	for (int n = 0; n < nframes; ++n)
// 	{
// 		basename = prefs.tempDir().filePath("aten-movie-%1-%2-%3.arc").arg(QApplication::applicationPid(), runid).arg(n, 9, 10, QChar('0'));
// 		QFile::remove(basename);
// 		if (!progress.update(pid,n)) break;
// 	}
// 	progress.terminate(pid);
// }

