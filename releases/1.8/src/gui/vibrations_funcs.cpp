/*
	*** Vibrations Dock Widget
	*** src/gui/vibrations_funcs.cpp
	Copyright T. Youngs 2007-2013

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
#include "gui/mainwindow.h"
#include "gui/vibrations.h"
#include "gui/toolbox.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Constructor
VibrationsWidget::VibrationsWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	// Private variables
	refreshing_ = FALSE;
	vibrationTimerId_ = -1;
	vibrationPlaying_ = FALSE;
	DONTDRAW = FALSE;

	ui.setupUi(this);
}

// Show window
void VibrationsWidget::showWidget()
{
	show();
	refresh();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.VibrationsButton->setChecked(TRUE);
}

// Refresh window contents
void VibrationsWidget::refresh()
{
	msg.enter("VibrationsWidget::refresh");
	refreshing_ = TRUE;
	Model *m = aten.currentModelOrFrame();
	Dnchar text;
	ui.VibrationsList->clear();
	ui.DisplacementsTable->clear();
	int count = 0;
	for (Vibration *vib = m->vibrations(); vib != NULL; vib = vib->next)
	{
		text.sprintf("%i. Freq=%f\n", ++count, vib->frequency());
		ui.VibrationsList->addItem(text.get());
	}
	ui.VibrationsList->setCurrentRow(0);
	// Disable widgets if there are no vibrations loaded
	if (count == 0)
	{
		m->setRenderFromVibration(FALSE);
		ui.PlayPauseVibration->setEnabled(FALSE);
		ui.FrameSlider->setEnabled(FALSE);
		ui.SaveImageButton->setEnabled(FALSE);
		ui.SaveMovieButton->setEnabled(FALSE);
	}
	else
	{
		m->setRenderFromVibration(TRUE);
		ui.PlayPauseVibration->setEnabled(TRUE);
		ui.FrameSlider->setEnabled(TRUE);
		ui.SaveImageButton->setEnabled(TRUE);
		ui.SaveMovieButton->setEnabled(TRUE);
	
		Model *m = aten.currentModelOrFrame();
		m->generateVibration(ui.VibrationsList->currentRow(), 20);
		m->setVibrationFrameIndex(ui.FrameSlider->value()-1);
	}
	ui.VibrationsTabWidget->setDisabled(count == 0);
	refreshing_ = FALSE;
	refreshDisplacements();
	msg.exit("VibrationsWidget::refresh");
}

// Refresh displacement data
void VibrationsWidget::refreshDisplacements()
{
	if (refreshing_) return;
	ui.DisplacementsTable->clear();
	ui.DisplacementsTable->setHorizontalHeaderLabels(QStringList() << "Atom" << "dX" << "dY" << "dZ" );
	int row = ui.VibrationsList->currentRow();
	if (row == -1) return;
	Model *m = aten.currentModelOrFrame();
	Vibration *vib = m->vibration(row);
	Vec3<double> *displacements = vib->displacements();
	QTableWidgetItem *item;
	ui.DisplacementsTable->setRowCount(vib->nDisplacements());
	int n;
	Atom *i = m->atoms();
	for (n=0; n<vib->nDisplacements(); ++n)
	{
		if (i == NULL) msg.print("Warning - More displacements defined in Vibration than there are atoms in the parent model.\n");
		item = new QTableWidgetItem();
		item->setText(i == NULL ? "NULL" : elements().symbol(i));
		ui.DisplacementsTable->setItem(n, 0, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].x));
		ui.DisplacementsTable->setItem(n, 1, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].y));
		ui.DisplacementsTable->setItem(n, 2, item);
		item = new QTableWidgetItem();
		item->setText(ftoa(displacements[n].z));
		ui.DisplacementsTable->setItem(n, 3, item);
		i = i->next;
	}
	for (n=0; n<4; ++n) ui.DisplacementsTable->resizeColumnToContents(n);
}

void VibrationsWidget::on_VibrationsList_currentRowChanged(int row)
{
	if (refreshing_) return;
	refreshDisplacements();
	if (ui.ShowVectorsCheck->isChecked()) gui.mainCanvas()->postRedisplay();
	// Regenerate vibration trajectory
	// Stop current timer (if playing) - we'll restart it afterwards
	bool wasplaying = ui.PlayPauseVibration->isChecked();
	if (wasplaying) stopTimer();
	Model *m = aten.currentModelOrFrame();
	m->generateVibration(ui.VibrationsList->currentRow(), 20);
	m->setVibrationFrameIndex(ui.FrameSlider->value()-1);
	if (wasplaying) resetTimer(ui.DelaySpin->value());
}

void VibrationsWidget::on_ShowVectorsCheck_clicked(bool checked)
{
	gui.mainCanvas()->postRedisplay();
}

void VibrationsWidget::on_VectorScaleSpin_valueChanged(double value)
{
	gui.mainCanvas()->postRedisplay();
}

void VibrationsWidget::on_PlayPauseVibration_clicked(bool checked)
{
	if (checked)
	{
		vibrationPlaying_ = TRUE;
		this->setEnabled(TRUE);
		Model *m = aten.currentModelOrFrame();
		ui.FrameSlider->setEnabled(FALSE);
		ui.SaveImageButton->setEnabled(FALSE);
		ui.SaveMovieButton->setEnabled(FALSE);
		resetTimer(ui.DelaySpin->value());
	}
	else
	{
		vibrationPlaying_ = FALSE;
		this->killTimer(vibrationTimerId_);
		vibrationTimerId_ = -1;
		Model *m = aten.currentModelOrFrame();
		ui.FrameSlider->setEnabled(TRUE);
		ui.SaveImageButton->setEnabled(TRUE);
		ui.SaveMovieButton->setEnabled(TRUE);
		gui.mainCanvas()->postRedisplay();
	}
}

void VibrationsWidget::on_FrameSlider_valueChanged(int value)
{
	if (vibrationPlaying_) return;
	Model *m = aten.currentModelOrFrame();
	m->setVibrationFrameIndex(ui.FrameSlider->value()-1);
	gui.mainCanvas()->postRedisplay();
}

void VibrationsWidget::on_DelaySpin_valueChanged(int value)
{
	if (vibrationTimerId_ == -1) return;
	resetTimer(value);
}

void VibrationsWidget::on_SaveImageButton_clicked(bool checked)
{
	gui.mainWindow()->ui.actionFileSaveImage->trigger();
}

void VibrationsWidget::on_SaveMovieButton_clicked(bool checked)
{
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainCanvas()->width(), (int) gui.mainCanvas()->height());
	int width, height;
	
	Tree dialog;
	TreeGui &dialogui = dialog.defaultDialog();
	dialogui.setProperty(TreeGuiWidgetEvent::TextProperty, "Vibration Movie Options");
	dialogui.addEdit("geometry", "Image Geometry", geometry,1,1);
	dialogui.addIntegerSpin("cycles", "Number of Cycles", 1, 1000, 1, 10 ,1,2);
	dialogui.addIntegerSpin("fps", "Movie FPS", 1, 200, 1, 25 ,1,3);

	if (!dialog.defaultDialog().execute()) return;
	
	// Retrieve widget values
	geometry = dialogui.asCharacter("geometry");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int ncycles = dialogui.asInteger("cycles");
	int fps = dialogui.asInteger("fps");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Movie", currentDirectory_.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	// Store path for next use
	currentDirectory_.setPath(filename);
	
	// Generate movie file...
	CommandNode::run(Command::SaveVibrationMovie, "ciiiiiii", qPrintable(filename), width, height, -1, ui.VibrationsList->currentRow()+1, 20, ncycles, fps);
}

// Stop current timer (if any)
void VibrationsWidget::stopTimer()
{
	if (vibrationTimerId_ != -1) this->killTimer(vibrationTimerId_);
	vibrationTimerId_ = -1;
}

// (Re)start timer event with specified delay
void VibrationsWidget::resetTimer(int delay)
{
	// If a timer currently exists, kill it first
	stopTimer();
	vibrationTimerId_ = this->startTimer(delay);
}

void VibrationsWidget::timerEvent(QTimerEvent*)
{
	if (DONTDRAW) msg.print(Messenger::Verbose, "VibrationsWidget - Still drawing previous frame...\n");
	else
	{
		DONTDRAW = TRUE;
		Model *m = aten.currentModelOrFrame();
		m->vibrationNextFrame();
		ui.FrameSlider->setValue(m->vibrationFrameIndex()+1);
		gui.mainCanvas()->postRedisplay();
		DONTDRAW = FALSE;
	}
}

void VibrationsWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	Model *m = aten.currentModelOrFrame();
	m->setRenderFromVibration(FALSE);
	gui.toolBoxWidget->ui.VibrationsButton->setChecked(FALSE);
	gui.setInteractive(TRUE);
	gui.mainCanvas()->postRedisplay();
	event->accept();
}
