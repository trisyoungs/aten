/*
	*** Trajectory Actions
	*** src/gui/trajectoryactions.cpp
	Copyright T. Youngs 2007-2011

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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/trajectory.h"
#include "gui/tcanvas.uih"
#include "model/model.h"
#include "base/sysfunc.h"

// Add trajectory to model
void AtenForm::on_actionTrajectoryOpen_triggered(bool checked)
{
	Tree *filter;
	Model *m = aten.currentModel();
	static QDir currentDirectory_(aten.workDir());
	QString selFilter;
	QString filename = QFileDialog::getOpenFileName(this, "Open Trajectory", currentDirectory_.path(), loadTrajectoryFilters, &selFilter);
	if (!filename.isEmpty())
	{
		// Store path for next use
		currentDirectory_.setPath(filename);
		// Find the filter that was selected
		filter = aten.findFilterByDescription(FilterData::TrajectoryImport, qPrintable(selFilter));
		// If filter == NULL then we didn't match a filter, i.e. the 'All files' filter was selected, and we must probe the file first.
		if (filter == NULL) filter = aten.probeFile(qPrintable(filename), FilterData::TrajectoryImport);
		if (filter != NULL)
		{
			m->initialiseTrajectory(qPrintable(filename), filter);
			updateTrajectoryMenu();
		}
		else msg.print( "Couldn't determine trajectory file format.\n");
		gui.update(GuiQt::AllTarget);
	}
}

// Remove associated trajectory to model
void AtenForm::on_actionTrajectoryRemove_triggered(bool checked)
{
	// TGAY TODO
}

// Switch render focus from the model's trajectory to the model.
void AtenForm::on_actionTrajectoryModel_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::ModelSource);
	gui.trajectoryWidget->refresh();
	gui.update(GuiQt::AllTarget);
}

// Switch render focus from the model to the model's trajectory
void AtenForm::on_actionTrajectoryFrames_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::TrajectorySource);
	gui.trajectoryWidget->refresh();
	gui.update(GuiQt::AllTarget);
}

void AtenForm::on_actionTrajectoryFirstFrame_triggered(bool checked)
{
	aten.currentModel()->seekFirstTrajectoryFrame();
	gui.trajectoryWidget->refresh();
	gui.update(GuiQt::AllTarget);
}

void AtenForm::on_actionTrajectoryLastFrame_triggered(bool checked)
{
	aten.currentModel()->seekLastTrajectoryFrame();
	gui.trajectoryWidget->refresh();
	gui.update(GuiQt::AllTarget);
}

void AtenForm::on_actionTrajectoryPlayPause_triggered(bool checked)
{
	gui.trajectoryWidget->ui.TrajectoryPlayPauseButton->setChecked(checked);
// 	gui.update(GuiQt::AllTarget);
}

void AtenForm::on_actionTrajectorySaveMovie_triggered(bool checked)
{
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainWidget->width(), (int) gui.mainWidget->height());
	int width, height;
	
	static Tree dialog("Save Movie","option('Image Size', 'edit', '10x10'); option('First Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Last Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Frame Skip', 'intspin', 0, 9999999, 0, 1, 'newline'); ");

	Model *m = aten.currentModel();

	// Poke values into dialog widgets and execute
	dialog.setWidgetValue("Image Size", ReturnValue(geometry.get()));
	dialog.setWidgetProperty("First Frame", "maximum", m->nTrajectoryFrames());
	dialog.setWidgetProperty("Last Frame", "maximum", m->nTrajectoryFrames());
	dialog.setWidgetProperty("Last Frame", "value", m->nTrajectoryFrames());
	if (!dialog.executeCustomDialog(FALSE)) return;

	// Retrieve widget values
	geometry = dialog.widgetValuec("Image Size");
	width = atoi(beforeChar(geometry,'x'));
	height = atoi(afterChar(geometry,'x'));
	if ((width < 1) || (height < 1))
	{
		Dnchar message(-1, "The geometry '%s' is not valid since one (or both) components are less than 1.\n", geometry.get());
		QMessageBox::warning(this, "Aten", message.get(), QMessageBox::Ok);
		return;
	}
	int firstframe = dialog.widgetValuei("First Frame") - 1;
	int lastframe = dialog.widgetValuei("Last Frame") - 1;
	int frameskip = dialog.widgetValuei("Frame Skip") + 1;

	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Movie", currentDirectory_.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	
	// Store path for next use
	currentDirectory_.setPath(filename);
	bool framemodel = prefs.frameCurrentModel(), frameview = prefs.frameWholeView();
	prefs.setFrameCurrentModel(FALSE);
	prefs.setFrameWholeView(FALSE);
	m->setRenderSource(Model::TrajectorySource);
	// Generate unique file basename
	int runid;
	Dnchar basename;
	do
	{
		runid = AtenMath::randomi(RAND_MAX);
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, 0);
	} while (fileExists(basename));
	
	// Save all frame images
	QPixmap pixmap;
	QImage image;
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / gui.mainWidget->height()) ));
	prefs.setLabelSize(newlabelsize);

	gui.mainWidget->setOffScreenRendering(TRUE);
	aten.initialiseProgress("Save movie frames", lastframe-firstframe);
	bool canceled = FALSE;
	for (int n = firstframe; n <= lastframe; n += frameskip)
	{
		m->seekTrajectoryFrame(n);
		basename.sprintf("%s%caten-movie-%i-%i-%09i.png", prefs.tempDir(), PATHSEP, gui.pid(), runid, n);
		gui.mainWidget->postRedisplay(TRUE);

		if (prefs.useFrameBuffer() == FALSE) pixmap = gui.mainWidget->renderPixmap(width, height, FALSE);
		else
		{
			QImage image = gui.mainWidget->grabFrameBuffer();
			pixmap = QPixmap::fromImage(image);
		}
		pixmap.save(basename.get(), "png", -1);
		if (!aten.updateProgress(n))
		{
			canceled = TRUE;
			msg.print("Canceled.\n");
			break;
		}
	}
	aten.cancelProgress();
	prefs.setFrameCurrentModel(framemodel);
	prefs.setFrameWholeView(frameview);
	gui.mainWidget->setOffScreenRendering(FALSE);
	if (!prefs.reusePrimitiveQuality()) gui.mainWidget->reinitialisePrimitives();

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	if (canceled) return;
	
	// Now run external program to create movie
	TProcess encoderProcess;
	Dnchar encoderCmd(-1, "");
	encodercmd.
	msg.print("Command to run will be '%s'\n", encoderCmd.get());
	if (!encoderProcess.execute(encoderCmd, XXX))
	{
		msg.print("Error: Failed to run encoder command.\n");
		return;
	}

	// Follow output here...
	while (!encoderProcess.finished())
	{
		// Is output file already present?
		XXwhile (encoderProcess.outputAvailable())
		{
			mopacProcess.printLineToMessages();
		}

		gui.processMessages();
	};
}
