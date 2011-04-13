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
#include "gui/tprocess.uih"
#include "model/model.h"
#include "parser/commandnode.h"
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
	Model *m = aten.currentModel();
	// Set view to be the parent model before we do anything
	ui.actionTrajectoryModel->trigger();
	m->clearTrajectory();
	gui.update();
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
	static Dnchar geometry(-1,"%ix%i", (int) gui.mainWidget()->width(), (int) gui.mainWidget()->height());
	int width, height;
	
	static Tree dialog("Save Movie","option('Image Size', 'edit', '10x10'); option('First Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Last Frame', 'intspin', 1, 1, 1, 1, 'newline'); option('Frame Interval', 'intspin', 1, 9999999, 0, 1, 'newline'); option('Movie FPS', 'intspin', 1, 100, 25, 1, 'newline'); ");

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
	int frameskip = dialog.widgetValuei("Frame Interval");
	int fps = dialog.widgetValuei("Movie FPS");
	
	// Get movie filename
	static QString selectedFilter("All Files (*.*)");
	static QDir currentDirectory_(aten.workDir());
	QString filename = QFileDialog::getSaveFileName(this, "Save Movie", currentDirectory_.path(), "All Files (*.*)", &selectedFilter);
	if (filename.isEmpty()) return;
	// Store path for next use
	currentDirectory_.setPath(filename);
	
	// Generate movie file...
	CommandNode::run(Command::SaveMovie, "ciiiiiii", qPrintable(filename), width, height, -1, firstframe, lastframe, frameskip, fps);
}

void AtenForm::on_actionTrajectoryInheritParentStyle_triggered(bool checked)
{
	if (!checked) return;
	// If a trajectory is already associated, change its style now
	Model *m = aten.currentModel();
	if (m->nTrajectoryFrames() == 0) return;
	else m->trajectoryCopyAtomStyle(m);
}

void AtenForm::on_actionTrajectoryCopyStyleToParent_triggered(bool checked)
{
	Model *m = aten.currentModel();
	Model *frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->copyAtomStyle(frame);
}

void AtenForm::on_actionTrajectoryPropagateStyleFromHere_triggered(bool checked)
{
	
	Model *m = aten.currentModel();
	Model *frame = m->trajectoryCurrentFrame();
	if ((m == NULL) || (frame == NULL)) return;
	m->trajectoryCopyAtomStyle(frame);
}
