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

// Add trajectory to model
void AtenForm::on_actionOpenTrajectory_triggered(bool checked)
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
