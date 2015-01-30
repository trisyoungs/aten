/*
	*** ToolBox Dock Widget Functions
	*** src/gui/toolbox_funcs.cpp
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

#include "gui/gui.h"
#include "gui/atomlist.h"
#include "gui/build.h"
#include "gui/celltransform.h"
#include "gui/celldefinition.h"
#include "gui/command.h"
#include "gui/disorderwizard.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/mainwindow.h"
#include "gui/md.h"
#include "gui/messages.h"
#include "gui/modellist.h"
#include "gui/pores.h"
#include "gui/position.h"
#include "gui/scriptmovie.h"
#include "gui/select.h"
#include "gui/toolbox.h"
#include "gui/trajectory.h"
#include "gui/transform.h"
#include "gui/vibrations.h"
#include "gui/zmatrix.h"

// Constructor
ToolBoxWidget::ToolBoxWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Update all buttons in the toolbox
void ToolBoxWidget::updateButtons()
{
	ui.AtomListButton->setChecked(gui.atomListWidget->isVisible());
	ui.BuildButton->setChecked(gui.buildWidget->isVisible());
	ui.CellDefinitionButton->setChecked(gui.cellDefinitionWidget->isVisible());
	ui.CellTransformButton->setChecked(gui.cellTransformWidget->isVisible());
	ui.CommandButton->setChecked(gui.commandWidget->isVisible());
	ui.ForcefieldsButton->setChecked(gui.forcefieldsWidget->isVisible());
	ui.FragmentsButton->setChecked(gui.fragmentsWidget->isVisible());
	ui.GeometryButton->setChecked(gui.geometryWidget->isVisible());
	ui.GridsButton->setChecked(gui.gridsWidget->isVisible());
	ui.GlyphsButton->setChecked(gui.glyphsWidget->isVisible());
	ui.ModelListButton->setChecked(gui.modelListWidget->isVisible());
	ui.MDButton->setChecked(gui.mdWidget->isVisible());
	ui.MessagesButton->setChecked(gui.messagesWidget->isVisible());
	ui.PoresButton->setChecked(gui.poresWidget->isVisible());
	ui.PositionButton->setChecked(gui.positionWidget->isVisible());
	ui.ScriptMovieButton->setChecked(gui.scriptMovieWidget->isVisible());
	ui.SelectButton->setChecked(gui.selectWidget->isVisible());
	ui.TrajectoryButton->setChecked(gui.trajectoryWidget->isVisible());
	ui.TransformButton->setChecked(gui.transformWidget->isVisible());
	ui.VibrationsButton->setChecked(gui.vibrationsWidget->isVisible());
}
	
void ToolBoxWidget::on_AtomListButton_clicked(bool checked)
{
	if (checked)
	{
		gui.atomListWidget->showWidget();
		if (gui.atomListWidget->isFloating()) gui.atomListWidget->move(QCursor::pos());
	}
	else gui.atomListWidget->hide();
}

void ToolBoxWidget::on_BuildButton_clicked(bool checked)
{
	if (checked)
	{
		gui.buildWidget->showWidget();
		if (gui.buildWidget->isFloating()) gui.buildWidget->move(QCursor::pos());
	}
	else gui.buildWidget->hide();
}

void ToolBoxWidget::on_CellDefinitionButton_clicked(bool checked)
{
	if (checked)
	{
		gui.cellDefinitionWidget->showWidget();
		if (gui.cellDefinitionWidget->isFloating()) gui.cellDefinitionWidget->move(QCursor::pos());
	}
	else gui.cellDefinitionWidget->hide();
}

void ToolBoxWidget::on_CellTransformButton_clicked(bool checked)
{
	if (checked)
	{
		gui.cellTransformWidget->showWidget();
		if (gui.cellTransformWidget->isFloating()) gui.cellTransformWidget->move(QCursor::pos());
	}
	else gui.cellTransformWidget->hide();
}

void ToolBoxWidget::on_CommandButton_clicked(bool checked)
{
	if (checked)
	{
		gui.commandWidget->showWidget();
		if (gui.commandWidget->isFloating()) gui.commandWidget->move(QCursor::pos());
	}
	else gui.commandWidget->hide();
}

void ToolBoxWidget::on_DisorderWizardButton_clicked(bool checked)
{
	int result = gui.disorderWizard->run();
}

void ToolBoxWidget::on_ForcefieldsButton_clicked(bool checked)
{
	if (checked)
	{
		gui.forcefieldsWidget->showWidget();
		if (gui.forcefieldsWidget->isFloating()) gui.forcefieldsWidget->move(QCursor::pos());
	}
	else gui.forcefieldsWidget->hide();
}

void ToolBoxWidget::on_FragmentsButton_clicked(bool checked)
{
	if (checked)
	{
		gui.fragmentsWidget->showWidget();
		if (gui.fragmentsWidget->isFloating()) gui.fragmentsWidget->move(QCursor::pos());
	}
	else gui.fragmentsWidget->hide();
}

void ToolBoxWidget::on_GeometryButton_clicked(bool checked)
{
	if (checked)
	{
		gui.geometryWidget->showWidget();
		if (gui.geometryWidget->isFloating()) gui.geometryWidget->move(QCursor::pos());
	}
	else gui.geometryWidget->hide();
}

void ToolBoxWidget::on_GridsButton_clicked(bool checked)
{
	if (checked)
	{
		gui.gridsWidget->showWidget();
		if (gui.gridsWidget->isFloating()) gui.gridsWidget->move(QCursor::pos());
	}
	else gui.gridsWidget->hide();
}

void ToolBoxWidget::on_GlyphsButton_clicked(bool checked)
{
	if (checked)
	{
		gui.glyphsWidget->showWidget();
		if (gui.glyphsWidget->isFloating()) gui.glyphsWidget->move(QCursor::pos());
	}
	else gui.glyphsWidget->hide();
}

void ToolBoxWidget::on_ModelListButton_clicked(bool checked)
{
	if (checked)
	{
		gui.modelListWidget->showWidget();
		if (gui.modelListWidget->isFloating()) gui.modelListWidget->move(QCursor::pos());
	}
	else gui.modelListWidget->hide();
}

void ToolBoxWidget::on_MDButton_clicked(bool checked)
{
	if (checked)
	{
		gui.mdWidget->showWidget();
		if (gui.mdWidget->isFloating()) gui.mdWidget->move(QCursor::pos());
	}
	else gui.mdWidget->hide();
}

void ToolBoxWidget::on_MessagesButton_clicked(bool checked)
{
	if (checked)
	{
		gui.messagesWidget->showWidget();
		if (gui.messagesWidget->isFloating()) gui.messagesWidget->move(QCursor::pos());
	}
	else gui.messagesWidget->hide();
}

void ToolBoxWidget::on_PoresButton_clicked(bool checked)
{
	if (checked)
	{
		gui.poresWidget->showWidget();
		if (gui.poresWidget->isFloating()) gui.poresWidget->move(QCursor::pos());
	}
	else gui.poresWidget->hide();
}

void ToolBoxWidget::on_PositionButton_clicked(bool checked)
{
	if (checked)
	{
		gui.positionWidget->showWidget();
		if (gui.positionWidget->isFloating()) gui.positionWidget->move(QCursor::pos());
	}
	else gui.positionWidget->hide();
}

void ToolBoxWidget::on_ScriptMovieButton_clicked(bool checked)
{
	if (checked)
	{
		gui.scriptMovieWidget->showWidget();
		if (gui.scriptMovieWidget->isFloating()) gui.scriptMovieWidget->move(QCursor::pos());
	}
	else gui.scriptMovieWidget->hide();
}

void ToolBoxWidget::on_SelectButton_clicked(bool checked)
{
	if (checked)
	{
		gui.selectWidget->showWidget();
		if (gui.selectWidget->isFloating()) gui.selectWidget->move(QCursor::pos());
	}
	else gui.selectWidget->hide();
}

void ToolBoxWidget::on_TrajectoryButton_clicked(bool checked)
{
	if (checked)
	{
		gui.trajectoryWidget->showWidget();
		if (gui.trajectoryWidget->isFloating()) gui.trajectoryWidget->move(QCursor::pos());
	}
	else gui.trajectoryWidget->hide();
}

void ToolBoxWidget::on_TransformButton_clicked(bool checked)
{
	if (checked)
	{
		gui.transformWidget->showWidget();
		if (gui.transformWidget->isFloating()) gui.transformWidget->move(QCursor::pos());
	}
	else gui.transformWidget->hide();
}

void ToolBoxWidget::on_VibrationsButton_clicked(bool checked)
{
	if (checked)
	{
		// Disable remainder of GUI and set canvas to be read-only
		gui.setInteractive(FALSE);
		gui.vibrationsWidget->setEnabled(TRUE);
		gui.vibrationsWidget->showWidget();
	}
	else gui.vibrationsWidget->hide();
}

void ToolBoxWidget::on_ZMatrixButton_clicked(bool checked)
{
	if (checked) gui.zmatrixDialog->showWidget();
	else gui.zmatrixDialog->hide();
}

void ToolBoxWidget::dockWidgetVisibilityChanged(bool visibility)
{
	// Cast sender
	QDockWidget *w = qobject_cast<QDockWidget*> (sender());
	if (w == NULL) return;
	if (w == gui.vibrationsWidget)
	{
		// Stop animation if it is playing
		if (gui.vibrationsWidget->ui.PlayPauseVibration->isChecked()) gui.vibrationsWidget->ui.PlayPauseVibration->click();
	}
}

void ToolBoxWidget::dockWidgetTopLevelChanged(bool topLevel)
{
	if (!topLevel) return;
	// Cast sender
	QDockWidget *w = qobject_cast<QDockWidget*> (sender());
	if (w == NULL) return;
	// Resize widget to the general width and a tiny height (so its natural minimum height is used)
	w->resize(270,10);
}
