/*
	*** ToolBox Dock Widget Functions
	*** src/gui/toolbox_funcs.cpp
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

#include "gui/gui.h"
#include "gui/atomlist.h"
#include "gui/build.h"
#include "gui/celltransform.h"
#include "gui/celldefinition.h"
#include "gui/command.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/md.h"
#include "gui/messages.h"
#include "gui/modellist.h"
#include "gui/position.h"
#include "gui/select.h"
#include "gui/toolbox.h"
#include "gui/transform.h"
#include "gui/vibrations.h"

// Constructor
ToolBoxWidget::ToolBoxWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

// Destructor
ToolBoxWidget::~ToolBoxWidget()
{
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
	ui.MessagesButton->setChecked(gui.mdWidget->isVisible());
	ui.PositionButton->setChecked(gui.positionWidget->isVisible());
	ui.SelectButton->setChecked(gui.selectWidget->isVisible());
	ui.TransformButton->setChecked(gui.transformWidget->isVisible());
	ui.VibrationsButton->setChecked(gui.vibrationsWidget->isVisible());
}
	
void ToolBoxWidget::on_AtomListButton_clicked(bool checked)
{
	if (checked) gui.atomListWidget->showWidget();
	else gui.atomListWidget->hide();
}

void ToolBoxWidget::on_BuildButton_clicked(bool checked)
{
	if (checked) gui.buildWidget->showWidget();
	else gui.buildWidget->hide();
}

void ToolBoxWidget::on_CellDefinitionButton_clicked(bool checked)
{
	if (checked) gui.cellDefinitionWidget->showWidget();
	else gui.cellDefinitionWidget->hide();
}

void ToolBoxWidget::on_CellTransformButton_clicked(bool checked)
{
	if (checked) gui.cellTransformWidget->showWidget();
	else gui.cellTransformWidget->hide();
}

void ToolBoxWidget::on_CommandButton_clicked(bool checked)
{
	if (checked) gui.commandWidget->showWidget();
	else gui.commandWidget->hide();
}

void ToolBoxWidget::on_ForcefieldsButton_clicked(bool checked)
{
	if (checked) gui.forcefieldsWidget->showWidget();
	else gui.forcefieldsWidget->hide();
}

void ToolBoxWidget::on_FragmentsButton_clicked(bool checked)
{
	if (checked) gui.fragmentsWidget->showWidget();
	else gui.fragmentsWidget->hide();
}

void ToolBoxWidget::on_GeometryButton_clicked(bool checked)
{
	if (checked) gui.geometryWidget->showWidget();
	else gui.geometryWidget->hide();
}

void ToolBoxWidget::on_GridsButton_clicked(bool checked)
{
	if (checked) gui.gridsWidget->showWidget();
	else gui.gridsWidget->hide();
}

void ToolBoxWidget::on_GlyphsButton_clicked(bool checked)
{
	if (checked) gui.glyphsWidget->showWidget();
	else gui.glyphsWidget->hide();
}

void ToolBoxWidget::on_ModelListButton_clicked(bool checked)
{
	if (checked) gui.modelListWidget->showWidget();
	else gui.modelListWidget->hide();
}

void ToolBoxWidget::on_MDButton_clicked(bool checked)
{
	if (checked) gui.mdWidget->showWidget();
	else gui.mdWidget->hide();
}

void ToolBoxWidget::on_MessagesButton_clicked(bool checked)
{
	if (checked) gui.messagesWidget->showWidget();
	else gui.messagesWidget->hide();
}

void ToolBoxWidget::on_PositionButton_clicked(bool checked)
{
	if (checked) gui.positionWidget->showWidget();
	else gui.positionWidget->hide();
}

void ToolBoxWidget::on_SelectButton_clicked(bool checked)
{
	if (checked) gui.selectWidget->showWidget();
	else gui.selectWidget->hide();
}

void ToolBoxWidget::on_TransformButton_clicked(bool checked)
{
	if (checked) gui.transformWidget->showWidget();
	else gui.transformWidget->hide();
}

void ToolBoxWidget::on_VibrationsButton_clicked(bool checked)
{
	if (checked) gui.vibrationsWidget->showWidget();
	else gui.vibrationsWidget->hide();
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
