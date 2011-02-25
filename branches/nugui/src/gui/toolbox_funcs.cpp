/*
	*** Toolbox Dock Widget Functions
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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/geometry.h"
#include "gui/grids.h"
#include "gui/glyphs.h"
#include "gui/build.h"
#include "gui/celltransform.h"
#include "gui/celldefine.h"
#include "gui/command.h"
#include "gui/transform.h"
#include "gui/position.h"
#include "gui/atomlist.h"
#include "gui/select.h"
#include "gui/forcefields.h"
#include "gui/fragment.h"
#include "gui/md.h"
#include "gui/minimiser.h"
#include "gui/vibrations.h"
#include "gui/zmatrix.h"

void AtenForm::on_AtomlistWidget_triggered(bool checked)
{
	if (checked)
	{
		gui.atomlistWindow->showWindow();
		gui.atomlistWindow->refresh();
	}
	else gui.atomlistWindow->hide();
}

void AtenForm::on_BuildWidget_triggered(bool checked)
{
	if (checked) gui.buildWindow->showWindow();
	else gui.buildWindow->hide();
}

void AtenForm::on_TransformWidget_triggered(bool checked)
{
	if (checked) gui.transformWindow->showWindow();
	else gui.transformWindow->hide();
}

void AtenForm::on_SelectWidget_triggered(bool checked)
{
	if (checked) gui.selectWindow->showWindow();
	else gui.selectWindow->hide();
}

void AtenForm::on_PositionWidget_triggered(bool checked)
{
	if (checked) gui.positionWindow->showWindow();
	else gui.positionWindow->hide();
}

void AtenForm::on_CellDefineWidget_triggered(bool checked)
{
	if (checked)
	{
		gui.cellDefineWindow->showWindow();
		gui.cellDefineWindow->refresh();
	}
	else gui.cellDefineWindow->hide();
}

void AtenForm::on_CellTransformWidget_triggered(bool checked)
{
	if (checked)
	{
		gui.cellTransformWindow->showWindow();
		gui.cellTransformWindow->refresh();
	}
	else gui.cellTransformWindow->hide();
}

void AtenForm::on_CommandWidget_triggered(bool checked)
{
	if (checked)
	{
		gui.commandWindow->showWindow();
		gui.commandWindow->refresh();
	}
	else gui.commandWindow->hide();
}

void AtenForm::on_MinimiserWidget_triggered(bool checked)
{
	if (checked) gui.minimiserWindow->showWindow();
	else gui.minimiserWindow->hide();
}

void AtenForm::on_DisorderWidget_triggered(bool checked)
{
	if (checked) gui.disorderWindow->showWindow();
	else gui.disorderWindow->hide();
}

void AtenForm::on_ForcefieldsWidget_triggered(bool checked)
{
	if (checked)
	{
		gui.forcefieldsWindow->showWindow();
		gui.forcefieldsWindow->refresh();
	}
	else gui.forcefieldsWindow->hide();
}

void AtenForm::on_FragmentWidget_triggered(bool checked)
{
	if (checked) gui.fragmentWindow->showWindow();
	else gui.fragmentWindow->hide();
}

void AtenForm::on_GeometryWidget_triggered(bool checked)
{
	if (checked) gui.geometryWindow->showWindow();
	else gui.geometryWindow->hide();
}

void AtenForm::on_GridsWidget_triggered(bool checked)
{
	if (checked) gui.gridsWindow->showWindow();
	else gui.gridsWindow->hide();
}

void AtenForm::on_GlyphsWidget_triggered(bool checked)
{
	if (checked) gui.glyphsWindow->showWindow();
	else gui.glyphsWindow->hide();
}

void AtenForm::on_MolecularDynamicsWidget_triggered(bool checked)
{
	if (checked) gui.mdWindow->showWindow();
	else gui.mdWindow->hide();
}

void AtenForm::on_VibrationsWidget_triggered(bool checked)
{
	if (checked) gui.vibrationsWindow->showWindow();
	else gui.vibrationsWindow->hide();
}

void AtenForm::on_ZMatrixWidget_triggered(bool checked)
{
	if (checked) gui.zmatrixWindow->showWindow();
	else gui.zmatrixWindow->hide();
}

