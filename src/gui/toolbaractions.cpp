/*
	*** ToolBar Actions
	*** src/gui/toolbaractions.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "model/model.h"

// Set current rendering style to stick
void AtenWindow::on_actionStyleStick_triggered(bool checked)
{
	prefs.setRenderStyle(Prefs::StickStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to tube
void AtenWindow::on_actionStyleTube_triggered(bool checked)
{
	prefs.setRenderStyle(Prefs::TubeStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to sphere
void AtenWindow::on_actionStyleSphere_triggered(bool checked)
{
	prefs.setRenderStyle(Prefs::SphereStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to scaled
void AtenWindow::on_actionStyleScaled_triggered(bool checked)
{
	prefs.setRenderStyle(Prefs::ScaledStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to individual
void AtenWindow::on_actionStyleIndividual_triggered(bool checked)
{
	prefs.setRenderStyle(Prefs::IndividualStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set corresponding action to supplied Prefs::DrawStyle
void AtenWindow::setActiveStyleAction(Prefs::DrawStyle ds)
{
	if (ds == Prefs::StickStyle) ui.actionStyleStick->setChecked(true);
	else if (ds == Prefs::TubeStyle) ui.actionStyleTube->setChecked(true);
	else if (ds == Prefs::SphereStyle) ui.actionStyleSphere->setChecked(true);
	else if (ds == Prefs::ScaledStyle) ui.actionStyleScaled->setChecked(true);
	else if (ds == Prefs::IndividualStyle) ui.actionStyleIndividual->setChecked(true);
	prefs.setRenderStyle(ds);
	postRedisplay();
}

