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
	prefs.setRenderStyle(Atom::StickStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to tube
void AtenWindow::on_actionStyleTube_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::TubeStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to sphere
void AtenWindow::on_actionStyleSphere_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::SphereStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to scaled
void AtenWindow::on_actionStyleScaled_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::ScaledStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set current rendering style to individual
void AtenWindow::on_actionStyleIndividual_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::IndividualStyle);
	aten_.globalLogChange(Log::Style);
	postRedisplay();
}

// Set corresponding action to supplied Atom::DrawStyle
void AtenWindow::setActiveStyleAction(Atom::DrawStyle ds)
{
	if (ds == Atom::StickStyle) ui.actionStyleStick->setChecked(TRUE);
	else if (ds == Atom::TubeStyle) ui.actionStyleTube->setChecked(TRUE);
	else if (ds == Atom::SphereStyle) ui.actionStyleSphere->setChecked(TRUE);
	else if (ds == Atom::ScaledStyle) ui.actionStyleScaled->setChecked(TRUE);
	else if (ds == Atom::IndividualStyle) ui.actionStyleIndividual->setChecked(TRUE);
	prefs.setRenderStyle(ds);
	postRedisplay();
}

// Enter basic atom selection mode
void AtenWindow::on_actionSelectAtoms_triggered(bool on)
{
	if (on) ui.MainView->setSelectedMode(UserAction::SelectAction);
}

// Enter molecule selection mode
void AtenWindow::on_actionSelectMolecules_triggered(bool on)
{
	if (on) ui.MainView->setSelectedMode(UserAction::SelectMoleculeAction);
}

// Enter element selection mode
void AtenWindow::on_actionSelectElement_triggered(bool on)
{
	if (on) ui.MainView->setSelectedMode(UserAction::SelectElementAction);
}

