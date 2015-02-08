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
#include "gui/gui.h"
#include "model/model.h"

// Set current rendering style to stick
void AtenForm::on_actionStyleStick_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::StickStyle);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current rendering style to tube
void AtenForm::on_actionStyleTube_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::TubeStyle);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current rendering style to sphere
void AtenForm::on_actionStyleSphere_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::SphereStyle);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current rendering style to scaled
void AtenForm::on_actionStyleScaled_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::ScaledStyle);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set current rendering style to individual
void AtenForm::on_actionStyleIndividual_triggered(bool checked)
{
	prefs.setRenderStyle(Atom::IndividualStyle);
	aten.globalLogChange(Log::Style);
	parent_.postRedisplay();
}

// Set corresponding action to supplied Atom::DrawStyle
void AtenForm::setActiveStyleAction(Atom::DrawStyle ds)
{
	if (ds == Atom::StickStyle) ui.actionStyleStick->setChecked(TRUE);
	else if (ds == Atom::TubeStyle) ui.actionStyleTube->setChecked(TRUE);
	else if (ds == Atom::SphereStyle) ui.actionStyleSphere->setChecked(TRUE);
	else if (ds == Atom::ScaledStyle) ui.actionStyleScaled->setChecked(TRUE);
	else if (ds == Atom::IndividualStyle) ui.actionStyleIndividual->setChecked(TRUE);
	prefs.setRenderStyle(ds);
	parent_.postRedisplay();
}

// Enter basic atom selection mode
void AtenForm::on_actionSelectAtoms_triggered(bool on)
{
	if (on) gui.mainCanvas()->setSelectedMode(UserAction::SelectAction);
}

// Enter molecule selection mode
void AtenForm::on_actionSelectMolecules_triggered(bool on)
{
	if (on) gui.mainCanvas()->setSelectedMode(UserAction::SelectMoleculeAction);
}

// Enter element selection mode
void AtenForm::on_actionSelectElement_triggered(bool on)
{
	if (on) gui.mainCanvas()->setSelectedMode(UserAction::SelectElementAction);
}

