/*
	*** Qt GUI: Measurement actions
	*** src/gui/measureactions.cpp
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
#include "model/model.h"
#include "parser/commandnode.h"

void AtenForm::on_actionMeasureDistance_triggered(bool on)
{
	if (on) gui.mainWidget->setSelectedMode(UserAction::MeasureDistanceAction);
}

void AtenForm::on_actionMeasureAngle_triggered(bool on)
{
	if (on) gui.mainWidget->setSelectedMode(UserAction::MeasureAngleAction);
}

void AtenForm::on_actionMeasureTorsion_triggered(bool on)
{
	if (on) gui.mainWidget->setSelectedMode(UserAction::MeasureTorsionAction);
}

void AtenForm::on_actionMeasureClearAll_triggered(bool on)
{
	CommandNode::run(Command::ClearMeasurements, "");
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionMeasureDistanceSelection_triggered(bool on)
{
	CommandNode::run(Command::MeasureSelected, "i", 2);
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionMeasureAngleSelection_triggered(bool on)
{
	CommandNode::run(Command::MeasureSelected, "i", 3);
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionMeasureTorsionSelection_triggered(bool on)
{
	CommandNode::run(Command::MeasureSelected, "i", 4);
	gui.update(FALSE,FALSE,FALSE);
}

void AtenForm::on_actionMeasureList_triggered(bool on)
{
	aten.currentModelOrFrame()->listMeasurements();
}
