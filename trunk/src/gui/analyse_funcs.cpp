/*
	*** Qt analyse functions interface
	*** src/gui/analyse_funcs.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/master.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "model/model.h"

void AtenForm::on_MeasureDistanceButton_clicked(bool on)
{
	if (on) setUserAction(on, UA_GEOMDIST);
}

void AtenForm::on_MeasureAngleButton_clicked(bool on)
{
	if (on) setUserAction(on, UA_GEOMANGLE);
}

void AtenForm::on_MeasureTorsionButton_clicked(bool on)
{
	if (on) setUserAction(on, UA_GEOMTORSION);
}

void AtenForm::on_RemoveMeasurementsButton_clicked(bool on)
{
	master.currentModel()->removeMeasurements(GT_DISTANCE);
	master.currentModel()->removeMeasurements(GT_ANGLE);
	master.currentModel()->removeMeasurements(GT_TORSION);
	gui.mainView.postRedisplay();
}

void AtenForm::on_MeasureDistanceSelectionButton_clicked(bool on)
{
	master.currentModel()->addMeasurementsInSelection(GT_DISTANCE);
	gui.mainView.postRedisplay();
}

void AtenForm::on_MeasureAngleSelectionButton_clicked(bool on)
{
	master.currentModel()->addMeasurementsInSelection(GT_ANGLE);
	gui.mainView.postRedisplay();
}

void AtenForm::on_MeasureTorsionSelectionButton_clicked(bool on)
{
	master.currentModel()->addMeasurementsInSelection(GT_TORSION);
	gui.mainView.postRedisplay();
}

