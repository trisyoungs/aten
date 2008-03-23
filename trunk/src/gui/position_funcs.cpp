/*
	*** Qt position functions interface
	*** src/gui/position_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

void AtenForm::on_FlipXButton_clicked(bool checked)
{
	flipSelection(0);
}

void AtenForm::on_FlipYButton_clicked(bool checked)
{
	flipSelection(1);
}

void AtenForm::on_FlipZButton_clicked(bool checked)
{
	flipSelection(2);
}

void AtenForm::flipSelection(int axis)
{
}

void AtenForm::on_DefineCentreButton_clicked(bool checked)
{
	// Get centre of current selection
	Vec3<double> centre = master.currentModel()->selectionCog();
	ui.CentreXSpin->setValue(centre.x);
	ui.CentreYSpin->setValue(centre.y);
	ui.CentreZSpin->setValue(centre.z);
}

void AtenForm::on_CentreSelectionButton_clicked(bool checked)
{
	Vec3<double> centre;
	centre.x = ui.CentreXSpin->value();
	centre.y = ui.CentreYSpin->value();
	centre.z = ui.CentreZSpin->value();
	Model *m = master.currentModel();
	char s[128];
	sprintf(s,"Centre %i atom(s) at %f %f %f\n",m->nSelected(),centre.x,centre.y,centre.z);
	m->beginUndostate(s);
	m->centre(centre);
	m->endUndostate();
	gui.refresh();
}
