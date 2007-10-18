/*
	*** Qt position functions interface
	*** src/gui-qt/position_funcs.cpp

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
#include "base/elements.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"

void AtenForm::flip_selection(int axis)
{
}

void AtenForm::on_DefineCentreButton_clicked(bool checked)
{
	// Get centre of current selection
	vec3<double> centre = master.get_currentmodel()->selection_get_cog();
	ui.CentreXSpin->setValue(centre.x);
	ui.CentreYSpin->setValue(centre.y);
	ui.CentreZSpin->setValue(centre.z);
}

void AtenForm::on_CentreSelectionButton_clicked(bool checked)
{
	vec3<double> centre, cog;
	centre.x = ui.CentreXSpin->value();
	centre.y = ui.CentreYSpin->value();
	centre.z = ui.CentreZSpin->value();
	model *m = master.get_currentmodel();
	cog = m->selection_get_cog();
	centre -= cog;
	m->translate_selection_local(centre);
	gui.refresh();
}
