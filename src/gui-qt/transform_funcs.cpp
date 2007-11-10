/*
	*** Qt atom transform interface
	*** src/gui-qt/transform_funcs.cpp
	Copyright T. Youngs 2007

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

/*
// Rotations
*/

void AtenForm::on_RotateDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	vec3<double> v = master.get_currentmodel()->selection_get_cog();
	// Set widgets
	ui.RotateOriginXSpin->setValue(v.x);
	ui.RotateOriginYSpin->setValue(v.y);
	ui.RotateOriginZSpin->setValue(v.z);
}

void AtenForm::on_RotateDefineAxisButton_clicked(bool on)
{
	// Get geometric centre of selection and current origin
	vec3<double> v, o;
	v = master.get_currentmodel()->selection_get_cog();
	o.x = ui.RotateOriginXSpin->value();
	o.y = ui.RotateOriginYSpin->value();
	o.z = ui.RotateOriginZSpin->value();
	// Set widgets
	v -= o;
	ui.RotateAxisXSpin->setValue(v.x);
	ui.RotateAxisYSpin->setValue(v.y);
	ui.RotateAxisZSpin->setValue(v.z);
}

void AtenForm::rotate_selection(double direction)
{
	vec3<double> v, o;
	v.x = ui.RotateAxisXSpin->value();
	v.y = ui.RotateAxisYSpin->value();
	v.z = ui.RotateAxisZSpin->value();
	o.x = ui.RotateOriginXSpin->value();
	o.y = ui.RotateOriginYSpin->value();
	o.z = ui.RotateOriginZSpin->value();
	master.get_currentmodel()->rotate_selection_vector(o, v, direction * ui.RotateAngleSpin->value());
	master.get_currentmodel()->update_measurements();
	gui.refresh();
}

/*
// Translate Functions
*/

void AtenForm::translate_selection(int axis, int dir)
{
	double step = ui.TranslateShiftSpin->value();	
	vec3<double> tvec;
	tvec.set(axis, double(dir));
	if (ui.TranslateModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		master.get_currentmodel()->translate_selection_local(tvec);
	}
	else if (ui.TranslateWorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		master.get_currentmodel()->translate_selection_world(tvec);
	}
	else if (ui.TranslateCellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (master.get_currentmodel()->cell.get_type() == CT_NONE)
		{
			msg(DM_NONE,"No unit cell defined for model.\n");
			return;
		}
		tvec = master.get_currentmodel()->cell.get_axes().get(axis);
		tvec *= double(dir) * step;
		master.get_currentmodel()->translate_selection_local(tvec);
	}
	master.get_currentmodel()->update_measurements();
	gui.refresh();
}
