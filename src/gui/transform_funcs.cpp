/*
	*** Qt atom transform interface
	*** src/gui/transform_funcs.cpp
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
#include "base/elements.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"

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
	char s[128];
	model *m = master.get_currentmodel();
	sprintf(s,"Rotate %i atom(s)\n",m->get_nselected());
	m->begin_undostate(s);
	m->rotate_selection_vector(o, v, direction * ui.RotateAngleSpin->value());
	m->end_undostate();
	m->update_measurements();
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
	static char s[128];
	// Grab model in preparation for undostate...
	model *m = master.get_currentmodel();
	if (ui.TranslateModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		sprintf(s,"Translate Cartesian (%i atom(s), %f %f %f)\n",m->get_nselected(), tvec.x, tvec.y, tvec.z);
		m->begin_undostate(s);
		m->translate_selection_local(tvec);
	}
	else if (ui.TranslateWorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		sprintf(s,"Translate Screen (%i atom(s), %f %f %f)\n",m->get_nselected(), tvec.x, tvec.y, tvec.z);
		m->begin_undostate(s);
		m->translate_selection_world(tvec);
	}
	else if (ui.TranslateCellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (m->get_celltype() == CT_NONE)
		{
			msg(DM_NONE,"No unit cell defined for model.\n");
			return;
		}
		tvec = master.get_currentmodel()->get_cell()->get_axes().get(axis);
		tvec *= double(dir) * step;
		sprintf(s,"Translate Cell (%i atom(s), %f %f %f)\n",m->get_nselected(), tvec.x, tvec.y, tvec.z);
		m->begin_undostate(s);
		m->translate_selection_local(tvec);
	}
	m->end_undostate();
	m->update_measurements();
	gui.refresh();
}
