/*
	*** Qt view actions
	*** src/gui/viewactions.cpp
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
#include "gui/prefs.h"

/*
// View Menu Actions
*/

void AtenForm::on_actionViewZoomIn_triggered(bool checked)
{
	master.get_currentmodel()->adjust_camera(0.0,0.0,5.0,0.0);
	gui.refresh();
}

void AtenForm::on_actionViewZoomOut_triggered(bool checked)
{
	master.get_currentmodel()->adjust_camera(0.0,0.0,-5.0,0.0);
	gui.refresh();
}

void AtenForm::on_actionViewReset_triggered(bool checked)
{
	master.get_currentmodel()->reset_view();
	gui.refresh();
}

void AtenForm::on_actionViewPerspective_triggered(bool checked)
{
	if (!checked) return;
	prefs.set_perspective(TRUE);
	gui.mainview.do_projection();
	master.get_currentmodel()->reset_view();
	gui.refresh();
}

void AtenForm::on_actionViewOrthographic_triggered(bool checked)
{
	prefs.set_perspective(FALSE);
	gui.mainview.do_projection();
	master.get_currentmodel()->reset_view();
	gui.refresh();
}

void AtenForm::on_actionViewModel_triggered(bool checked)
{
	// Switch render focus from the model's trajectory to the model.
	master.get_currentmodel()->render_from_self();
	gui.refresh();
}

void AtenForm::on_actionViewTrajectory_triggered(bool checked)
{
	// Switch render focus from the model to the trajectory.
	master.get_currentmodel()->render_from_frames();
	gui.refresh();
}

void AtenForm::set_cartesian_view(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified axis
	vec3<double> v;
	v.set(x,y,z);
	v.to_spherical();
	// set_rotation() expects the degrees of rotation about the x and y axes respectively,
	// so give it phi and theta in the reverse order. 
	master.get_currentmodel()->set_rotation(-v.z,v.y);
	gui.refresh();
}

void AtenForm::set_cell_view(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified axis
	vec3<double> v;
	v.set(x,y,z);
	v *= master.get_currentmodel()->get_cell()->get_transpose();
	v.to_spherical();
	// set_rotation() expects the degrees of rotation about the x and y axes respectively,
	// so give it phi and theta in the reverse order. 
	master.get_currentmodel()->set_rotation(-v.z,v.y);
	gui.refresh();
}

void AtenForm::on_actionViewUnhideAtoms_triggered(bool checked)
{
	// Set all atoms in the current model to be visible
	model *m = master.get_currentmodel();
	for (atom *i = m->get_atoms(); i != NULL; i = i->next) m->set_hidden(i, FALSE);
	gui.refresh();
}
