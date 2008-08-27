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

#include "base/aten.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "model/model.h"

/*
// View Menu Actions
*/

void AtenForm::on_actionViewZoomIn_triggered(bool checked)
{
	aten.currentModel()->adjustCamera(0.0,0.0,5.0,0.0);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewZoomOut_triggered(bool checked)
{
	aten.currentModel()->adjustCamera(0.0,0.0,-5.0,0.0);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewReset_triggered(bool checked)
{
	aten.currentModel()->resetView();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewPerspective_triggered(bool checked)
{
	if (!checked) return;
	prefs.setPerspective(TRUE);
	gui.mainView.doProjection();
	//aten.currentModel()->resetView();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewOrthographic_triggered(bool checked)
{
	prefs.setPerspective(FALSE);
	gui.mainView.doProjection();
	//aten.currentModel()->resetView();
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewModel_triggered(bool checked)
{
	// Switch render focus from the model's trajectory to the model.
	aten.currentModel()->setRenderFromSelf();
	Model *m = aten.currentModel()->renderSource();
	m->calculateViewMatrix();
	m->changeLog.add(Log::Camera);
	gui.modelChanged();
}

void AtenForm::on_actionViewTrajectory_triggered(bool checked)
{
	// Switch render focus from the model to the trajectory.
	aten.currentModel()->setRenderFromFrames();
	Model *m = aten.currentModel()->renderSource();
	m->calculateViewMatrix();
	m->changeLog.add(Log::Camera);
	gui.modelChanged();
}

void AtenForm::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	aten.currentModel()->viewAlong(x,y,z);
	gui.mainView.postRedisplay();
}

void AtenForm::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified cell axis
	aten.currentModel()->viewAlongCell(x,y,z);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionViewSetCartesianPosX_triggered(bool checked)
{
	 setCartesianView(1,0,0);
}

void AtenForm::on_actionViewSetCartesianPosY_triggered(bool checked)
{
	 setCartesianView(0,1,0);
}

void AtenForm::on_actionViewSetCartesianPosZ_triggered(bool checked)
{
	 setCartesianView(0,0,1);
}

void AtenForm::on_actionViewSetCartesianNegX_triggered(bool checked)
{
	 setCartesianView(-1,0,0);
}

void AtenForm::on_actionViewSetCartesianNegY_triggered(bool checked)
{
	 setCartesianView(0,-1,0);
}

void AtenForm::on_actionViewSetCartesianNegZ_triggered(bool checked)
{
	 setCartesianView(0,0,-1);
}

void AtenForm::on_actionViewSetCellNegX_triggered(bool checked)
{
	 setCellView(1,0,0);
}

void AtenForm::on_actionViewSetCellNegY_triggered(bool checked)
{
	 setCellView(0,1,0);
}

void AtenForm::on_actionViewSetCellNegZ_triggered(bool checked)
{
	 setCellView(0,0,1);
}

void AtenForm::on_actionViewSetCellPosX_triggered(bool checked)
{
	 setCellView(-1,0,0);
}

void AtenForm::on_actionViewSetCellPosY_triggered(bool checked)
{
	 setCellView(0,-1,0);
}

void AtenForm::on_actionViewSetCellPosZ_triggered(bool checked)
{
	 setCellView(0,0,-1);
}

void AtenForm::on_actionSchemeElement_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ElementScheme);
	aten.currentModel()->renderSource()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionSchemeCharge_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ChargeScheme);
	aten.currentModel()->renderSource()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}

void AtenForm::on_actionSchemeForce_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ForceScheme);
	aten.currentModel()->renderSource()->changeLog.add(Log::Visual);
	gui.mainView.postRedisplay();
}
