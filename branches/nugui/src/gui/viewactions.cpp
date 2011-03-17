/*
	*** View Actions
	*** src/gui/viewactions.cpp
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
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

// Zoom in
void AtenForm::on_actionViewZoomIn_triggered(bool checked)
{
	aten.currentModelOrFrame()->adjustCamera(0.0,0.0,5.0);
	gui.mainWidget->postRedisplay();
}

// Zoom out
void AtenForm::on_actionViewZoomOut_triggered(bool checked)
{
	aten.currentModelOrFrame()->adjustCamera(0.0,0.0,-5.0);
	gui.mainWidget->postRedisplay();
}

// Reset view
void AtenForm::on_actionViewReset_triggered(bool checked)
{
	aten.currentModelOrFrame()->resetView();
	gui.mainWidget->postRedisplay();
}

// Set perspective view
void AtenForm::on_actionViewPerspective_triggered(bool checked)
{
	if (!checked) return;
	prefs.setPerspective(TRUE);
	gui.mainWidget->doProjection();
	gui.mainWidget->postRedisplay();
}

// Set orthographic view
void AtenForm::on_actionViewOrthographic_triggered(bool checked)
{
	prefs.setPerspective(FALSE);
	gui.mainWidget->doProjection();
	gui.mainWidget->postRedisplay();
}

// Switch render focus from the model's trajectory to the model.
void AtenForm::on_actionViewModel_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::ModelSource);
	Model *m = aten.currentModelOrFrame();
	m->changeLog.add(Log::Camera);
	gui.update(GuiQt::AllTarget);
}

// Switch render focus from the model to the model's trajectory
void AtenForm::on_actionViewTrajectory_triggered(bool checked)
{
	aten.currentModel()->setRenderSource(Model::TrajectorySource);
	Model *m = aten.currentModelOrFrame();
	m->changeLog.add(Log::Camera);
	gui.update(GuiQt::AllTarget);
}

// Set view along cartesian axis supplied
void AtenForm::setCartesianView(double x, double y, double z)
{
	// Set model rotation matrix to be along the specified axis
	aten.currentModelOrFrame()->viewAlong(x,y,z);
	gui.mainWidget->postRedisplay();
}

// Set view along Cell axis supplied
void AtenForm::setCellView(double x, double y, double z)
{
	// Set model rotation matrix to be *along* the specified cell axis
	aten.currentModelOrFrame()->viewAlongCell(x,y,z);
	gui.mainWidget->postRedisplay();
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

// Set current colouring scheme to elemental colours
void AtenForm::on_actionSchemeElement_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ElementScheme);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	gui.mainWidget->postRedisplay();
}

// Set current colouring scheme to charge
void AtenForm::on_actionSchemeCharge_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ChargeScheme);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	gui.mainWidget->postRedisplay();
}

// Set current colouring scheme to force
void AtenForm::on_actionSchemeForce_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::ForceScheme);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	gui.mainWidget->postRedisplay();
}

// Set current colouring scheme to velocity
void AtenForm::on_actionSchemeVelocity_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::VelocityScheme);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	gui.mainWidget->postRedisplay();
}

// Set current colouring scheme to custom
void AtenForm::on_actionSchemeCustom_triggered(bool checked)
{
	if (!checked) return;
	prefs.setColourScheme(Prefs::CustomScheme);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	gui.mainWidget->postRedisplay();
}

// Set scheme actions to reflect supplied Prefs::ColouringScheme
void AtenForm::setActiveSchemeAction(Prefs::ColouringScheme cs)
{
	if (cs == Prefs::ChargeScheme) ui.actionSchemeCharge->setChecked(TRUE);
	else if (cs == Prefs::ElementScheme) ui.actionSchemeElement->setChecked(TRUE);
	else if (cs == Prefs::ForceScheme) ui.actionSchemeForce->setChecked(TRUE);
	else if (cs == Prefs::VelocityScheme) ui.actionSchemeVelocity->setChecked(TRUE);
	else if (cs == Prefs::CustomScheme) ui.actionSchemeCustom->setChecked(TRUE);
	aten.currentModelOrFrame()->changeLog.add(Log::Visual);
	prefs.setColourScheme(cs);
	gui.mainWidget->postRedisplay();
}

