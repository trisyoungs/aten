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

/*
// Flip
*/

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
	Model *m = master.currentModel();
	char s[128];
	sprintf(s,"Mirror %i atoms along %c\n", m->nSelected(), 88+axis);
	m->beginUndostate(s);
	m->mirrorSelectionLocal(axis);
	m->endUndostate();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Centre
*/

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
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Translate Functions
*/

void AtenForm::on_TranslatePosXButton_clicked(bool on)
{
	translateSelection(0, 1);
}

void AtenForm::on_TranslatePosYButton_clicked(bool on)
{
	translateSelection(1, 1);
}

void AtenForm::on_TranslatePosZButton_clicked(bool on)
{
	translateSelection(2, 1);
}

void AtenForm::on_TranslateNegXButton_clicked(bool on)
{
	translateSelection(0, -1);
}

void AtenForm::on_TranslateNegYButton_clicked(bool on)
{
	translateSelection(1, -1);
}

void AtenForm::on_TranslateNegZButton_clicked(bool on)
{
	translateSelection(2, -1);
}

void AtenForm::translateSelection(int axis, int dir)
{
	double step = ui.TranslateShiftSpin->value();	
	Vec3<double> tvec;
	tvec.set(axis, double(dir));
	static char s[128];
	// Grab model in preparation for undostate...
	Model *m = master.currentModel();
	if (ui.TranslateModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		sprintf(s,"Translate Cartesian (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndostate(s);
		m->translateSelectionLocal(tvec);
	}
	else if (ui.TranslateWorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		sprintf(s,"Translate Screen (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndostate(s);
		m->translateSelectionWorld(tvec);
	}
	else if (ui.TranslateCellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (m->cell()->type() == Cell::NoCell)
		{
			msg(Debug::None,"No unit cell defined for model.\n");
			return;
		}
		tvec = master.currentModel()->cell()->axes().get(axis);
		tvec *= double(dir) * step;
		sprintf(s,"Translate Cell (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndostate(s);
		m->translateSelectionLocal(tvec);
	}
	m->endUndostate();
	m->updateMeasurements();
	gui.mainView.postRedisplay();
}
