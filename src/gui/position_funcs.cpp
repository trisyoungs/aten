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

#include "aten/aten.h"
#include "gui/position.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"

// Constructor
AtenPosition::AtenPosition(QWidget *parent)
{
	ui.setupUi(this);
}

// Destructor
AtenPosition::~AtenPosition()
{
}

void AtenPosition::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

/*
// Flip
*/

void AtenPosition::on_FlipXButton_clicked(bool checked)
{
	flipSelection(0);
}

void AtenPosition::on_FlipYButton_clicked(bool checked)
{
	flipSelection(1);
}

void AtenPosition::on_FlipZButton_clicked(bool checked)
{
	flipSelection(2);
}

void AtenPosition::flipSelection(int axis)
{
	Model *m = aten.currentModel();
	char s[128];
	sprintf(s,"Mirror %i atoms along %c\n", m->nSelected(), 88+axis);
	m->beginUndoState(s);
	m->mirrorSelectionLocal(axis);
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Centre
*/

void AtenPosition::on_DefineCentreButton_clicked(bool checked)
{
	// Get centre of current selection
	Vec3<double> centre = aten.currentModel()->selectionCog();
	ui.CentreXSpin->setValue(centre.x);
	ui.CentreYSpin->setValue(centre.y);
	ui.CentreZSpin->setValue(centre.z);
}

void AtenPosition::on_CentreSelectionButton_clicked(bool checked)
{
	Vec3<double> centre;
	centre.x = ui.CentreXSpin->value();
	centre.y = ui.CentreYSpin->value();
	centre.z = ui.CentreZSpin->value();
	Model *m = aten.currentModel();
	char s[128];
	sprintf(s,"Centre %i atom(s) at %f %f %f\n",m->nSelected(),centre.x,centre.y,centre.z);
	m->beginUndoState(s);
	m->centre(centre);
	m->endUndoState();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Translate Functions
*/

void AtenPosition::on_TranslatePosXButton_clicked(bool on)
{
	translateSelection(0, 1);
}

void AtenPosition::on_TranslatePosYButton_clicked(bool on)
{
	translateSelection(1, 1);
}

void AtenPosition::on_TranslatePosZButton_clicked(bool on)
{
	translateSelection(2, 1);
}

void AtenPosition::on_TranslateNegXButton_clicked(bool on)
{
	translateSelection(0, -1);
}

void AtenPosition::on_TranslateNegYButton_clicked(bool on)
{
	translateSelection(1, -1);
}

void AtenPosition::on_TranslateNegZButton_clicked(bool on)
{
	translateSelection(2, -1);
}

void AtenPosition::translateSelection(int axis, int dir)
{
	double step = ui.TranslateShiftSpin->value();	
	Vec3<double> tvec;
	tvec.set(axis, double(dir));
	static char s[128];
	// Grab model in preparation for undostate...
	Model *m = aten.currentModel();
	if (ui.TranslateModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		sprintf(s,"Translate Cartesian (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndoState(s);
		m->translateSelectionLocal(tvec);
	}
	else if (ui.TranslateWorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		sprintf(s,"Translate Screen (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndoState(s);
		m->translateSelectionWorld(tvec);
	}
	else if (ui.TranslateCellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (m->cell()->type() == Cell::NoCell)
		{
			msg.print("No unit cell defined for model.\n");
			return;
		}
		tvec = aten.currentModel()->cell()->axes().get(axis);
		tvec *= double(dir) * step;
		sprintf(s,"Translate Cell (%i atom(s), %f %f %f)\n",m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->beginUndoState(s);
		m->translateSelectionLocal(tvec);
	}
	m->endUndoState();
	m->updateMeasurements();
	gui.mainView.postRedisplay();
}

/*
// Vector Shift Functions
*/

void AtenPosition::on_DefineVectorButton_clicked(bool checked)
{
	// Set vector from defined atoms
	Model *m = aten.currentModel();
	if (m->nSelected() != 2)
	{
		msg.print("Exactly two atoms must be selected to define a vector.\n");
		return;
	}
	Atom *i = m->firstSelected();
	Vec3<double> v = i->nextSelected()->r() - i->r();
	// Set widgets
	ui.VectorShiftXSpin->setValue(v.x);
	ui.VectorShiftYSpin->setValue(v.y);
	ui.VectorShiftZSpin->setValue(v.z);
}

void AtenPosition::on_VectorShiftPositiveButton_clicked(bool checked)
{
	Vec3<double> v;
	v.x = ui.VectorShiftXSpin->value();
	v.y = ui.VectorShiftYSpin->value();
	v.z = ui.VectorShiftZSpin->value();
	v *= ui.VectorDeltaSpin->value();
	char s[128];
	Model *m = aten.currentModel();
	sprintf(s,"Vector shift %i atom(s) {%f,%f,%f}\n",m->nSelected(),v.x,v.y,v.z);
	m->beginUndoState(s);
	m->translateSelectionLocal(v);
	m->endUndoState();
	m->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenPosition::on_VectorShiftNegativeButton_clicked(bool checked)
{
	Vec3<double> v;
	v.x = ui.VectorShiftXSpin->value();
	v.y = ui.VectorShiftYSpin->value();
	v.z = ui.VectorShiftZSpin->value();
	v *= -ui.VectorDeltaSpin->value();
	char s[128];
	Model *m = aten.currentModel();
	sprintf(s,"Vector shift %i atom(s) {%f,%f,%f}\n",m->nSelected(),v.x,v.y,v.z);
	m->beginUndoState(s);
	m->translateSelectionLocal(v);
	m->endUndoState();
	m->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void AtenPosition::dialogFinished(int result)
{
	gui.mainWindow->ui.actionPositionWindow->setChecked(FALSE);
}
