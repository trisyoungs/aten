/*
	*** Qt position functions interface
	*** src/gui/position_funcs.cpp
	Copyright T. Youngs 2007-2009

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
#include "gui/position.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
AtenPosition::AtenPosition(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
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
	char s[2];
	s[0] = 88+axis;
	s[1] = '\0';
	NuCommandNode::run(NuCommand::Mirror, "c", s);
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
	NuCommandNode::run(NuCommand::Centre, "ddd", ui.CentreXSpin->value(), ui.CentreYSpin->value(), ui.CentreZSpin->value());
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
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Vector Shift Functions
*/
void shiftPickAxisButton_callback(Reflist<Atom,int> *picked)
{
	gui.positionWindow->ui.DefineVectorButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r() - picked->first()->item->r();
	gui.positionWindow->ui.VectorShiftXSpin->setValue(v.x);
	gui.positionWindow->ui.VectorShiftYSpin->setValue(v.y);
	gui.positionWindow->ui.VectorShiftZSpin->setValue(v.z);
}

void AtenPosition::on_DefineVectorButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&shiftPickAxisButton_callback);
}

void AtenPosition::on_VectorShiftPositiveButton_clicked(bool checked)
{
	Vec3<double> v;
	v.x = ui.VectorShiftXSpin->value();
	v.y = ui.VectorShiftYSpin->value();
	v.z = ui.VectorShiftZSpin->value();
	v.normalise();
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
	v.normalise();
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
