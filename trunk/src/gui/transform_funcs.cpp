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

#include "base/aten.h"
#include "gui/mainwindow.h"
#include "gui/transform.h"
#include "gui/gui.h"
#include "model/model.h"

// Constructor
AtenTransform::AtenTransform(QWidget *parent)
{
	ui.setupUi(this);
}

// Destructor
AtenTransform::~AtenTransform()
{
}

void AtenTransform::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

/*
// Rotations
*/

void AtenTransform::on_RotateClockwiseButton_clicked(bool on)
{
	rotateSelection(1);
}

void AtenTransform::on_RotateAnticlockwiseButton_clicked(bool on)
{
	rotateSelection(-1);
}

void AtenTransform::on_RotateDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModel()->selectionCog();
	// Set widgets
	ui.RotateOriginXSpin->setValue(v.x);
	ui.RotateOriginYSpin->setValue(v.y);
	ui.RotateOriginZSpin->setValue(v.z);
}

void AtenTransform::on_RotateDefineAxisButton_clicked(bool on)
{
	// Get geometric centre of selection and current origin
	Vec3<double> v, o;
	v = aten.currentModel()->selectionCog();
	o.x = ui.RotateOriginXSpin->value();
	o.y = ui.RotateOriginYSpin->value();
	o.z = ui.RotateOriginZSpin->value();
	// Set widgets
	v -= o;
	ui.RotateAxisXSpin->setValue(v.x);
	ui.RotateAxisYSpin->setValue(v.y);
	ui.RotateAxisZSpin->setValue(v.z);
}

void AtenTransform::rotateSelection(double direction)
{
	Vec3<double> v, o;
	v.x = ui.RotateAxisXSpin->value();
	v.y = ui.RotateAxisYSpin->value();
	v.z = ui.RotateAxisZSpin->value();
	o.x = ui.RotateOriginXSpin->value();
	o.y = ui.RotateOriginYSpin->value();
	o.z = ui.RotateOriginZSpin->value();
	char s[128];
	Model *m = aten.currentModel();
	sprintf(s,"Rotate %i atom(s)\n",m->nSelected());
	m->beginUndostate(s);
	m->rotateSelectionVector(o, v, direction * ui.RotateAngleSpin->value());
	m->endUndostate();
	m->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Matrix Transformations
*/

void AtenTransform::on_DefineFromPlaneButton_clicked(bool on)
{
}

void AtenTransform::on_ApplyTransformButton_clicked(bool on)
{
	// Put values into our matrix...
	Mat3<double> m;
	m.set(0, ui.SourceMatrixAXSpin->value(), ui.SourceMatrixAYSpin->value(), ui.SourceMatrixAZSpin->value());
	m.set(1, ui.SourceMatrixBXSpin->value(), ui.SourceMatrixBYSpin->value(), ui.SourceMatrixBZSpin->value());
	m.set(2, ui.SourceMatrixCXSpin->value(), ui.SourceMatrixCYSpin->value(), ui.SourceMatrixCZSpin->value());

}

void AtenTransform::on_RotateIntoButton_clicked(bool on)
{
}

void defineSourceAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.DefineSourceAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.SourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.SourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.SourceMatrixAZSpin->setValue(v.z);
}

void defineSourceBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.DefineSourceBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.SourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.SourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.SourceMatrixBZSpin->setValue(v.z);
}

void defineSourceCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.DefineSourceCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.SourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.SourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.SourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_DefineSourceAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&defineSourceAButton_callback);
}

void AtenTransform::on_DefineSourceBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&defineSourceBButton_callback);
}

void AtenTransform::on_DefineSourceCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&defineSourceCButton_callback);
}

void AtenTransform::dialogFinished(int result)
{
	gui.mainWindow->ui.actionTransformWindow->setChecked(FALSE);
}
