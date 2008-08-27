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

void rotatePickAxisButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.RotatePickAxisButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.RotateAxisXSpin->setValue(v.x);
	gui.transformWindow->ui.RotateAxisYSpin->setValue(v.y);
	gui.transformWindow->ui.RotateAxisZSpin->setValue(v.z);
}

void AtenTransform::on_RotatePickAxisButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&rotatePickAxisButton_callback);
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
	m->beginUndoState(s);
	m->rotateSelectionVector(o, v, direction * ui.RotateAngleSpin->value());
	m->endUndoState();
	m->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Matrix Transformations - Apply matrix
*/

void AtenTransform::on_TransformApplyButton_clicked(bool on)
{
	// Put values into our matrix...
	Mat3<double> mat;
	mat.set(0, ui.TransformSourceMatrixAXSpin->value(), ui.TransformSourceMatrixAYSpin->value(), ui.TransformSourceMatrixAZSpin->value());
	mat.set(1, ui.TransformSourceMatrixBXSpin->value(), ui.TransformSourceMatrixBYSpin->value(), ui.TransformSourceMatrixBZSpin->value());
	mat.set(2, ui.TransformSourceMatrixCXSpin->value(), ui.TransformSourceMatrixCYSpin->value(), ui.TransformSourceMatrixCZSpin->value());
	// ...and grab coordinate origin
	Vec3<double> v;
	v.set(ui.TransformOriginXSpin->value(), ui.TransformOriginYSpin->value(), ui.TransformOriginZSpin->value());
	// Perform transformation
	char s[128];
	Model *m = aten.currentModel();
	sprintf(s,"Transform %i atom(s)\n", m->nSelected());
	m->beginUndoState(s);
	m->matrixTransformSelection(v, mat);
	m->endUndoState();
	m->updateMeasurements();
	gui.mainView.postRedisplay();
}

void transformDefineSourceAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineSourceAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformSourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformSourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformSourceMatrixAZSpin->setValue(v.z);
}

void transformDefineSourceBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineSourceBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformSourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformSourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformSourceMatrixBZSpin->setValue(v.z);
}

void transformDefineSourceCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineSourceCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformSourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformSourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformSourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_TransformDefineSourceAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineSourceAButton_callback);
}

void AtenTransform::on_TransformDefineSourceBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineSourceBButton_callback);
}

void AtenTransform::on_TransformDefineSourceCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineSourceCButton_callback);
}

void AtenTransform::on_TransformDefineFromPlaneButton_clicked(bool on)
{
}

void AtenTransform::on_TransformOriginCellCentreButton_clicked(bool on)
{
	Vec3<double> o;
	if (aten.currentModel()->cell()->type() == Cell::NoCell) o.set(0.0,0.0,0.0);
	else o = aten.currentModel()->cell()->centre();
	ui.TransformOriginXSpin->setValue(o.x);
	ui.TransformOriginYSpin->setValue(o.y);
	ui.TransformOriginZSpin->setValue(o.z);
}

void AtenTransform::on_TransformDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModel()->selectionCog();
	// Set widgets
	ui.TransformOriginXSpin->setValue(v.x);
	ui.TransformOriginYSpin->setValue(v.y);
	ui.TransformOriginZSpin->setValue(v.z);
}

/*
// Matrix Transformation - Rotate Into
*/

void AtenTransform::on_RotateIntoButton_clicked(bool on)
{
	gui.mainView.postRedisplay();
}

void transformDefineTargetAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineTargetAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformTargetMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformTargetMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformTargetMatrixAZSpin->setValue(v.z);
}

void transformDefineTargetBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineTargetBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformTargetMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformTargetMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformTargetMatrixBZSpin->setValue(v.z);
}

void transformDefineTargetCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineTargetCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWindow->ui.TransformTargetMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformTargetMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformTargetMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_TransformDefineTargetAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineTargetAButton_callback);
}

void AtenTransform::on_TransformDefineTargetBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineTargetBButton_callback);
}

void AtenTransform::on_TransformDefineTargetCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineTargetCButton_callback);
}

void AtenTransform::dialogFinished(int result)
{
	gui.mainWindow->ui.actionTransformWindow->setChecked(FALSE);
}
