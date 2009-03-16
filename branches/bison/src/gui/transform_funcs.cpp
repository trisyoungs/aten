/*
	*** Qt atom transform interface
	*** src/gui/transform_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/transform.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
AtenTransform::AtenTransform(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
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
	NuCommandNode::run(NuCommand::AxisRotate, "ddddddd", v.x, v.y, v.z, direction * ui.RotateAngleSpin->value(), o.x, o.y, o.z);
	Model *m = aten.currentModel();
	m->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

/*
// Matrix Transform
*/

void AtenTransform::on_TransformApplyButton_clicked(bool on)
{
	// Put values into our matrix...
	Mat3<double> mat;
	mat.set(0, ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	mat.set(1, ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	mat.set(2, ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	// ...and grab coordinate origin
	Vec3<double> v;
	v.set(ui.TransformOriginXSpin->value(), ui.TransformOriginYSpin->value(), ui.TransformOriginZSpin->value());

	NuCommandNode::run(NuCommand::MatrixTransform, "ddddddddddddd", mat[0], mat[1], mat[2], mat[3], mat[4], mat[5], mat[6], mat[7], mat[8], v.x, v.y, v.z);

	aten.currentModel()->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
}

void transformDefineAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixAZSpin->setValue(v.z);
}

void transformDefineBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixBZSpin->setValue(v.z);
}

void transformDefineCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.TransformDefineCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_TransformDefineAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineAButton_callback);
}

void AtenTransform::on_TransformDefineBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineBButton_callback);
}

void AtenTransform::on_TransformDefineCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&transformDefineCButton_callback);
}

void AtenTransform::on_TransformNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.normalise();
	gui.transformWindow->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_TransformNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v.normalise();
	gui.transformWindow->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_TransformNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v.normalise();
	gui.transformWindow->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_TransformOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	ref.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_TransformOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	ref.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_TransformOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	ref.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_TransformGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v2.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_TransformGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v2.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWindow->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_TransformGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v2.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.TransformMatrixCZSpin->setValue(v.z);
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
// Matrix Transformation 
*/

void AtenTransform::on_ConvertRotateIntoButton_clicked(bool on)
{
	// Put values into our matrices...
	Mat3<double> source, target, rotmat;
	source.set(0, ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	source.set(1, ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	source.set(2, ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	target.set(0, ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	target.set(1, ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	target.set(2, ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	// ...and grab coordinate origin
	Vec3<double> v;
	v.set(ui.ConvertOriginXSpin->value(), ui.ConvertOriginYSpin->value(), ui.ConvertOriginZSpin->value());

	NuCommandNode::run(NuCommand::MatrixConvert, "ddddddddddddddddddddd", source[0], source[1], source[2], source[3], source[4], source[5], source[6], source[7], source[8], target[0], target[1], target[2], target[3], target[4], target[5], target[6], target[7], target[8], v.x, v.y, v.z);

	aten.currentModel()->updateMeasurements();
	gui.modelChanged(TRUE,FALSE,FALSE);
	gui.mainView.postRedisplay();
}

void convertDefineSourceAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertSourceDefineAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void convertDefineSourceBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertSourceDefineBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void convertDefineSourceCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertSourceDefineCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceDefineAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertDefineSourceAButton_callback);
}

void AtenTransform::on_ConvertSourceDefineBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertDefineSourceBButton_callback);
}

void AtenTransform::on_ConvertSourceDefineCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertDefineSourceCButton_callback);
}

void AtenTransform::on_ConvertSourceNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	ref.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	ref.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	ref.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v2.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v2.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWindow->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertSourceGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v2.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertOriginCellCentreButton_clicked(bool on)
{
	Vec3<double> o;
	if (aten.currentModel()->cell()->type() == Cell::NoCell) o.set(0.0,0.0,0.0);
	else o = aten.currentModel()->cell()->centre();
	ui.ConvertOriginXSpin->setValue(o.x);
	ui.ConvertOriginYSpin->setValue(o.y);
	ui.ConvertOriginZSpin->setValue(o.z);
}

void AtenTransform::on_ConvertDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModel()->selectionCog();
	// Set widgets
	ui.ConvertOriginXSpin->setValue(v.x);
	ui.ConvertOriginYSpin->setValue(v.y);
	ui.ConvertOriginZSpin->setValue(v.z);
}

void convertTargetDefineAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertTargetDefineAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void convertTargetDefineBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertTargetDefineBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void convertTargetDefineCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWindow->ui.ConvertTargetDefineCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetDefineAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertTargetDefineAButton_callback);
}

void AtenTransform::on_ConvertTargetDefineBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertTargetDefineBButton_callback);
}

void AtenTransform::on_ConvertTargetDefineCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainView.beginManualPick(2,&convertTargetDefineCButton_callback);
}

void AtenTransform::on_ConvertTargetNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	ref.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	ref.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	ref.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v2.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v2.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWindow->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void AtenTransform::on_ConvertTargetGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v2.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWindow->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWindow->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWindow->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void AtenTransform::dialogFinished(int result)
{
	gui.mainWindow->ui.actionTransformWindow->setChecked(FALSE);
}
