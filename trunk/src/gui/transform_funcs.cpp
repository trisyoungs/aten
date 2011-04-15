/*
	*** Transform Dock Widget
	*** src/gui/transform_funcs.cpp
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
#include "gui/transform.h"
#include "gui/toolbox.h"
#include "gui/gui.h"
#include "model/model.h"
#include "parser/commandnode.h"

// Constructor
TransformWidget::TransformWidget(QWidget *parent, Qt::WindowFlags flags) : QDockWidget(parent,flags)
{
	ui.setupUi(this);
}

void TransformWidget::showWidget()
{
	show();
	// Make sure toolbutton is in correct state
	gui.toolBoxWidget->ui.TransformButton->setChecked(TRUE);
}

/*
// Rotations
*/

void TransformWidget::on_RotateClockwiseButton_clicked(bool on)
{
	rotateSelection(1);
}

void TransformWidget::on_RotateAnticlockwiseButton_clicked(bool on)
{
	rotateSelection(-1);
}

void TransformWidget::on_RotateDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModelOrFrame()->selectionCentreOfGeometry();
	// Set widgets
	ui.RotateOriginXSpin->setValue(v.x);
	ui.RotateOriginYSpin->setValue(v.y);
	ui.RotateOriginZSpin->setValue(v.z);
}

void TransformWidget::on_RotateDefineAxisButton_clicked(bool on)
{
	// Get geometric centre of selection and current origin
	Vec3<double> v, o;
	v = aten.currentModelOrFrame()->selectionCentreOfGeometry();
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
	gui.transformWidget->ui.RotatePickAxisButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	gui.transformWidget->ui.RotateAxisXSpin->setValue(v.x);
	gui.transformWidget->ui.RotateAxisYSpin->setValue(v.y);
	gui.transformWidget->ui.RotateAxisZSpin->setValue(v.z);
}

void TransformWidget::on_RotatePickAxisButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::RotatePickAxisAction,2,&rotatePickAxisButton_callback);
}

void TransformWidget::rotateSelection(double direction)
{
	Vec3<double> v, o;
	v.x = ui.RotateAxisXSpin->value();
	v.y = ui.RotateAxisYSpin->value();
	v.z = ui.RotateAxisZSpin->value();
	o.x = ui.RotateOriginXSpin->value();
	o.y = ui.RotateOriginYSpin->value();
	o.z = ui.RotateOriginZSpin->value();
	CommandNode::run(Command::AxisRotate, "ddddddd", v.x, v.y, v.z, direction * ui.RotateAngleSpin->value(), o.x, o.y, o.z);
	Model *m = aten.currentModelOrFrame();
	m->updateMeasurements();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

/*
// Matrix Transform
*/

void TransformWidget::on_TransformApplyButton_clicked(bool on)
{
	// Put values into our matrix...
	Matrix mat;
	mat.setColumn(0, ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value(), 0.0);
	mat.setColumn(1, ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value(), 0.0);
	mat.setColumn(2, ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value(), 0.0);
	// ...and grab coordinate origin
	Vec3<double> v;
	v.set(ui.TransformOriginXSpin->value(), ui.TransformOriginYSpin->value(), ui.TransformOriginZSpin->value());
	CommandNode::run(Command::MatrixTransform, "dddddddddddd", mat[0], mat[1], mat[2], mat[3], mat[4], mat[5], mat[6], mat[7], mat[8], v.x, v.y, v.z);

	aten.currentModelOrFrame()->updateMeasurements();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void transformPickAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.TransformPickAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixAZSpin->setValue(v.z);
}

void transformPickBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.TransformPickBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixBZSpin->setValue(v.z);
}

void transformPickCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.TransformPickCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_TransformPickAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::TransformPickAAction,2,&transformPickAButton_callback);
}

void TransformWidget::on_TransformPickBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::TransformPickBAction,2,&transformPickBButton_callback);
}

void TransformWidget::on_TransformPickCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::TransformPickCAction,2,&transformPickCButton_callback);
}

void TransformWidget::on_TransformNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.normalise();
	gui.transformWidget->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_TransformNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v.normalise();
	gui.transformWidget->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_TransformNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v.normalise();
	gui.transformWidget->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_TransformOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	ref.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_TransformOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	ref.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_TransformOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	ref.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_TransformGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v2.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.TransformMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_TransformGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v2.set(ui.TransformMatrixCXSpin->value(), ui.TransformMatrixCYSpin->value(), ui.TransformMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWidget->ui.TransformMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_TransformGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.TransformMatrixAXSpin->value(), ui.TransformMatrixAYSpin->value(), ui.TransformMatrixAZSpin->value());
	v2.set(ui.TransformMatrixBXSpin->value(), ui.TransformMatrixBYSpin->value(), ui.TransformMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.TransformMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.TransformMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.TransformMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_TransformOriginCellCentreButton_clicked(bool on)
{
	Vec3<double> o;
	if (aten.currentModelOrFrame()->cell()->type() == UnitCell::NoCell) o.set(0.0,0.0,0.0);
	else o = aten.currentModelOrFrame()->cell()->centre();
	ui.TransformOriginXSpin->setValue(o.x);
	ui.TransformOriginYSpin->setValue(o.y);
	ui.TransformOriginZSpin->setValue(o.z);
}

void TransformWidget::on_TransformDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModelOrFrame()->selectionCentreOfGeometry();
	// Set widgets
	ui.TransformOriginXSpin->setValue(v.x);
	ui.TransformOriginYSpin->setValue(v.y);
	ui.TransformOriginZSpin->setValue(v.z);
}

/*
// Matrix Transformation 
*/

void TransformWidget::on_ConvertRotateIntoButton_clicked(bool on)
{
	// Put values into our matrices...
	Matrix source, target, rotmat;
	source.setColumn(0, ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value(), 0.0);
	source.setColumn(1, ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value(), 0.0);
	source.setColumn(2, ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value(), 0.0);
	target.setColumn(0, ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value(), 0.0);
	target.setColumn(1, ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value(), 0.0);
	target.setColumn(2, ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value(), 0.0);
	// ...and grab coordinate origin
	Vec3<double> v;
	v.set(ui.ConvertOriginXSpin->value(), ui.ConvertOriginYSpin->value(), ui.ConvertOriginZSpin->value());

	CommandNode::run(Command::MatrixConvert, "ddddddddddddddddddddd", source[0], source[1], source[2], source[4], source[5], source[6], source[8], source[9], source[10], target[0], target[1], target[2], target[4], target[5], target[6], target[8], target[9], target[10], v.x, v.y, v.z);

	aten.currentModelOrFrame()->updateMeasurements();
	gui.update(GuiQt::CanvasTarget+GuiQt::AtomsTarget);
}

void convertSourcePickAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertSourcePickAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void convertSourcePickBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertSourcePickBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void convertSourcePickCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertSourcePickCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourcePickAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertSourcePickAAction,2,&convertSourcePickAButton_callback);
}

void TransformWidget::on_ConvertSourcePickBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertSourcePickBAction,2,&convertSourcePickBButton_callback);
}

void TransformWidget::on_ConvertSourcePickCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertSourcePickCAction,2,&convertSourcePickCButton_callback);
}

void TransformWidget::on_ConvertSourceNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	ref.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	ref.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	ref.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v2.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v2.set(ui.ConvertSourceMatrixCXSpin->value(), ui.ConvertSourceMatrixCYSpin->value(), ui.ConvertSourceMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWidget->ui.ConvertSourceMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertSourceGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertSourceMatrixAXSpin->value(), ui.ConvertSourceMatrixAYSpin->value(), ui.ConvertSourceMatrixAZSpin->value());
	v2.set(ui.ConvertSourceMatrixBXSpin->value(), ui.ConvertSourceMatrixBYSpin->value(), ui.ConvertSourceMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.ConvertSourceMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertSourceMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertSourceMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertOriginCellCentreButton_clicked(bool on)
{
	Vec3<double> o;
	if (aten.currentModelOrFrame()->cell()->type() == UnitCell::NoCell) o.set(0.0,0.0,0.0);
	else o = aten.currentModelOrFrame()->cell()->centre();
	ui.ConvertOriginXSpin->setValue(o.x);
	ui.ConvertOriginYSpin->setValue(o.y);
	ui.ConvertOriginZSpin->setValue(o.z);
}

void TransformWidget::on_ConvertDefineOriginButton_clicked(bool on)
{
	// Get geometric centre of selection
	Vec3<double> v = aten.currentModelOrFrame()->selectionCentreOfGeometry();
	// Set widgets
	ui.ConvertOriginXSpin->setValue(v.x);
	ui.ConvertOriginYSpin->setValue(v.y);
	ui.ConvertOriginZSpin->setValue(v.z);
}

void convertTargetDefineAButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertTargetPickAButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void convertTargetDefineBButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertTargetPickBButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void convertTargetDefineCButton_callback(Reflist<Atom,int> *picked)
{
	gui.transformWidget->ui.ConvertTargetPickCButton->setChecked(FALSE);
	// If there are not two atoms in the list then the mode must have been canceled
	if (picked->nItems() != 2) return;
	Vec3<double> v = picked->last()->item->r();
	v -= picked->first()->item->r();
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetPickAButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertTargetPickAAction,2,&convertTargetDefineAButton_callback);
}

void TransformWidget::on_ConvertTargetPickBButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertTargetPickBAction,2,&convertTargetDefineBButton_callback);
}

void TransformWidget::on_ConvertTargetPickCButton_clicked(bool on)
{
	// Enter manual picking mode
	gui.mainWidget()->setSelectedMode(UserAction::ConvertTargetPickCAction,2,&convertTargetDefineCButton_callback);
}

void TransformWidget::on_ConvertTargetNormaliseAButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetNormaliseBButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetNormaliseCButton_clicked(bool on)
{
	// Normalise vector in widgets
 	Vec3<double> v;
	v.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetOrthogonaliseAButton_clicked(bool on)
{
	// Orthogonalise vector from x vector (or y vector)
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	ref.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetOrthogonaliseBButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	ref.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetOrthogonaliseCButton_clicked(bool on)
{
	// Orthogonalise orthogonal vector from other vectors
 	Vec3<double> v, ref;
	v.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	ref.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v.orthogonalise(ref);
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetGenerateAButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v2.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixAXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixAYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixAZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetGenerateBButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v2.set(ui.ConvertTargetMatrixCXSpin->value(), ui.ConvertTargetMatrixCYSpin->value(), ui.ConvertTargetMatrixCZSpin->value());
	v = v1 * v2;
	gui.transformWidget->ui.ConvertTargetMatrixBXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixBYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixBZSpin->setValue(v.z);
}

void TransformWidget::on_ConvertTargetGenerateCButton_clicked(bool on)
{
	// Generate orthogonal vector from other vectors
 	Vec3<double> v1, v2, v;
	v1.set(ui.ConvertTargetMatrixAXSpin->value(), ui.ConvertTargetMatrixAYSpin->value(), ui.ConvertTargetMatrixAZSpin->value());
	v2.set(ui.ConvertTargetMatrixBXSpin->value(), ui.ConvertTargetMatrixBYSpin->value(), ui.ConvertTargetMatrixBZSpin->value());
	v = v1 * v2;
	v.normalise();
	gui.transformWidget->ui.ConvertTargetMatrixCXSpin->setValue(v.x);
	gui.transformWidget->ui.ConvertTargetMatrixCYSpin->setValue(v.y);
	gui.transformWidget->ui.ConvertTargetMatrixCZSpin->setValue(v.z);
}

void TransformWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	gui.toolBoxWidget->ui.TransformButton->setChecked(FALSE);
	if (this->isFloating()) gui.mainWidget()->postRedisplay();
	event->accept();
}
