/*
	*** Position Dock Widget
	*** src/gui/position_funcs.cpp
	Copyright T. Youngs 2007-2015

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

#include <QtGui/QCloseEvent>
#include "main/aten.h"
#include "gui/position.h"
#include "gui/mainwindow.h"
#include "model/model.h"
#include "parser/commandnode.h"
#include "base/sysfunc.h"

// Constructor
PositionWidget::PositionWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);
}

void PositionWidget::showWidget()
{
	show();
}

/*
// Flip
*/

void PositionWidget::on_FlipXButton_clicked(bool checked)
{
	flipSelection(0);
}

void PositionWidget::on_FlipYButton_clicked(bool checked)
{
	flipSelection(1);
}

void PositionWidget::on_FlipZButton_clicked(bool checked)
{
	flipSelection(2);
}

void PositionWidget::flipSelection(int axis)
{
	CommandNode::run(Commands::Mirror, "i", axis);
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

/*
// Centre
*/

void PositionWidget::on_DefineCentreButton_clicked(bool checked)
{
	// Get centre of current selection
	Vec3<double> centre = parent_.aten().currentModelOrFrame()->selectionCentreOfGeometry();
	ui.CentreXSpin->setValue(centre.x);
	ui.CentreYSpin->setValue(centre.y);
	ui.CentreZSpin->setValue(centre.z);
}

void PositionWidget::on_CentreSelectionButton_clicked(bool checked)
{
	Vec3<double> centre(ui.CentreXSpin->value(), ui.CentreYSpin->value(), ui.CentreZSpin->value());
	Vec3<int> lock(ui.CentreLockXCheck->isChecked(), ui.CentreLockYCheck->isChecked(), ui.CentreLockZCheck->isChecked());
	CommandNode::run(Commands::Centre, "dddiii", centre.x, centre.y, centre.z, lock.x, lock.y, lock.z);
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

/*
// Translate Functions
*/

void PositionWidget::on_TranslatePosXButton_clicked(bool on)
{
	translateSelection(0, 1);
}

void PositionWidget::on_TranslatePosYButton_clicked(bool on)
{
	translateSelection(1, 1);
}

void PositionWidget::on_TranslatePosZButton_clicked(bool on)
{
	translateSelection(2, 1);
}

void PositionWidget::on_TranslateNegXButton_clicked(bool on)
{
	translateSelection(0, -1);
}

void PositionWidget::on_TranslateNegYButton_clicked(bool on)
{
	translateSelection(1, -1);
}

void PositionWidget::on_TranslateNegZButton_clicked(bool on)
{
	translateSelection(2, -1);
}

void PositionWidget::translateSelection(int axis, int dir)
{
	double step = ui.TranslateShiftSpin->value();	
	Vec3<double> tvec;
	tvec.set(axis, double(dir));
	// Grab model in preparation for undostate...
	Model* m = parent_.aten().currentModelOrFrame();
	if (ui.TranslateModelFrameRadio->isChecked())
	{
		// Translate selection in the cartesian axes of the model
		tvec *= step;
		m->beginUndoState("Translate Cartesian (%i atom(s), %f %f %f)", m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->translateSelectionLocal(tvec);
	}
	else if (ui.TranslateWorldFrameRadio->isChecked())
	{
		// Translate selection in the world (view) axes
		tvec *= step;
		m->beginUndoState("Translate Screen (%i atom(s), %f %f %f)", m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->translateSelectionWorld(tvec);
	}
	else if (ui.TranslateCellFrameRadio->isChecked())
	{
		// Translate selection in the cell axes of the model
		if (m->cell()->type() == UnitCell::NoCell)
		{
			Messenger::print("No unit cell defined for model.");
			return;
		}
		tvec = parent_.aten().currentModelOrFrame()->cell()->axes().columnAsVec3(axis);
		tvec *= double(dir) * step;
		m->beginUndoState("Translate Cell (%i atom(s), %f %f %f)", m->nSelected(), tvec.x, tvec.y, tvec.z);
		m->translateSelectionLocal(tvec);
	}
	m->endUndoState();
	m->updateMeasurements();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

/*
// Vector Shift Functions
*/

void shiftPickAxisButton_callback(Reflist<Atom,int>* picked)
{
// 	gui.positionWidget->ui.ShiftPickVectorButton->setChecked(FALSE); ATEN2 TODO
// 	// If there are not two atoms in the list then the mode must have been canceled
// 	if (picked->nItems() != 2) return;
// 	Vec3<double> v = picked->last()->item->r() - picked->first()->item->r();
// 	gui.positionWidget->ui.ShiftVectorXSpin->setValue(v.x);
// 	gui.positionWidget->ui.ShiftVectorYSpin->setValue(v.y);
// 	gui.positionWidget->ui.ShiftVectorZSpin->setValue(v.z);
// 	gui.positionWidget->ui.ShiftVectorMagnitudeLabel->setText(QString::number(v.magnitude()));
}

void PositionWidget::on_ShiftPickVectorButton_clicked(bool on)
{
	// Enter manual picking mode
	parent_.ui.MainView->setSelectedMode(UserAction::ShiftPickVectorAction,2, &shiftPickAxisButton_callback);
}

void PositionWidget::on_ShiftNormaliseVectorButton_clicked(bool on)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	v.normalise();
	ui.ShiftVectorXSpin->setValue(v.x);
	ui.ShiftVectorYSpin->setValue(v.y);
	ui.ShiftVectorZSpin->setValue(v.z);
	ui.ShiftVectorMagnitudeLabel->setText("1.0");
}

void PositionWidget::on_ShiftVectorXSpin_valueChanged(double value)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	ui.ShiftVectorMagnitudeLabel->setText(QString::number(v.magnitude()));
}

void PositionWidget::on_ShiftVectorYSpin_valueChanged(double value)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	ui.ShiftVectorMagnitudeLabel->setText(QString::number(v.magnitude()));
}

void PositionWidget::on_ShiftVectorZSpin_valueChanged(double value)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	ui.ShiftVectorMagnitudeLabel->setText(QString::number(v.magnitude()));
}

void PositionWidget::on_ShiftVectorPositiveButton_clicked(bool checked)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	v *= ui.ShiftVectorDeltaSpin->value();
	Model* m = parent_.aten().currentModelOrFrame();
	m->beginUndoState("Vector shift %i atom(s) {%f,%f,%f}",m->nSelected(),v.x,v.y,v.z);
	m->translateSelectionLocal(v);
	m->endUndoState();
	m->updateMeasurements();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void PositionWidget::on_ShiftVectorNegativeButton_clicked(bool checked)
{
	Vec3<double> v;
	v.x = ui.ShiftVectorXSpin->value();
	v.y = ui.ShiftVectorYSpin->value();
	v.z = ui.ShiftVectorZSpin->value();
	v *= -ui.ShiftVectorDeltaSpin->value();
	Model* m = parent_.aten().currentModelOrFrame();
	m->beginUndoState("Vector shift %i atom(s) {%f,%f,%f}",m->nSelected(),v.x,v.y,v.z);
	m->translateSelectionLocal(v);
	m->endUndoState();
	m->updateMeasurements();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

/*
// Move
*/

void PositionWidget::on_RepositionSelectionButton_clicked(bool on)
{
	Vec3<double> v;
	v.x = ui.RepositionTargetXSpin->value() - ui.RepositionReferenceXSpin->value();
	v.y = ui.RepositionTargetYSpin->value() - ui.RepositionReferenceYSpin->value();
	v.z = ui.RepositionTargetZSpin->value() - ui.RepositionReferenceZSpin->value();
	Model* m = parent_.aten().currentModelOrFrame();
	m->beginUndoState("Reposition %i atom(s) {%f,%f,%f}",m->nSelected(),v.x,v.y,v.z);
	m->translateSelectionLocal(v);
	m->endUndoState();
	m->updateMeasurements();
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void PositionWidget::on_RepositionDefineReferenceButton_clicked(bool on)
{
	// Get centre of current selection
	Vec3<double> centre = parent_.aten().currentModelOrFrame()->selectionCentreOfGeometry();
	ui.RepositionReferenceXSpin->setValue(centre.x);
	ui.RepositionReferenceYSpin->setValue(centre.y);
	ui.RepositionReferenceZSpin->setValue(centre.z);
}

void PositionWidget::on_RepositionDefineTargetButton_clicked(bool on)
{
	// Get centre of current selection
	Vec3<double> centre = parent_.aten().currentModelOrFrame()->selectionCentreOfGeometry();
	ui.RepositionTargetXSpin->setValue(centre.x);
	ui.RepositionTargetYSpin->setValue(centre.y);
	ui.RepositionTargetZSpin->setValue(centre.z);
}

void PositionWidget::closeEvent(QCloseEvent* event)
{
	// Return to select mode if one of the modes in this window is still selected
	if (UserAction::isPositionWidgetAction(parent_.ui.MainView->selectedMode())) parent_.ui.MainView->cancelCurrentMode();

	event->accept();
}
