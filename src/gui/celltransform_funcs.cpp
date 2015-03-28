/*
	*** Cell Transform Dock Widget
	*** src/gui/celltransform_funcs.cpp
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
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/celltransform.h"
#include "parser/commandnode.h"

// Constructor
CellTransformWidget::CellTransformWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Show window
void CellTransformWidget::showWidget()
{
	show();
}

// Refresh window
void CellTransformWidget::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model* m = parent_.aten().currentModelOrFrame();
	if (m == NULL) return;
	UnitCell::CellType ct = m->cell()->type();
	if (refreshing_) return;
	else refreshing_ = TRUE;
	// Update checkboxes in replicate group
	ui.CellReplicateFoldCheck->setChecked( prefs.replicateFold() );
	ui.CellReplicateTrimCheck->setChecked( prefs.replicateTrim() );
	// Update the widgets on the page to reflect the current model's unit cell
	if (ct == UnitCell::NoCell)
	{
		// No cell, so disable group boxes and quit
		ui.CellTransformTabs->setEnabled(FALSE);
		refreshing_ = FALSE;
		return;
	}
	else
	{
		// Activate widgets
		ui.CellTransformTabs->setEnabled(TRUE);
	}
	refreshing_ = FALSE;
}

/*
// Replicate Functions
*/

void CellTransformWidget::on_CellReplicateButton_clicked(bool checked)
{
	CommandNode::run(Commands::Replicate, "dddddd", ui.CellReplicateNegXSpin->value(), ui.CellReplicateNegYSpin->value(), ui.CellReplicateNegZSpin->value(), ui.CellReplicatePosXSpin->value(), ui.CellReplicatePosYSpin->value(),  ui.CellReplicatePosZSpin->value());
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellTransformWidget::on_CellReplicateFoldCheck_clicked(bool checked)
{
	prefs.setReplicateFold(checked);
}

void CellTransformWidget::on_CellReplicateTrimCheck_clicked(bool checked)
{
	prefs.setReplicateTrim(checked);
}

void CellTransformWidget::on_CellReplicateNegXSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_CellReplicateNegYSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_CellReplicateNegZSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_CellReplicatePosXSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_CellReplicatePosYSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_CellReplicatePosZSpin_valueChanged(double d)
{
	parent_.postRedisplay();
}

/*
// Scale Functions
*/

void CellTransformWidget::on_CellScaleButton_clicked(bool checked)
{
	if (ui.CellScaleUseCogsCheck->isChecked()) CommandNode::run(Commands::ScaleMolecules, "dddi", ui.CellScaleXSpin->value(), ui.CellScaleYSpin->value(), ui.CellScaleZSpin->value(), ui.CellScaleCalculateEnergyCheck->isChecked());
	else CommandNode::run(Commands::Scale, "dddi", ui.CellScaleXSpin->value(), ui.CellScaleYSpin->value(), ui.CellScaleZSpin->value(), ui.CellScaleCalculateEnergyCheck->isChecked());
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget+AtenWindow::CellTarget);
}

/*
// Rotate Functions
*/

void CellTransformWidget::on_CellRotateXClockwise_clicked(bool checked)
{
	// Construct rotation matrix
	Matrix mat;
// 	mat.set(0,1.0,0.0,0.0);
// 	mat.set(1,1.0,cos(HALFPI),sin(HALFPI));
// 	mat.set(2,1.0,0.0,0.0);     TODO
}

void CellTransformWidget::on_CellRotateXAnticlockwise_clicked(bool checked)
{
}

void CellTransformWidget::on_CellRotateYClockwise_clicked(bool checked)
{
}

void CellTransformWidget::on_CellRotateYAnticlockwise_clicked(bool checked)
{
}

void CellTransformWidget::on_CellRotateZClockwise_clicked(bool checked)
{
}

void CellTransformWidget::on_CellRotateZAnticlockwise_clicked(bool checked)
{
}

/*
// Miller Cut Functions
*/

void CellTransformWidget::on_MillerCutButton_clicked(bool checked)
{
	CommandNode::run(Commands::MillerCut, "iiii", ui.MillerHSpin->value(), ui.MillerKSpin->value(), ui.MillerLSpin->value(), ui.MillerInRadio->isChecked());
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void CellTransformWidget::on_MillerSelectButton_clicked(bool checked)
{
	CommandNode::run(Commands::SelectMiller, "iiii", ui.MillerHSpin->value(), ui.MillerKSpin->value(), ui.MillerLSpin->value(), ui.MillerInRadio->isChecked());
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::AtomsTarget);
}

void CellTransformWidget::on_MillerHSpin_valueChanged(int value)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_MillerKSpin_valueChanged(int value)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_MillerLSpin_valueChanged(int value)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_MillerInRadio_clicked(bool checked)
{
	parent_.postRedisplay();
}

void CellTransformWidget::on_MillerOutRadio_clicked(bool checked)
{
	parent_.postRedisplay();
}

void CellTransformWidget::closeEvent(QCloseEvent* event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	if (this->isFloating()) parent_.postRedisplay();
	event->accept();
}
