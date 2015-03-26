/*
	*** Cell Definition Dock Widget
	*** src/gui/celldefinition_funcs.cpp
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
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "base/spacegroup.h"
#include "parser/commandnode.h"

// Constructor
CellDefinitionWidget::CellDefinitionWidget(AtenWindow& parent, Qt::WindowFlags flags) : QDockWidget(&parent, flags), parent_(parent)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Show window
void CellDefinitionWidget::showWidget()
{
	show();
	refresh();
}

void CellDefinitionWidget::refreshMatrix()
{
	Model* m = parent_.aten().currentModelOrFrame();
	if (m == NULL) return;
	UnitCell* cell = m->cell();
	Matrix matrix = cell->axes();
	ui.CellMatrixXXSpin->setValue(matrix[0]);
	ui.CellMatrixXYSpin->setValue(matrix[1]);
	ui.CellMatrixXZSpin->setValue(matrix[2]);
	ui.CellMatrixYXSpin->setValue(matrix[4]);
	ui.CellMatrixYYSpin->setValue(matrix[5]);
	ui.CellMatrixYZSpin->setValue(matrix[6]);
	ui.CellMatrixZXSpin->setValue(matrix[8]);
	ui.CellMatrixZYSpin->setValue(matrix[9]);
	ui.CellMatrixZZSpin->setValue(matrix[10]);
}

void CellDefinitionWidget::refreshABC()
{
	Model* m = parent_.aten().currentModelOrFrame();
	if (m == NULL) return;
	UnitCell* cell = m->cell();
	Vec3<double> lengths, angles;
	lengths = cell->lengths();
	angles = cell->angles();
	ui.CellLengthASpin->setValue(lengths.x);
	ui.CellLengthBSpin->setValue(lengths.y);
	ui.CellLengthCSpin->setValue(lengths.z);
	ui.CellAngleASpin->setValue(angles.x);
	ui.CellAngleBSpin->setValue(angles.y);
	ui.CellAngleCSpin->setValue(angles.z);
}

void CellDefinitionWidget::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model* m = parent_.aten().currentModelOrFrame();
	if (m == NULL) return;

	UnitCell* cell = m->cell();
	UnitCell::CellType ct = cell->type();
	QString label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", cell->volume());
	ui.CellVolumeLabel->setText(label);
	if (refreshing_) return;
	else refreshing_ = TRUE;

	// Update the widgets on the page to reflect the current model's unit cell
	if (ct == UnitCell::NoCell)
	{
		// No cell, so disable group boxes and quit
		ui.CellDefinitionGroup->setChecked(FALSE);
		ui.CellSpacegroupGroup->setEnabled(FALSE);
		refreshing_ = FALSE;
		return;
	}
	else
	{
		// Activate widgets
		ui.CellDefinitionGroup->setChecked(TRUE);
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}

	// Set values in spin boxes
	refreshMatrix();
	refreshABC();

	// Set spacegroup label
	label.sprintf("%s (%i)\n", Spacegroups[m->cell()->spacegroupId()].name,  m->cell()->spacegroupId());
	ui.SpacegroupLabel->setText(label);
	refreshing_ = FALSE;
}


void CellDefinitionWidget::on_DefineFromABCButton_clicked(bool checked)
{
	if (refreshing_) return;
	CommandNode::run(Commands::Cell, "dddddd", ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value(), ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value());
	Model* m = parent_.aten().currentModelOrFrame();
	QString label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", m->cell()->volume());
	ui.CellVolumeLabel->setText(label);
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::CellTarget);
}

void CellDefinitionWidget::cellChanged(int index, double newvalue)
{
	if (refreshing_) return;
	Model* m = parent_.aten().currentModelOrFrame();
	// Check supplied matrix index of supplied value to determine if it has changed...
	if ((index != -1) && (fabs(m->cell()->axes()[index] - newvalue) < 1.0E-7)) return;
	CommandNode::run(Commands::CellAxes, "ddddddddd", ui.CellMatrixXXSpin->value(), ui.CellMatrixXYSpin->value(), ui.CellMatrixXZSpin->value(), ui.CellMatrixYXSpin->value(), ui.CellMatrixYYSpin->value(), ui.CellMatrixYZSpin->value(), ui.CellMatrixZXSpin->value(), ui.CellMatrixZYSpin->value(), ui.CellMatrixZZSpin->value());
	QString label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", m->cell()->volume());
	ui.CellVolumeLabel->setText(label);
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::CellTarget);
}

/*
// Cell Definition
*/

void CellDefinitionWidget::on_CellMatrixXXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(0, spin->value());
}

void CellDefinitionWidget::on_CellMatrixXYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(1, spin->value());
}

void CellDefinitionWidget::on_CellMatrixXZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(2, spin->value());
}

void CellDefinitionWidget::on_CellMatrixYXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(3,spin->value());
}

void CellDefinitionWidget::on_CellMatrixYYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(4,spin->value());
}

void CellDefinitionWidget::on_CellMatrixYZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(5,spin->value());
}

void CellDefinitionWidget::on_CellMatrixZXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(6,spin->value());
}

void CellDefinitionWidget::on_CellMatrixZYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(7,spin->value());
}

void CellDefinitionWidget::on_CellMatrixZZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox* spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(8,spin->value());
}

void CellDefinitionWidget::on_CellSpacegroupEdit_returnPressed()
{
	on_CellSpacegroupSetButton_clicked(FALSE);
}

void CellDefinitionWidget::on_CellDefinitionGroup_clicked(bool checked)
{
	// If the group is checked we store the current spin values in the current model.
	if (checked)
	{
		cellChanged(-1,0.0);
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}
	else
	{
		CommandNode::run(Commands::NoCell, "");
		ui.CellSpacegroupGroup->setEnabled(FALSE);
	}
	// Must also update the disordered builder and cell transform tool windows here, since a cell has been added/removed
	parent_.updateWidgets(AtenWindow::CanvasTarget+AtenWindow::CellTarget);
}

/*
// Spacegroup Functions
*/

void CellDefinitionWidget::on_CellSpacegroupSetButton_clicked(bool checked)
{
	// Grab the current text of the line edit and determine spacegroup
	CommandNode::run(Commands::Spacegroup, "c", qPrintable(ui.CellSpacegroupEdit->text()));
	ui.CellSpacegroupEdit->setText("");
	// Set spacegroup label
	Model* m = parent_.aten().currentModelOrFrame();
	QString label;
	label.sprintf("%s (%i)\n", Spacegroups[m->cell()->spacegroupId()].name, m->cell()->spacegroupId());
	ui.SpacegroupLabel->setText(label);
}

void CellDefinitionWidget::on_CellSpacegroupRemoveButton_clicked(bool checked)
{
	CommandNode::run(Commands::Spacegroup, "i", 0);
	// Set spacegroup label
	ui.SpacegroupLabel->setText("None (0)");
}

void CellDefinitionWidget::on_CellSpacegroupPackButton_clicked(bool checked)
{
	CommandNode::run(Commands::Pack, "");
	parent_.updateWidgets(AtenWindow::CanvasTarget);
}

void CellDefinitionWidget::closeEvent(QCloseEvent *event)
{
	// Ensure that the relevant button in the ToolBox dock widget is unchecked now
	if (this->isFloating()) parent_.postRedisplay();
	event->accept();
}
