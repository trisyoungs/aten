/*
	*** Qt GUI: Cell Definition Window
	*** src/gui/celldefine_funcs.cpp
	Copyright T. Youngs 2007-2010

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
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/celldefine.h"
#include "gui/celltransform.h"
#include "gui/disorder.h"
#include "base/spacegroup.h"
#include "parser/commandnode.h"

// Constructor
AtenCellDefine::AtenCellDefine(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Destructor
AtenCellDefine::~AtenCellDefine()
{
}

// Show window
void AtenCellDefine::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

void AtenCellDefine::refreshMatrix()
{
	Model *m = aten.currentModelOrFrame();
	if (m == NULL) return;
	Cell *cell = m->cell();
	Mat3<double> matrix = cell->axes();
	ui.CellMatrixXXSpin->setValue(matrix[0]);
	ui.CellMatrixXYSpin->setValue(matrix[1]);
	ui.CellMatrixXZSpin->setValue(matrix[2]);
	ui.CellMatrixYXSpin->setValue(matrix[3]);
	ui.CellMatrixYYSpin->setValue(matrix[4]);
	ui.CellMatrixYZSpin->setValue(matrix[5]);
	ui.CellMatrixZXSpin->setValue(matrix[6]);
	ui.CellMatrixZYSpin->setValue(matrix[7]);
	ui.CellMatrixZZSpin->setValue(matrix[8]);
}

void AtenCellDefine::refreshABC()
{
	Model *m = aten.currentModelOrFrame();
	if (m == NULL) return;
	Cell *cell = m->cell();
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

void AtenCellDefine::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model *m = aten.currentModelOrFrame();
	if (m == NULL) return;
	Cell *cell = m->cell();
	Cell::CellType ct = cell->type();
	Dnchar label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", cell->volume());
	ui.CellVolumeLabel->setText(label.get());
	if (refreshing_) return;
	else refreshing_ = TRUE;
	// Update the widgets on the page to reflect the current model's unit cell
	if (ct == Cell::NoCell)
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
	ui.SpacegroupLabel->setText(label.get());
	refreshing_ = FALSE;
}


void AtenCellDefine::on_DefineFromABCButton_clicked(bool checked)
{
	if (refreshing_) return;
	CommandNode::run(Command::Cell, "dddddd", ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value(), ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value());
	Model *m = aten.currentModelOrFrame();
	Dnchar label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", m->cell()->volume());
	ui.CellVolumeLabel->setText(label.get());
	gui.update(FALSE,TRUE,FALSE);
}

void AtenCellDefine::cellChanged(int index, double newvalue)
{
	if (refreshing_) return;
	Model *m = aten.currentModelOrFrame();
	// Check supplied matrix index of supplied value to determine if it has changed...
	if ((index != -1) && (fabs(m->cell()->axes()[index] - newvalue) < 1.0E-7)) return;
	CommandNode::run(Command::CellAxes, "ddddddddd", ui.CellMatrixXXSpin->value(), ui.CellMatrixXYSpin->value(), ui.CellMatrixXZSpin->value(), ui.CellMatrixYXSpin->value(), ui.CellMatrixYYSpin->value(), ui.CellMatrixYZSpin->value(), ui.CellMatrixZXSpin->value(), ui.CellMatrixZYSpin->value(), ui.CellMatrixZZSpin->value());
	Dnchar label;
	label.sprintf(" Volume : %10.3f &#8491;<sup>3</sup>", m->cell()->volume());
	ui.CellVolumeLabel->setText(label.get());
	gui.update(FALSE,TRUE,FALSE);
}

/*
// Cell Definition
*/

void AtenCellDefine::on_CellMatrixXXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(0,spin->value());
}

void AtenCellDefine::on_CellMatrixXYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(1,spin->value());
}

void AtenCellDefine::on_CellMatrixXZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(2,spin->value());
}

void AtenCellDefine::on_CellMatrixYXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(3,spin->value());
}

void AtenCellDefine::on_CellMatrixYYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(4,spin->value());
}

void AtenCellDefine::on_CellMatrixYZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(5,spin->value());
}

void AtenCellDefine::on_CellMatrixZXSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(6,spin->value());
}

void AtenCellDefine::on_CellMatrixZYSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(7,spin->value());
}

void AtenCellDefine::on_CellMatrixZZSpin_editingFinished()
{
	// Cast sender as QDoubleSpinBox
	QDoubleSpinBox *spin = qobject_cast<QDoubleSpinBox*> (sender());
	if (spin == NULL) return;
	cellChanged(8,spin->value());
}

void AtenCellDefine::on_CellSpacegroupEdit_returnPressed()
{
	on_CellSpacegroupSetButton_clicked(FALSE);
}

void AtenCellDefine::on_CellDefinitionGroup_clicked(bool checked)
{
	// If the group is checked we store the current spin values in the current model.
	if (checked)
	{
		cellChanged(-1,0.0);
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}
	else
	{
		CommandNode::run(Command::NoCell, "");
		ui.CellSpacegroupGroup->setEnabled(FALSE);
	}
	// Must also update the disordered builder and cell transform tool windows here, since a cell has been added/removed
	gui.cellTransformWindow->refresh();
	gui.disorderWindow->refresh();
	gui.update(FALSE,FALSE,FALSE);
}

/*
// Spacegroup Functions
*/

void AtenCellDefine::on_CellSpacegroupSetButton_clicked(bool checked)
{
	// Grab the current text of the line edit and determine spacegroup
	CommandNode::run(Command::Spacegroup, "c", qPrintable(ui.CellSpacegroupEdit->text()));
	ui.CellSpacegroupEdit->setText("");
	// Set spacegroup label
	Model *m = aten.currentModelOrFrame();
	Dnchar label;
	label.sprintf("%s (%i)\n", Spacegroups[m->cell()->spacegroupId()].name, m->cell()->spacegroupId());
	ui.SpacegroupLabel->setText(label.get());
}

void AtenCellDefine::on_CellSpacegroupRemoveButton_clicked(bool checked)
{
	CommandNode::run(Command::Spacegroup, "i", 0);
	// Set spacegroup label
	ui.SpacegroupLabel->setText("None (0)");
}

void AtenCellDefine::on_CellSpacegroupPackButton_clicked(bool checked)
{
	CommandNode::run(Command::Pack, "");
	gui.update();
}

void AtenCellDefine::dialogFinished(int result)
{
	gui.mainWindow->ui.actionCellDefineWindow->setChecked(FALSE);
}
