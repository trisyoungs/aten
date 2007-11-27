/*
	*** Qt cell functions interface
	*** src/gui-qt/cell_funcs.cpp
	Copyright T. Youngs 2007

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

#include "base/master.h"
#include "gui/gui.h"
#include "gui-qt/mainwindow.h"

// Local variables
bool cellpage_refreshing = FALSE;

void AtenForm::refresh_cellpage()
{
	if (cellpage_refreshing) return;
	else cellpage_refreshing = TRUE;
	// Update the widgets on the page to reflect the current model's unit cell
	unitcell *cell = master.get_currentmodel()->get_cell();
	cell_type ct = cell->get_type();
	if (cell->get_type() == CT_NONE)
	{
		// No cell, so disable group boxes and quit
		ui.CellDefinitionGroup->setChecked(FALSE);
		ui.CellReplicateGroup->setEnabled(FALSE);
		ui.CellScaleGroup->setEnabled(FALSE);
		cellpage_refreshing = FALSE;
		return;
	}
	else
	{
		// Activate widgets
		ui.CellDefinitionGroup->setChecked(TRUE);
		ui.CellReplicateGroup->setEnabled(TRUE);
		ui.CellScaleGroup->setEnabled(TRUE);
	}
	// Set values in spin boxes
	vec3<double> lengths, angles;
	lengths = cell->get_lengths();
	angles = cell->get_angles();
	ui.CellLengthASpin->setValue(lengths.x);
	ui.CellLengthBSpin->setValue(lengths.y);
	ui.CellLengthCSpin->setValue(lengths.z);
	ui.CellAngleASpin->setValue(angles.x);
	ui.CellAngleBSpin->setValue(angles.y);
	ui.CellAngleCSpin->setValue(angles.z);
	cellpage_refreshing = FALSE;
}

void AtenForm::cell_changed()
{
	if (cellpage_refreshing) return;
	else cellpage_refreshing = TRUE;
	// Construct length and angle vectors and set cell of model
	static vec3<double> lengths, angles;
	lengths.set(ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value());
	angles.set(ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value());
	// Make changes
	model *m = master.get_currentmodel();
	if (m->get_celltype() == CT_NONE) m->begin_undostate("Add Cell");
	else m->begin_undostate("Change Cell");
	m->set_cell(lengths, angles);
	m->end_undostate();
	m->calculate_density();
	gui.refresh();
	cellpage_refreshing = FALSE;
}

void AtenForm::on_CellDefinitionGroup_clicked(bool checked)
{
	// If the group is checked we store the current spin values in the current model.
	if (checked) cell_changed();
	else
	{
		model *m = master.get_currentmodel();
		m->begin_undostate("Remove Cell");
		m->remove_cell();
		m->end_undostate();
	}
	gui.refresh();
}

void AtenForm::on_CellReplicateButton_clicked(bool checked)
{
	// Get values from spin boxes...
	vec3<double> neg, pos;
	neg.x = ui.CellReplicateNegXSpin->value();
	neg.y = ui.CellReplicateNegYSpin->value();
	neg.z = ui.CellReplicateNegZSpin->value();
	pos.x = ui.CellReplicatePosXSpin->value();
	pos.y = ui.CellReplicatePosYSpin->value();
	pos.z = ui.CellReplicatePosZSpin->value();
	master.get_currentmodel()->replicate_cell(neg, pos);
	gui.refresh();
}

void AtenForm::on_CellScaleButton_clicked(bool checked)
{
	vec3<double> scale;
	scale.x = ui.CellScaleXSpin->value();
	scale.y = ui.CellScaleYSpin->value();
	scale.z = ui.CellScaleZSpin->value();
	master.get_currentmodel()->scale_cell(scale);
	gui.refresh();
}
