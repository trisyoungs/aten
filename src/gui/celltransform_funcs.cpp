/*
	*** Qt GUI: Cell Transform Window
	*** src/gui/celltransform_funcs.cpp
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

#include "base/master.h"
#include "model/model.h"
#include "gui/gui.h"
#include "gui/celldefine.h"
#include "base/spacegroup.h"

// Constructor
AtenCellTransform::AtenCellTransform(QWidget *parent)
{
	// Private variables
	refreshing_ = FALSE;
}

// Destructor
AtenCellTransform::~AtenCellTransform()
{
}

// Refresh window
void AtenCellTransform::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model *m = master.currentModel();
	Cell *cell = m->cell();
	Cell::CellType ct = cell->type();
	static char s[64];
	sprintf(s," Volume : %10.3f &#8491;<sup>-3</sup>",cell->volume());
	ui.CellVolumeLabel->setText(s);
	if (refreshing_) return;
	else refreshing_ = TRUE;
	// Update checkboxes in replicate group
	ui.CellReplicateFoldCheck->setChecked( prefs.replicateFold() );
	ui.CellReplicateTrimCheck->setChecked( prefs.replicateTrim() );
	// Update the widgets on the page to reflect the current model's unit cell
	if (cell->type() == Cell::NoCell)
	{
		// No cell, so disable group boxes and quit
		ui.CellDefinitionGroup->setChecked(FALSE);
		ui.CellReplicateGroup->setEnabled(FALSE);
		ui.CellScaleGroup->setEnabled(FALSE);
		ui.CellSpacegroupGroup->setEnabled(FALSE);
		refreshing_ = FALSE;
		return;
	}
	else
	{
		// Activate widgets
		ui.CellDefinitionGroup->setChecked(TRUE);
		ui.CellReplicateGroup->setEnabled(TRUE);
		ui.CellScaleGroup->setEnabled(TRUE);
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}
	// Set values in spin boxes
	Vec3<double> lengths, angles;
	lengths = cell->lengths();
	angles = cell->angles();
	ui.CellLengthASpin->setValue(lengths.x);
	ui.CellLengthBSpin->setValue(lengths.y);
	ui.CellLengthCSpin->setValue(lengths.z);
	ui.CellAngleASpin->setValue(angles.x);
	ui.CellAngleBSpin->setValue(angles.y);
	ui.CellAngleCSpin->setValue(angles.z);
	// Set spacegroup label
	sprintf(s,"%s (%i)\n", master.spacegroups[m->spacegroup()].displayName,  m->spacegroup());
	ui.SpacegroupLabel->setText(s);
	refreshing_ = FALSE;
}

/*
// Replicate Functions
*/

void AtenCellTransform::on_CellReplicateButton_clicked(bool checked)
{
	// Get values from spin boxes...
	Vec3<double> neg, pos;
	neg.x = ui.CellReplicateNegXSpin->value();
	neg.y = ui.CellReplicateNegYSpin->value();
	neg.z = ui.CellReplicateNegZSpin->value();
	pos.x = ui.CellReplicatePosXSpin->value();
	pos.y = ui.CellReplicatePosYSpin->value();
	pos.z = ui.CellReplicatePosZSpin->value();
	Model *m = master.currentModel();
	m->beginUndostate("Replicate Cell");
	m->replicateCell(neg, pos);
	m->endUndostate();
	gui.modelChanged();
}

void AtenCellTransform::on_CellReplicateFoldCheck_clicked(bool checked)
{
	prefs.setReplicateFold(checked);
}

void AtenCellTransform::on_CellReplicateTrimCheck_clicked(bool checked)
{
	prefs.setReplicateTrim(checked);
}

/*
// Scale Functions
*/

void AtenCellTransform::on_CellScaleButton_clicked(bool checked)
{
	Vec3<double> scale;
	scale.x = ui.CellScaleXSpin->value();
	scale.y = ui.CellScaleYSpin->value();
	scale.z = ui.CellScaleZSpin->value();
	Model *m = master.currentModel();
	m->beginUndostate("Scale Cell");
	m->scaleCell(scale);
	m->endUndostate();
	gui.modelChanged(FALSE,TRUE,FALSE);
}
