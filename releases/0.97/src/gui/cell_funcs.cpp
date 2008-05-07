/*
	*** Qt cell functions interface
	*** src/gui/cell_funcs.cpp
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
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "model/model.h"
#include "base/spacegroup.h"

// Local variables
bool UPDATING = FALSE;

void AtenForm::refreshCellPages()
{
	// If the cell page is not visible, don't do anything
	//if ((!ui.ShowCellDefinePageButton->isChecked()) && (!ui.ShowCellManipulatePageButton->isChecked())) return;
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model *m = master.currentModel();
	Cell *cell = m->cell();
	Cell::CellType ct = cell->type();
	static char s[64];
	sprintf(s," Volume : %10.3f &#8491;<sup>-3</sup>",cell->volume());
	ui.CellVolumeLabel->setText(s);
	if (UPDATING) return;
	else UPDATING = TRUE;
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
		UPDATING = FALSE;
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
	UPDATING = FALSE;
}

void AtenForm::cellChanged()
{
	static char s[64];
	if (UPDATING) return;
	else UPDATING = TRUE;
	// Construct length and angle vectors and set cell of model
	static Vec3<double> lengths, angles;
	lengths.set(ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value());
	angles.set(ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value());
	// Make changes
	Model *m = master.currentModel();
	if (m->cell()->type() == Cell::NoCell) m->beginUndostate("Add Cell");
	else m->beginUndostate("Change Cell");
	m->setCell(lengths, angles);
	m->endUndostate();
	m->calculateDensity();
	sprintf(s," Volume : %10.3f &#8491;<sup>-3</sup>",m->cell()->volume());
	ui.CellVolumeLabel->setText(s);
	gui.modelChanged(FALSE,FALSE,FALSE);
	UPDATING = FALSE;
}

/*
// Cell Definition
*/

void AtenForm::on_CellLengthASpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellLengthBSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellLengthCSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellAngleASpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellAngleBSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellAngleCSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenForm::on_CellSpacegroupEdit_returnPressed()
{
	on_CellSpacegroupSetButton_clicked(FALSE);
}

void AtenForm::on_CellDefinitionGroup_clicked(bool checked)
{
	// If the group is checked we store the current spin values in the current model.
	if (checked)
	{
		cellChanged();
		ui.CellReplicateGroup->setEnabled(TRUE);
		ui.CellScaleGroup->setEnabled(TRUE);
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}
	else
	{
		Model *m = master.currentModel();
		m->beginUndostate("Remove Cell");
		m->removeCell();
		m->endUndostate();
		ui.CellReplicateGroup->setEnabled(FALSE);
		ui.CellScaleGroup->setEnabled(FALSE);
		ui.CellSpacegroupGroup->setEnabled(FALSE);
	}
	// Must also update the disordered builder stack page here, since a cell has been added/removed
	refreshDisorderPage();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

/*
// Spacegroup Functions
*/

void AtenForm::on_CellSpacegroupSetButton_clicked(bool checked)
{
	Model *m = master.currentModel();
	int sg;
	static char s[64];
	// Grab the current text of the line edit and determine spacegroup
	// Try a direct number conversion first...
	strcpy(s,qPrintable(ui.CellSpacegroupEdit->text()));
	sg = atoi(s);
	if (sg == 0) sg = master.findSpacegroupByName(s);
	// Check for null spacegroup
	if (sg == 0) msg(Debug::None,"Unrecognised spacegroup '%s'.\n", s);
	else
	{
		m->setSpacegroup(sg);
		ui.CellSpacegroupEdit->setText("");
		// Set spacegroup label
		sprintf(s,"%s (%i)\n", master.spacegroups[m->spacegroup()].displayName, m->spacegroup());
		ui.SpacegroupLabel->setText(s);
	}
}

void AtenForm::on_CellSpacegroupRemoveButton_clicked(bool checked)
{
	static char s[64];
	Model *m = master.currentModel();
	m->setSpacegroup(0);
	// Set spacegroup label
	sprintf(s,"%s (%i)\n", master.spacegroups[m->spacegroup()].displayName, m->spacegroup());
	ui.SpacegroupLabel->setText(s);
}

void AtenForm::on_CellSpacegroupPackButton_clicked(bool checked)
{
	Model *m = master.currentModel();
	m->beginUndostate("Pack Cell");
	m->pack();
	m->endUndostate();
	gui.modelChanged();
}

/*
// Replicate Functions
*/

void AtenForm::on_CellReplicateButton_clicked(bool checked)
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

void AtenForm::on_CellReplicateFoldCheck_clicked(bool checked)
{
	prefs.setReplicateFold(checked);
}

void AtenForm::on_CellReplicateTrimCheck_clicked(bool checked)
{
	prefs.setReplicateTrim(checked);
}

/*
// Scale Functions
*/

void AtenForm::on_CellScaleButton_clicked(bool checked)
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
