/*
	*** Qt GUI: Cell Definition Window
	*** src/gui/celldefine_funcs.cpp
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

#include "main/aten.h"
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/celldefine.h"
#include "gui/celltransform.h"
#include "gui/disorder.h"
#include "base/spacegroup.h"
#include "command/staticcommand.h"

// Constructor
AtenCellDefine::AtenCellDefine(QWidget *parent)
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

void AtenCellDefine::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model *m = aten.currentModel();
	Cell *cell = m->cell();
	Cell::CellType ct = cell->type();
	static char s[64];
	sprintf(s," Volume : %10.3f &#8491;<sup>3</sup>",cell->volume());
	ui.CellVolumeLabel->setText(s);
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
	sprintf(s,"%s (%i)\n", spacegroups.displayName(m->cell()->spacegroup()),  m->cell()->spacegroup());
	ui.SpacegroupLabel->setText(s);
	refreshing_ = FALSE;
}

void AtenCellDefine::cellChanged()
{
	static StaticCommandNode cmd(Command::CA_CELL, "dddddd", 1.0, 1.0, 1.0, 90.0, 90.0, 90.0);
	if (refreshing_) return;
	else refreshing_ = TRUE;
	cmd.pokeArguments("dddddd", ui.CellLengthASpin->value(), ui.CellLengthBSpin->value(), ui.CellLengthCSpin->value(), ui.CellAngleASpin->value(), ui.CellAngleBSpin->value(), ui.CellAngleCSpin->value());
	// Make changes
	cmd.execute();
	Model *m = aten.currentModel()->renderSource();
	char s[64];
	sprintf(s," Volume : %10.3f &#8491;<sup>3</sup>", m->cell()->volume());
	ui.CellVolumeLabel->setText(s);
	gui.modelChanged(FALSE,FALSE,FALSE);
	refreshing_ = FALSE;
}

/*
// Cell Definition
*/

void AtenCellDefine::on_CellLengthASpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellLengthBSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellLengthCSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellAngleASpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellAngleBSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellAngleCSpin_valueChanged(double d)
{
	cellChanged();
}

void AtenCellDefine::on_CellSpacegroupEdit_returnPressed()
{
	on_CellSpacegroupSetButton_clicked(FALSE);
}

void AtenCellDefine::on_CellDefinitionGroup_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_NOCELL, "");
	// If the group is checked we store the current spin values in the current model.
	if (checked)
	{
		cellChanged();
		ui.CellSpacegroupGroup->setEnabled(TRUE);
	}
	else
	{
		cmd.execute();
		ui.CellSpacegroupGroup->setEnabled(FALSE);
	}
	// Must also update the disordered builder and cell transform tool windows here, since a cell has been added/removed
	gui.cellTransformWindow->refresh();
	gui.disorderWindow->refresh();
	gui.modelChanged(FALSE,FALSE,FALSE);
}

/*
// Spacegroup Functions
*/

void AtenCellDefine::on_CellSpacegroupSetButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SPACEGROUP, "i", 0);
	int sg;
	static char s[64];
	// Grab the current text of the line edit and determine spacegroup
	// Try a direct number conversion first...
	strcpy(s,qPrintable(ui.CellSpacegroupEdit->text()));
	sg = atoi(s);
	if (sg == 0) sg = spacegroups.spacegroup(s);
	// Check for null spacegroup
	if (sg == 0) msg.print("Unrecognised spacegroup '%s'.\n", s);
	else
	{
		cmd.pokeArguments("i", sg);
		cmd.execute();
		ui.CellSpacegroupEdit->setText("");
		// Set spacegroup label
		Model *m = aten.currentModel()->renderSource();
		sprintf(s,"%s (%i)\n", spacegroups.displayName(m->cell()->spacegroup()), sg);
		ui.SpacegroupLabel->setText(s);
	}
}

void AtenCellDefine::on_CellSpacegroupRemoveButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_SPACEGROUP, "i", 0);
	cmd.execute();
	// Set spacegroup label
	char s[128];
	sprintf(s,"%s (0)\n", spacegroups.displayName(0));
	ui.SpacegroupLabel->setText(s);
}

void AtenCellDefine::on_CellSpacegroupPackButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_PACK, "");
	cmd.execute();
	gui.modelChanged();
}

void AtenCellDefine::dialogFinished(int result)
{
	gui.mainWindow->ui.actionCellDefineWindow->setChecked(FALSE);
}
