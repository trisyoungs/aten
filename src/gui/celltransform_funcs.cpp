/*
	*** Qt GUI: Cell Transform Window
	*** src/gui/celltransform_funcs.cpp
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
#include "model/model.h"
#include "gui/mainwindow.h"
#include "gui/gui.h"
#include "gui/celltransform.h"
#include "command/staticcommand.h"
// Constructor
AtenCellTransform::AtenCellTransform(QWidget *parent, Qt::WindowFlags flags) : QDialog(parent,flags)
{
	ui.setupUi(this);

	// Private variables
	refreshing_ = FALSE;
}

// Destructor
AtenCellTransform::~AtenCellTransform()
{
}

// Show window
void AtenCellTransform::showWindow()
{
	//if (shouldRefresh_) refresh();
	show();
}

// Refresh window
void AtenCellTransform::refresh()
{
	// Set label to show cell volume (do this before early exit check so we update the cell volume after widget-enforced cell changes)
	Model *m = aten.currentModel();
	Cell::CellType ct = m->cell()->type();
	if (refreshing_) return;
	else refreshing_ = TRUE;
	// Update checkboxes in replicate group
	ui.CellReplicateFoldCheck->setChecked( prefs.replicateFold() );
	ui.CellReplicateTrimCheck->setChecked( prefs.replicateTrim() );
	// Update the widgets on the page to reflect the current model's unit cell
	if (ct == Cell::NoCell)
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

void AtenCellTransform::on_CellReplicateButton_clicked(bool checked)
{
	static StaticCommandNode cmd(Command::CA_REPLICATE, "dddddd", 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
	// Get values from spin boxes...
	cmd.pokeArguments("dddddd", ui.CellReplicateNegXSpin->value(), ui.CellReplicateNegYSpin->value(), ui.CellReplicateNegZSpin->value(), ui.CellReplicatePosXSpin->value(), ui.CellReplicatePosYSpin->value(),  ui.CellReplicatePosZSpin->value());
	Model *m = aten.currentModel();
	cmd.execute();
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
	static StaticCommandNode cmd(Command::CA_SCALE, "ddd", 0.0, 0.0, 0.0);
	cmd.pokeArguments("ddd", ui.CellScaleXSpin->value(), ui.CellScaleYSpin->value(), ui.CellScaleZSpin->value());
	cmd.setFunction(ui.CellScaleUseCogsCheck->isChecked() ? Command::CA_SCALEMOLECULES : Command::CA_SCALE);
	cmd.execute();
	gui.modelChanged(FALSE,TRUE,FALSE);
}

void AtenCellTransform::dialogFinished(int result)
{
	gui.mainWindow->ui.actionCellTransformWindow->setChecked(FALSE);
}
