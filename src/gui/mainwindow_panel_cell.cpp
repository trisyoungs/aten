/*
	*** Main Window - Cell Panel Functions
	*** src/gui/mainwindow_panel_cell.cpp
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

#include "gui/mainwindow.h"
#include "gui/popupcellmatrix.h"
#include "model/model.h"

// Update cell panel
void AtenWindow::updateCellPanel(Model* sourceModel)
{
	if (!sourceModel) return;

	Messenger::enter("AtenWindow::updateCellPanel");

	ui.CellDefinePeriodicButton->setEnabled(sourceModel);
	if (sourceModel) ui.CellDefinePeriodicButton->setChecked(sourceModel->cell().type() != UnitCell::NoCell);
	ui.CellDefineAnglesButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellDefineLengthsButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellDefineMatrixButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellSpacegroupSetButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellTransformReplicateButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellTransformScaleButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);

	Messenger::exit("AtenWindow::updateCellPanel");
}

/*
 * Define
 */

void AtenWindow::on_CellDefinePeriodicButton_clicked(bool checked)
{
	if (refreshing_) return;

	if (checked)
	{
		ReturnValue rv;
		ui.CellDefinePeriodicButton->callPopupMethod("setMatrix", rv);
	}
	else CommandNode::run(Commands::NoCell, "");

	// Update display
	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Transform
 */

void AtenWindow::on_CellTransformReplicateButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.CellTransformReplicateButton->callPopupMethod("replicate", rv);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_CellTransformScaleButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.CellTransformScaleButton->callPopupMethod("scale", rv);

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

/*
 * Miller
 */

void AtenWindow::on_CellMillerSelectButton_clicked(bool checked)
{
}
