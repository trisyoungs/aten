/*
	*** Main Window - Cell Panel Functions
	*** src/gui/mainwindow_panel_cell.cpp
	Copyright T. Youngs 2007-2017

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

	// Define
	ui.CellDefinePeriodicButton->setEnabled(sourceModel);
	if (sourceModel) ui.CellDefinePeriodicButton->setChecked(sourceModel->cell().type() != UnitCell::NoCell);
	ui.CellDefineAnglesButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellDefineLengthsButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellDefineMatrixButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	// Spacegroup
	ui.CellSpacegroupSetButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	// Transform
	ui.CellTransformReplicateButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellTransformScaleButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	// Miller
	ui.CellMillerHSpin->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellMillerKSpin->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellMillerLSpin->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);
	ui.CellMillerSelectButton->setEnabled(sourceModel ? sourceModel->cell().type() != UnitCell::NoCell : false);

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
		ui.CellDefineMatrixButton->callPopupMethod("setMatrix", rv);
	}
	else CommandNode::run(Commands::NoCell, "");

	// Update display
	updateWidgets(AtenWindow::CellPanelTarget);
}

/*
 * Transform
 */

void AtenWindow::on_CellTransformReplicateButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.CellTransformReplicateButton->callPopupMethod("replicate", rv);

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_CellTransformScaleButton_clicked(bool checked)
{
	ReturnValue rv;
	ui.CellTransformScaleButton->callPopupMethod("scale", rv);

	updateWidgets(AtenWindow::AtomsTableTarget);
}

/*
 * Miller
 */

void AtenWindow::on_CellMillerShowButton_clicked(bool checked)
{
	updateWidgets();
}

void AtenWindow::on_CellMillerHSpin_valueChanged(int value)
{
	updateWidgets();
}

void AtenWindow::on_CellMillerKSpin_valueChanged(int value)
{
	updateWidgets();
}

void AtenWindow::on_CellMillerLSpin_valueChanged(int value)
{
	updateWidgets();
}

void AtenWindow::on_CellMillerSelectButton_clicked(bool checked)
{
	CommandNode::run(Commands::SelectMiller, "iii", ui.CellMillerHSpin->value(), ui.CellMillerKSpin->value(), ui.CellMillerLSpin->value());

	updateWidgets(AtenWindow::AtomsTableTarget);
}

/*
 * Fold
 */

void AtenWindow::on_CellFoldAtomsButton_clicked(bool checked)
{
	CommandNode::run(Commands::Fold, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_CellFoldMoleculesButton_clicked(bool checked)
{
	CommandNode::run(Commands::FoldMolecules, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}
