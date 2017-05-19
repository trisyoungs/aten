/*
	*** Main Window - Calculate Panel Functions
	*** src/gui/mainwindow_panel_calculate.cpp
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
#include "main/aten.h"

// Update calculate panel
void AtenWindow::updateCalculatePanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateCalculatePanel");

	Messenger::exit("AtenWindow::updateCalculatePanel");
}

/*
 * Measure
 */

void AtenWindow::on_CalculateMeasureDistanceButton_clicked(bool checked)
{
	setSelectedMode(UserAction::MeasureDistanceAction, checked);
}

void AtenWindow::on_CalculateMeasureAngleButton_clicked(bool checked)
{
	setSelectedMode(UserAction::MeasureAngleAction, checked);
}

void AtenWindow::on_CalculateMeasureTorsionButton_clicked(bool checked)
{
	setSelectedMode(UserAction::MeasureTorsionAction, checked);
}

void AtenWindow::on_CalculateMeasureClearButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::ClearMeasurements);

	// Update display
	updateWidgets();
}

void AtenWindow::on_CalculateMeasureListButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->listMeasurements();

	// Update display
	updateWidgets();
}

/*
 * Charge
 */

void AtenWindow::on_CalculateChargeTotalButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	double total = 0.0;
	if (currentModel->nSelected() == 0)
	{
		for (Atom* i = currentModel->atoms(); i != NULL; i = i->next) total += i->charge();
		Messenger::print("Total charge in model = %f e.", total);
	}
	else
	{
		for (RefListItem<Atom,int>* ri = currentModel->selection(); ri != NULL; ri = ri->next) total += ri->item->charge();
		Messenger::print("Total charge in selection (%i atoms) = %f e.", currentModel->nSelected(), total);
	}

	// Update display
	updateWidgets();
}

void AtenWindow::on_CalculateChargeAverageButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	double total = 0.0;
	if (currentModel->nSelected() == 0)
	{
		for (Atom* i = currentModel->atoms(); i != NULL; i = i->next) total += i->charge();
		Messenger::print("Average charge in model = %f e.", total / currentModel->nAtoms());
	}
	else
	{
		for (RefListItem<Atom,int>* ri = currentModel->selection(); ri != NULL; ri = ri->next) total += ri->item->charge();
		Messenger::print("Average charge in selection (%i atoms) = %f e.", currentModel->nSelected(), total / currentModel->nSelected());
	}

	// Update display
	updateWidgets();
}

/*
 * Geometry
 */

void AtenWindow::on_CalculateGeometryCentreButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	// Get the centre of geometry of the current selection and, if the model is periodic, fold it into the unit cell
	Vec3<double> cog = currentModel->selectionCentreOfGeometry();
	if (currentModel->isPeriodic()) cog = currentModel->cell().fold(cog);
	Messenger::print("Geometric centre of current selection (%i atoms) is { %f %f %f }.", currentModel->nSelected(), cog.x, cog.y, cog.z);

	// Create dummy atom at the position?
	ReturnValue rv;
	ui.CalculateGeometryCentreButton->callPopupMethod("addDummy", rv);
	if (rv.asBool())
	{
		currentModel->beginUndoState("Add dummy atom at selection's centre of geometry");
		currentModel->addAtom(0, cog);
		currentModel->endUndoState();
	}

	// Update display
	updateWidgets();
}
