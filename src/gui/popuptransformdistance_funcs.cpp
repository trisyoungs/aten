/*
	*** Popup Widget - Transform Distance Functions
	*** src/gui/popuptransformdistance_funcs.cpp
	Copyright T. Youngs 2007-2016

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

#include "gui/popuptransformdistance.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
TransformDistancePopup::TransformDistancePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformDistancePopup::updateControls()
{
	refreshing_ = true;

	// If there are exactly two atoms selected, set the label text and and Copy button status accordingly
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		setEnabled(true);

		ui.CopyCurrentDistanceButton->setEnabled(model->nSelected() == 2);
		if (model->nSelected() == 2)
		{
			Atom* atoms[2];
			if (!model->selectedAtoms(2, atoms)) return;
			currentDistance_ = model->distance(atoms[0], atoms[1]);
			ui.DistanceLabel->setText(QString("%1 &#8491; (atoms %2-%3)").arg(currentDistance_).arg(atoms[0]->id()+1).arg(atoms[1]->id()+1));
		}
		else ui.DistanceLabel->setText("N/A");
	}
	else setEnabled(false);

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformDistancePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "set") on_SetDistanceButton_clicked(false);
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else
	{
		printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
		result = false;
	}
	return result;
}

/*
 * Widget Functions
 */

void TransformDistancePopup::on_CopyCurrentDistanceButton_clicked(bool checked)
{
	ui.NewDistanceSpin->setValue(currentDistance_);
}

void TransformDistancePopup::on_SetDistanceButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetDistances, "dc", ui.NewDistanceSpin->value(), "low");
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetDistances, "dc", ui.NewDistanceSpin->value(), "high");
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetDistances, "dc", ui.NewDistanceSpin->value(), "light");
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetDistances, "dc", ui.NewDistanceSpin->value(), "heavy");
	else CommandNode::run(Commands::SetDistances, "dc", ui.NewDistanceSpin->value(), "both");

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);

	done();
}

void TransformDistancePopup::on_IncreaseDistanceButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetDistances, "dci", ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetDistances, "dci", ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetDistances, "dci", ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetDistances, "dci", ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetDistances, "dci", ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void TransformDistancePopup::on_DecreaseDistanceButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetDistances, "dci", -ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetDistances, "dci", -ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetDistances, "dci", -ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetDistances, "dci", -ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetDistances, "dci", -ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}
