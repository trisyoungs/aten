/*
	*** Popup Widget - Transform Angle Functions
	*** src/gui/popuptransformangle_funcs.cpp
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

#include "gui/popuptransformangle.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
TransformAnglePopup::TransformAnglePopup(AtenWindow& parent, TMenuButton* buttonParent) : TPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformAnglePopup::updateControls()
{
	refreshing_ = true;

	// If there are exactly two atoms selected, set the label text and and Copy button status accordingly
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		setEnabled(true);

		ui.CopyCurrentAngleButton->setEnabled(model->nSelected() == 2);
		if (model->nSelected() == 3)
		{
			Atom* atoms[3], *orderedAtoms[3];
			if (!model->selectedAtoms(3, atoms)) return;

			// Do these atoms form a bound angle?
			if (Atom::formAngle(atoms, orderedAtoms))
			{
				currentAngle_ = model->angle(orderedAtoms[0], orderedAtoms[1], orderedAtoms[2]);
				ui.AngleLabel->setText(QString("%1 &#176; (atoms %2-%3-%4)").arg(currentAngle_).arg(orderedAtoms[0]->id()+1).arg(orderedAtoms[1]->id()+1).arg(orderedAtoms[2]->id()+1));
			}
			else ui.AngleLabel->setText("N/A");
		}
		else ui.AngleLabel->setText("N/A");
	}
	else setEnabled(false);

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformAnglePopup::callMethod(QString methodName, ReturnValue& rv)
{
	bool result = true;
	if (methodName == "TEST") return true;
	else if (methodName == "set") on_SetAngleButton_clicked(false);
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

void TransformAnglePopup::on_CopyCurrentAngleButton_clicked(bool checked)
{
	ui.NewAngleSpin->setValue(currentAngle_);
}

void TransformAnglePopup::on_SetAngleButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetAngles, "dc", ui.NewAngleSpin->value(), "low");
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetAngles, "dc", ui.NewAngleSpin->value(), "high");
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetAngles, "dc", ui.NewAngleSpin->value(), "light");
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetAngles, "dc", ui.NewAngleSpin->value(), "heavy");
	else CommandNode::run(Commands::SetAngles, "dc", ui.NewAngleSpin->value(), "both");

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);

	done();
}

void TransformAnglePopup::on_IncreaseAngleButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetAngles, "dci", ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetAngles, "dci", ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetAngles, "dci", ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetAngles, "dci", ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetAngles, "dci", ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void TransformAnglePopup::on_DecreaseAngleButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetAngles, "dci", -ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetAngles, "dci", -ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetAngles, "dci", -ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetAngles, "dci", -ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetAngles, "dci", -ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}
