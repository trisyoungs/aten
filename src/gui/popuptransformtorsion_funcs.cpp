/*
	*** Popup Widget - Transform Torsion Functions
	*** src/gui/popuptransformtorsion_funcs.cpp
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

#include "gui/popuptransformtorsion.h"
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "parser/commandnode.h"
#include "base/namespace.h"

ATEN_USING_NAMESPACE

// Constructor
TransformTorsionPopup::TransformTorsionPopup(AtenWindow& parent, TMenuButton* buttonParent) : TMenuButtonPopupWidget(buttonParent), parent_(parent)
{
	// Set up interface
	ui.setupUi(this);
}

// Update controls (before show()) (virtual)
void TransformTorsionPopup::updateControls()
{
	refreshing_ = true;

	// If there are exactly two atoms selected, set the label text and and Copy button status accordingly
	Model* model = parent_.aten().currentModelOrFrame();
	if (model)
	{
		setEnabled(true);

		bool validTorsion = false;
		Atom* atoms[4], *torsionAtoms[4];
		if (model->nSelected() == 4)
		{
			// Do these atoms form a torsion angle?
			if (!model->selectedAtoms(4, atoms)) validTorsion = false;
			else if (!Atom::formTorsion(atoms, torsionAtoms)) validTorsion = false;
			else
			{
				currentTorsion_ = model->torsion(torsionAtoms[0], torsionAtoms[1], torsionAtoms[2], torsionAtoms[3]);
				validTorsion = true;
			}
		}

		// Set controls
		if (validTorsion) ui.TorsionLabel->setText(QString("%1 &#176; (atoms %2-%3-%4-%5)").arg(currentTorsion_).arg(torsionAtoms[0]->id()+1).arg(torsionAtoms[1]->id()+1).arg(torsionAtoms[2]->id()+1).arg(torsionAtoms[3]->id()+1));
		else ui.TorsionLabel->setText("N/A");
		ui.CopyCurrentTorsionButton->setEnabled(validTorsion);
	}
	else setEnabled(false);

	refreshing_ = false;
}

// Call named method associated to popup
bool TransformTorsionPopup::callMethod(QString methodName, ReturnValue& rv)
{
	if (methodName == "TEST") return true;
	else if (methodName == "set") on_SetTorsionButton_clicked(false);
	else if (methodName == "hideEvent")
	{
		return true;
	}
	else printf("No method called '%s' is available in this popup.\n", qPrintable(methodName));
	return false;
}

/*
 * Widget Functions
 */

void TransformTorsionPopup::on_CopyCurrentTorsionButton_clicked(bool checked)
{
	ui.NewTorsionSpin->setValue(currentTorsion_);
}

void TransformTorsionPopup::on_SetTorsionButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetTorsions, "dc", ui.NewTorsionSpin->value(), "low");
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetTorsions, "dc", ui.NewTorsionSpin->value(), "high");
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetTorsions, "dc", ui.NewTorsionSpin->value(), "light");
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetTorsions, "dc", ui.NewTorsionSpin->value(), "heavy");
	else CommandNode::run(Commands::SetTorsions, "dc", ui.NewTorsionSpin->value(), "both");

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);

	done();
}

void TransformTorsionPopup::on_IncreaseTorsionButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetTorsions, "dci", ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetTorsions, "dci", ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetTorsions, "dci", ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetTorsions, "dci", ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetTorsions, "dci", ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void TransformTorsionPopup::on_DecreaseTorsionButton_clicked(bool checked)
{
	if (ui.MoveTypeCombo->currentIndex() == 0) CommandNode::run(Commands::SetTorsions, "dci", -ui.DeltaSpin->value(), "low", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 1) CommandNode::run(Commands::SetTorsions, "dci", -ui.DeltaSpin->value(), "high", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 2) CommandNode::run(Commands::SetTorsions, "dci", -ui.DeltaSpin->value(), "light", 1);
	else if (ui.MoveTypeCombo->currentIndex() == 3) CommandNode::run(Commands::SetTorsions, "dci", -ui.DeltaSpin->value(), "heavy", 1);
	else CommandNode::run(Commands::SetTorsions, "dci", -ui.DeltaSpin->value(), "both", 1);

	parent_.updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}
