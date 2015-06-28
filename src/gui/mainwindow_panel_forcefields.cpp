/*
	*** Main Window - Forcefields Panel Functions
	*** src/gui/mainwindow_panel_forcefields.cpp
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
#include "main/aten.h"
#include "ff/forcefield.h"
#include "gui/ffeditor.h"
#include <QFileDialog>

// Update forcefields panel
void AtenWindow::updateForcefieldsPanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateForcefieldsPanel");

	Forcefield* currentForcefield = aten_.currentForcefield();

	// Forcefields list
	ui.ForcefieldsList->clear();
	int n = 0;
	for (Forcefield* ff = aten_.forcefields(); ff != NULL; ff = ff->next)
	{
		ui.ForcefieldsList->addItem(ff->name());
		if (ff == aten_.currentForcefield()) ui.ForcefieldsList->setCurrentRow(n);
		++n;
	}
	ui.ForcefieldsManageCloseButton->setEnabled(currentForcefield);
	ui.ForcefieldsManageEditButton->setEnabled(currentForcefield);
	ui.ForcefieldsManageAssignButton->setEnabled(currentForcefield);
// 	ui.AutomaticTypingGroup->setEnabled(ffselected);  // ATEN2 TODO
// 	ui.ManualTypingGroup->setEnabled(ffselected);

	// Calculate
	ui.ForcefieldsCalculateMinimiseButton->setEnabled(sourceModel);
	ui.ForcefieldsCalculateEnergyButton->setEnabled(sourceModel);
	ui.ForcefieldsCalculateForcesButton->setEnabled(sourceModel);

	Messenger::exit("AtenWindow::updateForcefieldsPanel");
}

/*
 * Manage
 */

void AtenWindow::on_ForcefieldsList_currentRowChanged(int row)
{
	if (refreshing_) return;

	// Set the new default forcefield in the master and refresh the forcefields page
	aten_.setCurrentForcefield(row);
}

void AtenWindow::on_ForcefieldsManageLoadButton_clicked(bool checked)
{
	static QDir currentDirectory_(aten_.dataDir());
	QString filename = QFileDialog::getOpenFileName(this, "Open Forcefield", currentDirectory_.path(), "Forcefield Files (*.ff);;All files (*)");
	if (!filename.isEmpty())
	{
		aten_.loadForcefield(qPrintable(filename));

		updateWidgets(AtenWindow::ForcefieldsTarget);
		
		// Store path for next use
		currentDirectory_.setPath(filename);
	}
}

void AtenWindow::on_ForcefieldsManageCloseButton_clicked(bool checked)
{
	aten_.removeForcefield(aten_.currentForcefield());
	updateWidgets(AtenWindow::ForcefieldsTarget);
}

void AtenWindow::on_ForcefieldsManageEditButton_clicked(bool checked)
{
	AtenForcefieldEditor ffEditor(this);
	ffEditor.populate(aten_.currentForcefield());
	ffEditor.show();
}

void AtenWindow::on_ForcefieldsManageAssignButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->setForcefield(aten_.currentForcefield());
}

/*
 * Calculate
 */

void AtenWindow::on_ForcefieldsCalculateMinimiseButton_clicked(bool checked)
{
}

void AtenWindow::on_ForcefieldsCalculateEnergyButton_clicked(bool checked)
{
	ReturnValue result;
	if (aten_.current().rs() == aten_.currentModel()) result = CommandNode::run(Commands::ModelEnergy, "");
	else result = CommandNode::run(Commands::FrameEnergy, "");

	// Print energy
	aten_.currentModel()->renderSourceModel()->energy.print();
}

void AtenWindow::on_ForcefieldsCalculateForcesButton_clicked(bool checked)
{
	ReturnValue result;
	if (aten_.current().rs() == aten_.currentModel()) result = CommandNode::run(Commands::ModelForces, "");
	else result = CommandNode::run(Commands::FrameForces, "");

	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Patterns
 */

void AtenWindow::on_ForcefieldsPatternsCreateButton_clicked(bool checked)
{
	CommandNode::run(Commands::CreatePatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsClearButton_clicked(bool checked)
{
	CommandNode::run(Commands::ClearPatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsListButton_clicked(bool checked)
{
	CommandNode::run(Commands::ListPatterns, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

void AtenWindow::on_ForcefieldsPatternsDefaultButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->createDefaultPattern(); // ATEN2 TODO Make this into a command?

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}

/*
 * Expression
 */

void AtenWindow::on_ForcefieldsExpressionTypeButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->typeAll(aten_.currentForcefield());

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_ForcefieldsExpressionCreateButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->createExpression(Choice::Default, Choice::Default, ui.ForcefieldsExpressionChargesButton->isChecked() ? Choice::Yes : Choice::No, aten_.currentForcefield(), aten_.combinationRules());
}

void AtenWindow::on_ForcefieldsExpressionClearButton_clicked(bool checked)
{
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	
	currentModel->removeTyping();

	updateWidgets(AtenWindow::MainViewTarget);
}
