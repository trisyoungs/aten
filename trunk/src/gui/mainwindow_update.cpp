/*
	*** Main Window - Update Functions
	*** src/gui/mainwindow_update.cpp
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

#include "main/version.h"
#include "gui/mainwindow.h"
#include "main/aten.h"
#include "gui/forcefields.h"
#include "gui/vibrations.h"
#include "gui/glyphs.h"
#include <QtWidgets/QMessageBox>

// Update GUI after model change (or different model selected)
void AtenWindow::updateMainWindow()
{
	// Get current model
	Model* currentModel = aten_.currentModel();

	// Update status bar
	QString s;
	if (currentModel)
	{
		// First label - atom and trajectory frame information
		if (currentModel->hasTrajectory())
		{
			if (currentModel->renderSourceModel() == currentModel) s = "(Parent of " + QString::number(currentModel->nTrajectoryFrames()) + " frames) ";
			else s = "(Frame " + QString::number(currentModel->trajectoryFrameIndex()+1) + " of " + QString::number(currentModel->nTrajectoryFrames()) + ") ";
		}

		currentModel = currentModel->renderSourceModel();
		s += QString::number(currentModel->nAtoms());
		s += " Atoms ";

		// Add on unknown atom information
		if (currentModel->nUnknownAtoms() != 0) s += " (<b>" + QString::number(currentModel->nUnknownAtoms()) + " unknown</b>) ";
		if (currentModel->nSelected() != 0) s += "(<b>" + QString::number(currentModel->nSelected()) + " selected</b>) ";
		s += QString::number(currentModel->mass());
		s += " g mol<sup>-1</sup> ";
		infoLabel1_->setText(s);

		// Second label - cell information
		UnitCell::CellType ct = currentModel->cell().type();
		if (ct != UnitCell::NoCell)
		{
			s = QString("%1, %2").arg(UnitCell::cellType(ct)).arg(currentModel->density());
			switch (prefs.densityUnit())
			{
				case (Prefs::GramsPerCm):
					s += " g cm<sup>-3</sup>";
					break;
				case (Prefs::AtomsPerAngstrom):
					s += " atoms &#8491;<sup>-3</sup>";
					break;
				default:
					break;
			}
		}
		else s = "Non-periodic";
	}
	else s = "(No Model)";
	infoLabel2_->setText(s);

	// Check for empty filters list and enable/disable menu actions accordingly
	ui.RecentMenu->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());

	ui.actionSaveExpression->setEnabled(!aten_.fileDialogFilters(FilterData::ExpressionExport).isEmpty());

	// Update main window title
	QString title = QString("Aten v2 PRERELEASE (v%1)").arg(ATENVERSION);
	if (currentModel) title += QString(" - %1 (%2)%3").arg(currentModel->name(), currentModel->filename().isEmpty() ? "<<no filename>>" : currentModel->filename(), currentModel->isModified() ? " [Modified]" : "");
	else title += " [[[ No Current Model ]]]";
	setWindowTitle(title);
}

// Update and show
void AtenWindow::initialUpdateAndShow()
{
	Messenger::enter("AtenWindow::initialUpdateAndShow");

	// Display message box warning if there was a filter load error
	if (aten_.nFilterPrograms() == 0)
	{
		QMessageBox::warning(NULL, "Aten", "Filters could not be found.\nNo import/export will be possible.\nSet the environment variable ATENDATA to point to Aten's data directory (e.g. 'export ATENDATA=/usr/local/aten/data'), or run with --atendata <dir>.\n", QMessageBox::Ok, QMessageBox::Ok);
	}
	else if (aten_.failedFilters().count() > 0)
	{
		// Construct the messagebox text
		QString text("One or more filters could not be loaded properly on startup.\nCheck shell output or run Settings->Reload Filters to diagnose the problem.\nFilters with errors were:\n");
		for (int n=0; n<aten_.failedFilters().count(); ++n)
		{
			text += "\t";
			text += aten_.failedFilters().at(n) + "\n";
		}
		QMessageBox::warning(NULL, "Aten", text, QMessageBox::Ok, QMessageBox::Ok);
	}

	// Show the window
	show();

	// Update everything else
	updateWidgets(AtenWindow::AllTarget);

	Messenger::exit("AtenWindow::initialUpdateAndShow");
}

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenWindow's function)
void AtenWindow::updateWidgets(int targets)
{
	Messenger::enter("AtenWindow::updateWidgets");

	refreshing_ = true;

	// Always update main window bottom-left status info, menu items, and titlebar
	updateMainWindow();

	// Update main view
	if (targets&AtenWindow::MainViewTarget) ui.MainView->update();

	// Update model list
	if (targets&AtenWindow::ModelsListTarget) updateModelsList();

	// Get current model
	Model* currentModel = aten_.currentModelOrFrame();

	// Always update Home panel
	updateHomePanel(currentModel);

	// Enable / disable Selection tab
	ui.SelectionTab->setEnabled(currentModel && currentModel->nSelected());

	// Update atoms table
	if (targets&AtenWindow::AtomsTableTarget) updateAtomsTable(currentModel);

	// Panels
	if (targets&AtenWindow::BuildPanelTarget) updateBuildPanel(currentModel);
	if (targets&AtenWindow::CellPanelTarget) updateCellPanel(currentModel);
	if (targets&AtenWindow::CalculatePanelTarget) updateCalculatePanel(currentModel);
	if (targets&AtenWindow::TransformPanelTarget) updateTransformPanel(currentModel);
	if (targets&AtenWindow::GridsPanelTarget) updateGridsPanel(currentModel);
	if (targets&AtenWindow::TrajectoryPanelTarget) updateTrajectoryPanel(currentModel);
	if (targets&AtenWindow::SelectPanelTarget) updateSelectPanel(currentModel);

	
	
	if (targets&AtenWindow::VibrationsTarget) vibrationsWidget->refresh();

	// Update contents of the atom list

	// Update contents of the glyph list
	if (targets&AtenWindow::GlyphsTarget) glyphsWidget->refresh();

	// Update forcefields in the forcefield widget
	if (targets&AtenWindow::ForcefieldsTarget) forcefieldsWidget->refresh();

	if (targets&AtenWindow::StatusBarTarget)
	{
		QString text;
		static UserAction::Action lastAction = UserAction::NoAction;
		
		// Initialise string if NoAction
		if (lastAction == UserAction::NoAction) text.clear();
		
		// If current action is not the same as the last action, recreate string
		if (lastAction != ui.MainView->selectedMode())
		{
			lastAction = ui.MainView->selectedMode();
			text.sprintf("<b>%s:</b> %s", UserActions[lastAction].name, UserActions[lastAction].unModified);
			if (UserActions[lastAction].shiftModified[0] != '\0') text += ", <b>+shift</b> %s" + QString(UserActions[lastAction].shiftModified);
			if (UserActions[lastAction].ctrlModified[0] != '\0') text += ", <b>+ctrl</b> %s" + QString(UserActions[lastAction].ctrlModified);
			if (UserActions[lastAction].altModified[0] != '\0') text += ", <b>+alt</b> %s" + QString(UserActions[lastAction].altModified);
		}

		// Set text in statusbar widget
		this->setMessageLabel(text);
	}

	refreshing_ = false;

	Messenger::exit("AtenWindow::updateWidgets");
}
