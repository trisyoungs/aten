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
#include "gui/grids.h"
#include "gui/trajectory.h"
#include "gui/select.h"
#include "gui/forcefields.h"
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "gui/vibrations.h"
#include "gui/atomlist.h"
#include "gui/glyphs.h"
#include <QtWidgets/QMessageBox>

// Update and show
void AtenWindow::initialUpdateAndShow()
{
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

	// Update model list
	updateModelList();

	// Update everything else
	updateMainWindow();
}


// Update GUI after model change (or different model selected)
void AtenWindow::updateMainWindow()
{
	refreshing_ = true;

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
		updateTrajectoryMenu();	// ATEN2 TODO

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
		UnitCell::CellType ct = currentModel->cell()->type();
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

	// Update save button status
	ui.actionFileSave->setEnabled( currentModel ? currentModel->isModified() : false );

	// Enable the Atom menu if one or more atoms are selected
	ui.AtomContextMenu->setEnabled( currentModel ? currentModel->renderSourceModel()->nSelected() != 0 : false );

	// Update Undo Redo lists
	updateUndoRedo();

	// Enable/Disable cut/copy/paste/delete based on selection status and clipboard contents
	ui.actionEditPaste->setEnabled( aten_.userClipboard->nAtoms() != 0);
	ui.actionEditPasteTranslated->setEnabled( aten_.userClipboard->nAtoms() != 0);
	ui.actionEditCopy->setEnabled( currentModel ? currentModel->nSelected() != 0 : false );
	ui.actionEditCut->setEnabled( currentModel ? currentModel->nSelected() != 0 : false );
	ui.actionEditDelete->setEnabled( currentModel ? currentModel->nSelected() != 0 : false );

	// Check for empty filters list and enable/disable menu actions accordingly
	ui.actionFileOpen->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.RecentMenu->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.actionTrajectoryOpen->setEnabled(!aten_.fileDialogFilters(FilterData::TrajectoryImport).isEmpty());
	ui.actionFileSave->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());
	ui.actionFileSaveAs->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());
	ui.actionSaveExpression->setEnabled(!aten_.fileDialogFilters(FilterData::ExpressionExport).isEmpty());
	gridsWidget->ui.actionGridLoad->setEnabled(!aten_.fileDialogFilters(FilterData::GridImport).isEmpty());

	// Update main window title
	QString title = QString("Aten v2 PRERELEASE (v%1)").arg(ATENVERSION);
	if (currentModel) title += QString(" - %1 (%2)%3").arg(currentModel->name(), currentModel->filename().isEmpty() ? "<<no filename>>" : currentModel->filename(), currentModel->isModified() ? " [Modified]" : "");
	else title += " [[[ No Current Model ]]]";
	setWindowTitle(title);

	// Build panel
	/* nothing */

	// Cell panel
	ui.CellDefinePeriodicButton->setEnabled(currentModel);
	if (currentModel) ui.CellDefinePeriodicButton->setChecked(currentModel->cell()->type() != UnitCell::NoCell);
	ui.CellDefineAnglesButton->setEnabled(currentModel ? currentModel->cell()->type() != UnitCell::NoCell : false);
	ui.CellDefineLengthsButton->setEnabled(currentModel ? currentModel->cell()->type() != UnitCell::NoCell : false);
	ui.CellDefineMatrixButton->setEnabled(currentModel ? currentModel->cell()->type() != UnitCell::NoCell : false);

	refreshing_ = false;
}

// Update trajectory menu
void AtenWindow::updateTrajectoryMenu()
{
	// First see if the model has a trajectory associated to it
	Model* m = aten_.currentModel();
	Model::RenderSource rs = m->renderSource();
	bool hasTrj = (m->nTrajectoryFrames() != 0);
	int frameNAtoms = hasTrj ? m->trajectoryCurrentFrame()->nAtoms() : -1;
	ui.actionTrajectoryRemove->setEnabled(hasTrj);
	ui.actionTrajectoryInheritParentStyle->setChecked(m->trajectoryPropagateParentStyle());
	ui.actionTrajectoryInheritParentStyle->setEnabled(m->nAtoms() == frameNAtoms);
	ui.actionTrajectoryCopyStyleToParent->setEnabled((rs == Model::TrajectorySource) && (m->nAtoms() == frameNAtoms));
	ui.actionTrajectoryPropagateStyleFromHere->setEnabled((rs == Model::TrajectorySource) && m->trajectoryIsCached());
	ui.actionTrajectoryFirstFrame->setEnabled(hasTrj);
	ui.actionTrajectoryLastFrame->setEnabled(hasTrj);
	ui.actionTrajectoryPlayPause->setEnabled(hasTrj);
	ui.actionTrajectoryPlayPause->setChecked(trajectoryWidget->ui.TrajectoryPlayPauseButton->isChecked());
	ui.actionTrajectoryFrames->setEnabled(hasTrj);
	ui.actionTrajectorySaveMovie->setEnabled(hasTrj);

	// Select the correct view action
	ui.actionTrajectoryModel->setChecked(rs == Model::ModelSource);
	ui.actionTrajectoryFrames->setChecked(rs == Model::TrajectorySource);
}


// Update GUI after model change (or different model selected) (accessible wrapper to call AtenWindow's function)
void AtenWindow::updateWidgets(int targets)
{
	// Refresh aspects of main window and dock widgets
	updateMainWindow();
	updateContextMenu();
	
	if (targets&AtenWindow::SelectTarget) selectWidget->refresh();
	if (targets&AtenWindow::VibrationsTarget) vibrationsWidget->refresh();
	if (targets&AtenWindow::TrajectoryTarget) trajectoryWidget->refresh();

	// Update contents of the atom list
	if (targets&AtenWindow::AtomsTarget) atomListWidget->refresh();

	// Update contents of the glyph list
	if (targets&AtenWindow::GlyphsTarget) glyphsWidget->refresh();

	// Update contents of the grid window
	if (targets&AtenWindow::GridsTarget) gridsWidget->refresh();

	// Update the contents of the cell page
	if (targets&AtenWindow::CellTarget)
	{
		cellDefinitionWidget->refresh();
		cellTransformWidget->refresh();
	}

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
	
	// Request redraw of the main canvas
	if (targets&AtenWindow::CanvasTarget) postRedisplay();
}

// Refresh Viewer widget, and any associated widgets
void AtenWindow::postRedisplay()
{
	ui.MainView->update();
}
