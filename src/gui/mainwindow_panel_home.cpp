/*
	*** Main Window - Home Panel Functions
	*** src/gui/mainwindow_panel_home.cpp
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

#include "gui/mainwindow.h"
#include "main/aten.h"
#include "gui/exportfilm.h"
#include "gui/exportimage.h"
#include "gui/openmodel.h"
#include "undo/undostate.h"
#include <QFileDialog>

// Static local variables
Matrix storedView;

// Update home panel
void AtenWindow::updateHomePanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateHomePanel");

	// File
	ui.HomeFileOpenButton->setEnabled(aten_.pluginStore().nFilePlugins(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin) > 0);
	ui.HomeFileSaveButton->setEnabled((aten_.pluginStore().nFilePlugins(PluginTypes::ModelFilePlugin, PluginTypes::ExportPlugin) > 0) && sourceModel && sourceModel->isModified());
	ui.HomeFileSaveAsButton->setEnabled(aten_.pluginStore().nFilePlugins(PluginTypes::ModelFilePlugin, PluginTypes::ImportPlugin) > 0);

	// Edit
	ui.HomeEditPasteButton->setEnabled( sourceModel && aten_.userClipboard->nAtoms() != 0);
	ui.HomeEditCopyButton->setEnabled( sourceModel ? sourceModel->nSelected() != 0 : false );
	ui.HomeEditCutButton->setEnabled( sourceModel ? sourceModel->nSelected() != 0 : false );
	ui.HomeEditDeleteButton->setEnabled( sourceModel ? sourceModel->nSelected() != 0 : false );
	// -- Update undo / redo state
	if (sourceModel && sourceModel->currentUndoState())
	{
		ui.HomeEditUndoButton->setToolTip("Undo (" + sourceModel->currentUndoState()->description() + ")");
		ui.HomeEditUndoButton->setEnabled(true);
	}
	else
	{
		ui.HomeEditUndoButton->setToolTip("(Nothing to Undo)");
		ui.HomeEditUndoButton->setEnabled(false);
	}
	if (sourceModel && sourceModel->currentRedoState())
	{
		ui.HomeEditRedoButton->setToolTip("Redo (" + sourceModel->currentRedoState()->description() + ")");
		ui.HomeEditRedoButton->setEnabled(true);
	}
	else
	{
		ui.HomeEditRedoButton->setToolTip("(Nothing to Redo)");
		ui.HomeEditRedoButton->setEnabled(false);
	}

	// View
	ui.HomeAppearancePerspectiveButton->setChecked(prefs.hasPerspective());
	ui.HomeViewHBondsButton->setChecked(prefs.drawHydrogenBonds());
	ui.HomeViewCorrectGridsButton->setChecked(prefs.correctTransparentGrids());
	ui.HomeViewLockButton->setChecked(prefs.viewLock() == Prefs::FullLock);
	if (sourceModel) TMenuButton::setGroupButtonChecked("ViewStyles", sourceModel->drawStyle());
	if (sourceModel) TMenuButton::setGroupButtonChecked("ColourSchemes", sourceModel->colourScheme());

	Messenger::exit("AtenWindow::updateHomePanel");
}

/*
 * File
 */

void AtenWindow::on_HomeFileNewButton_clicked(bool checked)
{
	Model* m = aten_.addModel();
	m->enableUndoRedo();

	// Update GUI
	aten_.setCurrentModel(m);

	updateWidgets(AtenWindow::AllTargets);
}

void AtenWindow::on_HomeFileOpenButton_clicked(bool checked)
{
	if (openModelDialog_.execute(aten_.pluginStore().logPoint()))
	{
		// Open model(s) selected in dialog
		QStringList filesToLoad = openModelDialog_.selectedFilenames();
		const FilePluginInterface* plugin = openModelDialog_.selectedPlugin();
		for (int n=0; n<filesToLoad.count(); ++n) aten_.importModel(filesToLoad.at(n), plugin, openModelDialog_.standardImportOptions(), openModelDialog_.selectedPluginOptions());

		aten().setSingleModelVisible(aten().currentModel());

		updateWidgets(AtenWindow::AllTargets);
	}
}

void AtenWindow::on_HomeFileSaveButton_clicked(bool checked)
{
	Model* m = aten_.currentModelOrFrame();

	aten_.exportModel(m, m->filename(), m->plugin());

	updateWidgets();
}

void AtenWindow::on_HomeFileSaveAsButton_clicked(bool checked)
{
	Model* m = aten_.currentModelOrFrame();

	aten_.exportModel(m, m->filename(), NULL);

	updateWidgets();
}

void AtenWindow::on_HomeFileCloseButton_clicked(bool checked)
{
	closeModel(aten_.currentModel());

	// Check for there being no models
	if (aten_.nModels() == 0)
	{
		aten_.setCurrentModel(aten_.addModel());
	}

	// Update GUI
	updateWidgets(AtenWindow::AllTargets);
}

void AtenWindow::on_HomeFileImageButton_clicked(bool checked)
{
	if (!exportImageDialog_.getImageDetails()) return;

	// Get values from dialog
	int imageWidth = exportImageDialog_.imageWidth();
	int imageHeight = exportImageDialog_.imageHeight();
	QString fileName = exportImageDialog_.fileName();
	if (fileName.isEmpty()) return;

	QPixmap pixmap = scenePixmap(imageWidth, imageHeight, exportImageDialog_.imageTransparent());
	pixmap.save(fileName, exportImageDialog_.imageFormat(), -1);
}

void AtenWindow::on_HomeFileFilmButton_clicked(bool checked)
{
	if (!exportFilmDialog_.getFilmDetails()) return;

	exportFilm();
}

/*
 * Edit
 */

void AtenWindow::on_HomeEditCopyButton_clicked(bool checked)
{
	CommandNode::run(Commands::Copy, "");

	updateWidgets();
}

void AtenWindow::on_HomeEditCutButton_clicked(bool checked)
{
	CommandNode::run(Commands::Cut, "");

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditPasteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Paste, "");

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditDeleteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Delete, "");

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditUndoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Undo, "");

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_HomeEditRedoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Redo, "");

	updateWidgets(AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}


/*
 * Control
 */

void AtenWindow::on_HomeViewResetButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());
	updateWidgets();
}

// Get current view
void AtenWindow::on_HomeViewGetButton_clicked(bool checked)
{
	// Get view from current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	storedView = currentModel->modelViewMatrix();
}

// Set current view (from stored view)
void AtenWindow::on_HomeViewSetButton_clicked(bool checked)
{
	// Run command
	CommandNode::run(Commands::SetView, "dddddddddddd", storedView[0], storedView[1], storedView[2], storedView[4], storedView[5], storedView[6], storedView[8], storedView[9], storedView[10], storedView[12], storedView[13], storedView[14]);

	updateWidgets();
}

// Toggle detection of H-Bonds
void AtenWindow::on_HomeViewHBondsButton_clicked(bool checked)
{
	if (refreshing_) return;
	
	prefs.setDrawHydrogenBonds(checked);

	updateWidgets();
}

// Toggle automatic correction of (transparent) grid data
void AtenWindow::on_HomeViewCorrectGridsButton_clicked(bool checked)
{
	if (refreshing_) return;
	
	prefs.setCorrectTransparentGrids(checked);

	if (prefs.correctTransparentGrids())
	{
		for (Model* m = aten_.models(); m != NULL; m = m->next) m->updateGridAxisOrdering();
	}

	updateWidgets();
}

void AtenWindow::on_HomeViewLockButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get view from current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	currentModel->setCommonViewMatrixFromLocal();

	prefs.setViewLock(checked ? Prefs::FullLock : Prefs::NoLock);

	updateWidgets();
}

/*
 * Appearance
 */

void AtenWindow::on_HomeAppearanceLineButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model drawing style");
	currentModel->setDrawStyle(Prefs::LineStyle);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceTubeButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model drawing style");
	currentModel->setDrawStyle(Prefs::TubeStyle);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceSphereButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model drawing style");
	currentModel->setDrawStyle(Prefs::SphereStyle);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceScaledButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model drawing style");
	currentModel->setDrawStyle(Prefs::ScaledStyle);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceOwnStyleButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model drawing style");
	currentModel->setDrawStyle(Prefs::OwnStyle);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceElementButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::ElementScheme);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceChargeButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::ChargeScheme);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceForceButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::ForceScheme);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceVelocityButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::VelocityScheme);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceBondsButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::BondsScheme);
	currentModel->endUndoState();

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceOwnColourButton_clicked(bool checked)
{
	if (!checked) return;

	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;

	currentModel->beginUndoState("Change model colour scheme");
	currentModel->setColourScheme(Prefs::OwnScheme);
	currentModel->endUndoState();

	updateWidgets();
}

// Set perspective view
void AtenWindow::on_HomeAppearancePerspectiveButton_clicked(bool checked)
{
	if (refreshing_) return;

	prefs.setPerspective(checked);

	updateWidgets();
}

void AtenWindow::on_HomeAppearanceShowAllButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShowAll, "");

	updateWidgets(AtenWindow::AtomsTableTarget);
}
