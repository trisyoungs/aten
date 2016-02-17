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
#include "gui/selectfilter.h"
#include "gui/loadmodel.h"
#include "gui/exportfilm.h"
#include "gui/exportimage.h"
#include "model/undostate.h"
#include <QFileDialog>

// Static local variables
Matrix storedView;

// Update home panel
void AtenWindow::updateHomePanel(Model* sourceModel)
{
	Messenger::enter("AtenWindow::updateHomePanel");

	// File
	ui.HomeFileOpenButton->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.HomeFileSaveButton->setEnabled((!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty()) && sourceModel && sourceModel->isModified());
	ui.HomeFileSaveAsButton->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());

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

	// Appearance
	ui.HomeAppearancePerspectiveButton->setChecked(prefs.hasPerspective());
	ui.HomeViewHBondsButton->setChecked(prefs.drawHydrogenBonds());  //ATEN2 TODO Move this from Prefs?
	ui.HomeViewLockButton->setChecked(Model::useCommonModelViewMatrix());
	TMenuButton::setGroupButtonChecked("ViewStyles", Prefs::drawStyle(prefs.renderStyle()));
	TMenuButton::setGroupButtonChecked("ColourSchemes", Prefs::colouringScheme(prefs.colourScheme()));

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

	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_HomeFileOpenButton_clicked(bool checked)
{
	AtenLoadModel loadModelDialog(*this);
	Tree* filter;

	if (loadModelDialog.updateAndExec() == 1)
	{
		bool result = aten_.loadModel(loadModelDialog.selectedFilename(), loadModelDialog.selectedFormat());
		if (result)
		{
			aten().setSingleModelVisible(aten().currentModel());

			updateWidgets(AtenWindow::AllTarget);
		}
	}
}

void AtenWindow::on_HomeFileSaveButton_clicked(bool checked)
{
	// Check the filter of the current model
	// If there isn't one, or it can't export, raise the file dialog.
	// Similarly, if no filename has been set, raise the file dialog.
	Model* m = aten_.currentModelOrFrame();
	Tree* t = m->filter();
	if ((t != NULL) && (t->filter.type() != FilterData::ModelExport)) t = NULL;
	QString filename;
	filename = m->filename();
	if (filename.isEmpty() || (t == NULL))
	{
		if (runSaveModelDialog())
		{
			m->setFilter(saveModelFilter_);
			m->setFilename(saveModelFilename_);
			// Temporarily disable undo/redo for the model, save, and re-enable
			m->disableUndoRedo();
			if (saveModelFilter_->executeWrite(saveModelFilename_))
			{
				m->updateSavePoint();
				Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(saveModelFilename_), qPrintable(saveModelFilter_->filter.name()));
			}
			else Messenger::print("Failed to save model '%s'.", qPrintable(m->name()));
			m->enableUndoRedo();
		}
	}
	else
	{
		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		t->executeWrite(filename);
		m->updateSavePoint();
		m->enableUndoRedo();
	}

	updateWidgets();
}

void AtenWindow::on_HomeFileSaveAsButton_clicked(bool checked)
{
	Model* m;
	if (runSaveModelDialog())
	{
		m = aten_.currentModelOrFrame();
		if (m == NULL)
		{
			printf("Internal Error: Model pointer is NULL in AtenWindow::on_actionFileSaveAs_triggered.\n");
			return;
		}
		m->setFilter(saveModelFilter_);
		m->setFilename(saveModelFilename_);

		// Temporarily disable undo/redo for the model, save, and re-enable
		m->disableUndoRedo();
		
		if (saveModelFilter_->executeWrite(saveModelFilename_))
		{
			m->updateSavePoint();
			Messenger::print("Model '%s' saved to file '%s' (%s)", qPrintable(m->name()), qPrintable(saveModelFilename_), qPrintable(saveModelFilter_->filter.name()));
		}
		else Messenger::print("Failed to save model '%s'.", qPrintable(m->name()));
		m->enableUndoRedo();
		updateWidgets();
	}
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
	updateWidgets(AtenWindow::AllTarget);
}

void AtenWindow::on_HomeFileImageButton_clicked(bool checked)
{
	if (!exportImageDialog_.getImageDetails()) return;

	// Get values from dialog
	int imageWidth = exportImageDialog_.ui.ImageWidthSpin->value();
	int imageHeight = exportImageDialog_.ui.ImageHeightSpin->value();
	QString fileName = exportImageDialog_.fileName();
	if (fileName.isEmpty()) return;

	QPixmap pixmap = scenePixmap(imageWidth, imageHeight);
	AtenWindow::BitmapFormat bf = (AtenWindow::BitmapFormat) exportImageDialog_.ui.ImageFormatCombo->currentIndex();
	pixmap.save(fileName, AtenWindow::bitmapFormatExtension(bf), -1);
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

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeEditCutButton_clicked(bool checked)
{
	CommandNode::run(Commands::Cut, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditPasteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Paste, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditDeleteButton_clicked(bool checked)
{
	CommandNode::run(Commands::Delete, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget);
}

void AtenWindow::on_HomeEditUndoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Undo, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}

void AtenWindow::on_HomeEditRedoButton_clicked(bool checked)
{
	CommandNode::run(Commands::Redo, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget+AtenWindow::SelectPanelTarget+AtenWindow::GlyphsTarget);
}


/*
 * Control
 */

void AtenWindow::on_HomeViewResetButton_clicked(bool checked)
{
	aten_.currentModelOrFrame()->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());
	updateWidgets(AtenWindow::MainViewTarget);
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

	updateWidgets(AtenWindow::MainViewTarget);
}

// Toggle detection of H-Bonds
void AtenWindow::on_HomeViewHBondsButton_clicked(bool checked)
{
	if (refreshing_) return;
	
	prefs.setDrawHydrogenBonds(checked);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeViewLockButton_clicked(bool checked)
{
	if (refreshing_) return;

	// Get view from current model
	Model* currentModel = aten_.currentModelOrFrame();
	if (!currentModel) return;
	currentModel->setCommonViewMatrixFromLocal();

	Model::setUseCommonModelViewMatrix(checked);


	updateWidgets(AtenWindow::MainViewTarget);
}

/*
 * Appearance
 */

void AtenWindow::on_HomeAppearanceLineButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::LineStyle);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceTubeButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::TubeStyle);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceSphereButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::SphereStyle);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceScaledButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::ScaledStyle);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceOwnStyleButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setRenderStyle(Prefs::OwnStyle);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceElementButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setColourScheme(Prefs::ElementScheme);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceChargeButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setColourScheme(Prefs::ChargeScheme);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceForceButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setColourScheme(Prefs::ForceScheme);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceVelocityButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setColourScheme(Prefs::VelocityScheme);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceOwnColourButton_clicked(bool checked)
{
	if (!checked) return;

	prefs.setColourScheme(Prefs::OwnScheme);
	aten_.globalLogChange(Log::Style);

	updateWidgets(AtenWindow::MainViewTarget);
}

// Set perspective view
void AtenWindow::on_HomeAppearancePerspectiveButton_clicked(bool checked)
{
	if (refreshing_) return;

	prefs.setPerspective(checked);

	updateWidgets(AtenWindow::MainViewTarget);
}

void AtenWindow::on_HomeAppearanceShowAllButton_clicked(bool checked)
{
	CommandNode::run(Commands::ShowAll, "");

	updateWidgets(AtenWindow::MainViewTarget+AtenWindow::AtomsTableTarget);
}
