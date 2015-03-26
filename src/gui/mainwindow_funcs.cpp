/*
	*** Qt main window functions
	*** src/gui/mainwindow_funcs.cpp
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

#include <QtGui/QMessageBox>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/build.h"
#include "gui/loadmodel.h"
#include "gui/trajectory.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/about.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "model/undostate.h"
#include "parser/commandnode.h"
#include <QtGui/QFileDialog>
#include <QtGui/QKeyEvent>
#include <QtGui/QProgressBar>
#include "base/sysfunc.h"
#include "main/version.h"
#include <iostream>
#include <fstream>

#include "gui/atomlist.h"
#include "gui/build.h"
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "gui/command.h"
#include "gui/disorderwizard.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/modellist.h"
#include "gui/pores.h"
#include "gui/position.h"
#include "gui/scriptmovie.h"
#include "gui/select.h"
#include "gui/trajectory.h"
#include "gui/transform.h"
#include "gui/vibrations.h"

void AtenWindow::on_TestToolButton_customContextMenuRequested(const QPoint& point)
{
	static TrajectoryWidget twid(*this, Qt::FramelessWindowHint | Qt::Popup);
	twid.show();
	printf("lksjdlkjkl\n");
}

void AtenWindow::on_TestToolButton_clicked(bool checked)
{
	static TrajectoryWidget twid(*this, Qt::FramelessWindowHint | Qt::Popup);
	twid.show();
	printf("Cursor = %i %i\n", QCursor::pos().x(), QCursor::pos().y());
	QPoint toolPos = ui.TestToolButton->parentWidget()->mapToGlobal(ui.TestToolButton->pos()+QPoint(0,ui.TestToolButton->height()));
	printf("Tool = %i %i\n", toolPos.x(), toolPos.y());
	QPoint newPos = mapToGlobal(ui.TestToolButton->pos());
	printf("New = %i %i\n", toolPos.x(), toolPos.y());
	twid.move(toolPos);
	printf("clicked.\n");
}


// Constructor
AtenWindow::AtenWindow(Aten& aten) : QMainWindow(NULL), aten_(aten)
{
	Messenger::enter("AtenWindow::AtenWindow()");

	// Initialise Qt's icons resource
	Q_INIT_RESOURCE(icons);

	// Seutp user interface
	ui.setupUi(this);

	// Set pointer to Aten and AtenWindow in the Viewer
	ui.MainView->setAten(&aten_);
	ui.MainView->setAtenWindow(this);

	// Private variables
	saveModelFilter_ = NULL;
	contextAtom_ = NULL;

	// Public variables
	infoLabel1_ = NULL;
	infoLabel2_ = NULL;
	messageLabel_ = NULL;

	// If no model loaded, add one
	if (aten.nModels() == 0)
	{
		Model* m = aten.addModel();
		m->enableUndoRedo();
		m->regenerateIcon();
	}

	// Create dock widgets
	atomListWidget = new AtomListWidget(*this, Qt::Tool);
	buildWidget = new BuildWidget(*this, Qt::Tool);
	cellDefinitionWidget = new CellDefinitionWidget(*this, Qt::Tool);
	cellTransformWidget = new CellTransformWidget(*this, Qt::Tool);
	commandWidget = new CommandWidget(*this, Qt::Tool);
	disorderWizard = new DisorderWizard(*this);
	forcefieldsWidget = new ForcefieldsWidget(*this, Qt::Tool);
	fragmentsWidget = new FragmentsWidget(*this, Qt::Tool);
	geometryWidget = new GeometryWidget(*this, Qt::Tool);
	glyphsWidget = new GlyphsWidget(*this, Qt::Tool);
	gridsWidget = new GridsWidget(*this, Qt::Tool);
	modelListWidget = new ModelListWidget(*this, Qt::Tool);
	positionWidget = new PositionWidget(*this, Qt::Tool);
	poresWidget = new PoresWidget(*this, Qt::Tool);
	scriptMovieWidget = new ScriptMovieWidget(*this, Qt::Tool);
	selectWidget = new SelectWidget(*this, Qt::Tool);
	trajectoryWidget = new TrajectoryWidget(*this, Qt::Tool);
	transformWidget = new TransformWidget(*this, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(*this, Qt::Tool);
	dockWidgets_ << atomListWidget << buildWidget << cellDefinitionWidget << cellTransformWidget << commandWidget << fragmentsWidget << geometryWidget << glyphsWidget << gridsWidget << modelListWidget << poresWidget << positionWidget << scriptMovieWidget << selectWidget << transformWidget << vibrationsWidget;

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	finaliseUi();

	// Set controls in some windows
	setControls();
	fragmentsWidget->refresh();
	commandWidget->refresh();

	// Refresh the necessary windows, including the mainwindow
	gridsWidget->refresh();
	forcefieldsWidget->refresh();
	cellDefinitionWidget->refresh();
	cellTransformWidget->refresh();
	commandWidget->refreshScripts();
	modelListWidget->refresh();
	atomListWidget->refresh();
	updateControls();
	updateWidgets();

	// Reset view of all loaded models
	for (Model* m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());

	postRedisplay();

	// Set some preferences back to their default values
	prefs.setZMapType(ElementMap::AutoZMap, FALSE);
	prefs.setKeepView(FALSE);

	Messenger::exit("AtenWindow::AtenWindow()");
}

// Destructor
AtenWindow::~AtenWindow()
{
}

/*
 * Aten Reference
 */

// Return reference to Aten
Aten& AtenWindow::aten()
{
	return aten_;
}

/*
 * Window Functions
 */

// Catch window close event
void AtenWindow::closeEvent(QCloseEvent *event)
{
	if (saveBeforeClose())
	{
		saveSettings();
		event->accept();
	}
	else event->ignore();
}


/*
// Methods
*/

// Close specified model, saving first if requested
bool AtenWindow::closeModel(Model* m)
{
	QString text;
	Tree* filter;
	if (m->changeLog.isModified())
	{
		// Create a modal message dialog
		text.sprintf("Model '%s' has been modified.", qPrintable(m->name()));
		int returnvalue = QMessageBox::warning(this, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				break;
				// Cancel close
			case (QMessageBox::Cancel):
				return FALSE;
				// Save model before quit
			case (QMessageBox::Save):
				// Temporarily disable undo/redo for the model, save, and re-enable
				m->disableUndoRedo();
				// If model has a filter set, just save it
				filter = m->filter();
				if (filter != NULL) filter->executeWrite(m->filename());
				else if (runSaveModelDialog())
				{
					m->setFilter(saveModelFilter_);
					m->setFilename(saveModelFilename_);
					if (!saveModelFilter_->executeWrite(saveModelFilename_))
					{
						Messenger::print("Not saved.");
						m->enableUndoRedo();
						return FALSE;
					}
				}
				else
				{
					m->enableUndoRedo();
					return FALSE;
				}
				break;
		}
	}

	// Remove model and update gui
	aten_.removeModel(m);

	// Update GUI
	updateWidgets(AtenWindow::AllTarget);
	
	return TRUE;
}

// Check the status of all models, asking to save before close if necessary
bool AtenWindow::saveBeforeClose()
{
	while (aten_.models())
	{
		if (!closeModel(aten_.models())) return false;
	}
	return true;
}

// Return the PID of Aten
int AtenWindow::pid()
{
#if QT_VERSION >= 0x040400
	return QApplication::applicationPid();
#else
	static int pid = AtenMath::random(50000)+1000;
	return pid;
#endif
}

// Set interactivity (to full or zero), except for main view camera changes
void AtenWindow::setInteractive(bool interactive)
{
	// Set enabled status of all the dock widgets..
	foreach( QObject *obj, dockWidgets_) obj->setProperty("enabled", interactive);

	// ...and the main toolbar...
	ui.MainToolbar->setEnabled(interactive);
	
	// ...and set the canvas 'editability'
	ui.MainView->setEditable(interactive);
}

/*
// Refresh Functions
*/

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenWindow's function)
void AtenWindow::updateWidgets(int targets)
{
	// Refresh aspects of main window and dock widgets
	updateMainWindow();
	updateContextMenu();
	
	if (targets&AtenWindow::ModelsTarget) modelListWidget->refresh();
	if (targets&AtenWindow::GeometryTarget) geometryWidget->refresh();
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

// Refresh widget / scene
void AtenWindow::postRedisplay()
{
// 	if ((!valid_) || drawing_) return;
	ui.MainView->update();
}

// Update GUI after model change (or different model selected)
void AtenWindow::updateMainWindow()
{
	// Update status bar
	QString s;
	Model* m = aten_.currentModel();
	if (m == NULL) return;

	// First label - atom and trajectory frame information
	if (m->hasTrajectory())
	{
		if (m->renderSourceModel() == m)
		{
			s = "(Parent of ";
			s += QString::number(m->nTrajectoryFrames());
			s += " frames) ";
		}
		else
		{
			s = "(Frame ";
			s += QString::number(m->trajectoryFrameIndex()+1);
			s += " of ";
			s += QString::number(m->nTrajectoryFrames());
			s += ") ";
		}
		trajectoryWidget->refresh();
	}
	updateTrajectoryMenu();

	m = m->renderSourceModel();
	s += QString::number(m->nAtoms());
	s += " Atoms ";

	// Add on unknown atom information
	if (m->nUnknownAtoms() != 0)
	{
		s += " (<b>";
		s += QString::number(m->nUnknownAtoms());
		s += " unknown</b>) ";
	}
	if (m->nSelected() != 0)
	{
		s += "(<b>";
		s += QString::number(m->nSelected());
		s += " selected</b>) ";
	}
	s += QString::number(m->mass());
	s += " g mol<sup>-1</sup> ";
	infoLabel1_->setText(s);

	// Second label - cell information
	UnitCell::CellType ct = m->cell()->type();
	if (ct != UnitCell::NoCell)
	{
		s = UnitCell::cellType(ct);
		s += ", ";
		s += QString::number(m->density());
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
	infoLabel2_->setText(s);

	// Update save button status
	ui.actionFileSave->setEnabled( m->changeLog.isModified() );

	// Enable the Atom menu if one or more atoms are selected
	ui.AtomContextMenu->setEnabled( m->renderSourceModel()->nSelected() == 0 ? FALSE : TRUE);

	// Update Undo Redo lists
	updateUndoRedo();

	// Enable/Disable cut/copy/paste/delete based on selection status and clipboard contents
	ui.actionEditPaste->setEnabled( aten_.userClipboard->nAtoms() != 0);
	ui.actionEditPasteTranslated->setEnabled( aten_.userClipboard->nAtoms() != 0);
	ui.actionEditCopy->setEnabled( m->nSelected() != 0 );
	ui.actionEditCut->setEnabled( m->nSelected() != 0 );
	ui.actionEditDelete->setEnabled( m->nSelected() != 0 );

	// Check for empty filters list and enable/disable menu actions accordingly
	ui.actionFileOpen->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.RecentMenu->setEnabled(!aten_.fileDialogFilters(FilterData::ModelImport).isEmpty());
	ui.actionTrajectoryOpen->setEnabled(!aten_.fileDialogFilters(FilterData::TrajectoryImport).isEmpty());
	ui.actionFileSave->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());
	ui.actionFileSaveAs->setEnabled(!aten_.fileDialogFilters(FilterData::ModelExport).isEmpty());
	ui.actionSaveExpression->setEnabled(!aten_.fileDialogFilters(FilterData::ExpressionExport).isEmpty());
	gridsWidget->ui.actionGridLoad->setEnabled(!aten_.fileDialogFilters(FilterData::GridImport).isEmpty());

	// Update main window title
	updateWindowTitle();
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

// Refresh window title
void AtenWindow::updateWindowTitle()
{
	Model* m = aten_.currentModel();
	QString title;
	title.sprintf("Aten (v%s) - %s (%s)%s", ATENVERSION, qPrintable(m->name()), m->filename().isEmpty() ? "<<no filename>>" : qPrintable(m->filename()), m->changeLog.isModified() ? " [Modified]" : "");
	setWindowTitle(title);
}

// Load recent file
void AtenWindow::loadRecent()
{
	QString filename;
	Model* m;
	Tree* filter;

	// Cast sending QAction and grab filename
	QAction* action = qobject_cast<QAction*> (sender());
	if (!action)
	{
		printf("AtenWindow::loadRecent - Sender was not a QAction.\n");
		return;
	}

	// Grab the filename from the action
	filename = action->data().toString();

	// See if any loaded model filename matches this filename
	for (m = aten_.models(); m != NULL; m = m->next)
	{
		Messenger::print(Messenger::Verbose, "Checking loaded models for '%s': %s", qPrintable(filename), qPrintable(m->filename()));
		if (filename == m->filename())
		{
			Messenger::print(Messenger::Verbose, "Matched filename to loaded model.");
			aten_.setCurrentModel(m);
			// Update GUI
			updateWidgets(AtenWindow::AllTarget);
			return;
		}
	}

	// If we get to here then the model is not currently loaded...
	filter = aten_.probeFile(filename, FilterData::ModelImport);
	if (filter != NULL)
	{
		ReturnValue rv;
		filter->executeRead(filename, rv);
		aten_.currentModel()->regenerateIcon();
		// Update GUI
		updateWidgets(AtenWindow::AllTarget);
	}
	else
	{
		// Remove file from recent files list
		int last, n;
		for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
		for (n=last+1; n<MAXRECENTFILES; n++)
			if (actionRecentFile[last]->isVisible())
			{
				actionRecentFile[n-1]->setText(actionRecentFile[n]->text());
				actionRecentFile[n-1]->setData(actionRecentFile[n]->data());
			}
	}
}

// Add file to top of recent list
void AtenWindow::addRecent(QString filename)
{
	// Find unused (i.e. still hidden) recent file action
	int last, n;
	QString temp;
	for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;

	// 'last' now holds the first empty slot in the recent files list.
	// If 'last' == MAXRECENTFILES then shuffle top 'n-1' down a position and add at '0'.
	if (last == MAXRECENTFILES)
	{
		// Push the top items down the list
		for (n=MAXRECENTFILES-2; n>=0; n--)
		{
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
			temp.sprintf("&%i %s", n+1, qPrintable(actionRecentFile[n]->data().toString()));
			actionRecentFile[n+1]->setText(temp);
			actionRecentFile[n+1]->setData(actionRecentFile[n]->data());
		}
		last = 0;
	}

	// Set the new data
	temp.sprintf("&%i %s", last, qPrintable(filename));
	actionRecentFile[last]->setText(temp);
	actionRecentFile[last]->setData(filename);
	actionRecentFile[last]->setVisible(TRUE);
}

void AtenWindow::on_actionAboutAten_triggered(bool checked)
{
	AtenAbout aboutDialog;
	aboutDialog.show();
}

void AtenWindow::on_actionAboutQt_triggered(bool checked)
{
	QMessageBox::aboutQt(this, "About Qt");
}

// Update any controls related to Prefs values etc.
void AtenWindow::updateControls()
{
	ui.actionManualSwapBuffers->setChecked(prefs.manualSwapBuffers());
	ui.actionDetectDisplayHBonds->setChecked(prefs.drawHydrogenBonds());
	buildWidget->ui.BondToleranceSpin->setValue(prefs.bondTolerance());
	buildWidget->ui.BondToleranceSlider->setValue(int(prefs.bondTolerance()*1000.0));
}

// Update undo/redo actions in Edit menu
void AtenWindow::updateUndoRedo()
{
	Model* m = aten_.currentModelOrFrame();
	// Check the model's state pointers
	if (m->currentUndoState() == NULL)
	{
		ui.actionEditUndo->setText("Undo");
		ui.actionEditUndo->setEnabled(FALSE);
	}
	else
	{
		ui.actionEditUndo->setText("Undo (" + m->currentUndoState()->description() + ")");
		ui.actionEditUndo->setEnabled(TRUE);
	}
	if (m->currentRedoState() == NULL)
	{
		ui.actionEditRedo->setText("Redo");
		ui.actionEditRedo->setEnabled(FALSE);
	}
	else
	{
		ui.actionEditRedo->setText("Redo (" + m->currentUndoState()->description() + ")");
		ui.actionEditRedo->setEnabled(TRUE);
	}
}

// Change current user action
void AtenWindow::uaButtonClicked(int id)
{
	QAbstractButton *button;
	// Check button correspondiong to supplied index
	button = uaButtons_.button(id);
	if (button == NULL) printf("Internal Error: AtenWindow::uaButtonClicked - No button associated to id %i\n", id);
	else if (button->isChecked())
	{
		// Activate the relevant mode, bu tonly if it isn't already active
		if (ui.MainView->selectedMode() == (UserAction::Action) id) return;
		ui.MainView->setSelectedMode((UserAction::Action) id);
	}
}

// Set action/button to reflect supplied user action
void AtenWindow::setActiveUserAction(UserAction::Action ua)
{
	// Set (check) relevant action or button based on supplied UserAction
	QAbstractButton *button;
	switch (ua)
	{
		// No active mode
		case (UserAction::NoAction):
			uaDummyButton_->setChecked(TRUE);
			ui.actionNoAction->setChecked(TRUE);
			break;
		// Three select QActions on main ToolBar
		case (UserAction::SelectAction):
		case (UserAction::SelectMoleculeAction):
		case (UserAction::SelectElementAction):
			uaDummyButton_->setChecked(TRUE);
			break;
		// All other actions are related to buttons elsewhere in the GUI
		default:
			ui.actionNoAction->setChecked(TRUE);
			button = uaButtons_.button(ua);
			if (button == NULL) printf("No button associated to user action %i.\n", ua);
			else button->setChecked(TRUE);
			break;
	}
}

// Set message label text
void AtenWindow::setMessageLabel(QString message)
{
	messageLabel_->setText(message);
}
