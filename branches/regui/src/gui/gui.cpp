/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007,2008

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

#include "base/master.h"
#include "gui/canvas.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/selectpattern.h"
#include "gui/ffeditor.h"
#include "gui/tcanvas.uih"
#include "gui/grids.h"
#include "gui/disorder.h"
#include "gui/atomlist.h"
#include "gui/forcefields.h"
#include "gui/celldefine.h"
#include "gui/celltransform.h"
#include "gui/build.h"
#include "gui/glyphs.h"
#include "gui/minimiser.h"
#include "gui/transform.h"
#include "gui/position.h"
#include "model/model.h"
#include <QtGui/QMessageBox>
#include <QtCore/QTextStream>
#include <QtGui/QProgressBar>

// External Declarations
GuiQt gui;

// Constructor
GuiQt::GuiQt()
{
	mainWidget = NULL;
	mainWindow = NULL;
	prefsDialog = NULL;
	loadModelDialog = NULL;
	selectPatternDialog = NULL;
	doesExist_ = FALSE;
	isAvailable_ = FALSE;
	trajectoryPlaying_ = FALSE;
	trajectoryTimerId_ = -1;
}

// Initialise QApplication and the main QGlWidget
void GuiQt::initialise(int &argc, char **argv)
{
	// Create the QApplication
	app = new QApplication(argc, argv);
	// Create the QGLWidget
	mainWidget = new TCanvas(NULL);
	mainWidget->setGeometry(0,0,800,600);
	// Set the main gui widgetcanvas to be associated to the GUIs TCanvas (and vice versa)
	mainView.setWidget(mainWidget);
	mainWidget->setCanvas(&mainView);
	mainView.enableDrawing();
}

// Initialise and create GUI
void GuiQt::run()
{
	dbgBegin(Debug::Calls,"GuiQt::run");

	// Initialise Qt's icons resource

	Q_INIT_RESOURCE(icons);

	// Create GUI window, dialog windows, and sub windows
	mainWindow = new AtenForm;
	// ...dialog windows...
	prefsDialog = new AtenPrefs(mainWindow);
	forcefieldEditorDialog = new AtenForcefieldEditor(mainWindow);
	loadModelDialog = new AtenLoadModel(mainWindow);
	selectPatternDialog = new AtenSelectPattern(mainWindow);
	// ...tool windows
	atomlistDialog = new AtenAtomlist(mainWindow, Qt::Dialog|Qt::Tool);
	buildDialog = new AtenBuild(mainWindow);
	cellDefineDialog = new AtenCellDefine(mainWindow);
	cellTransformDialog = new AtenCellTransform(mainWindow);
	disorderDialog = new AtenDisorder(mainWindow);
	forcefieldsDialog = new AtenForcefields(mainWindow);
	glyphsDialog = new AtenGlyphs(mainWindow);
	gridsDialog = new AtenGrids(mainWindow);
	minimiserDialog = new AtenMinimiser(mainWindow);
	positionDialog = new AtenPosition(mainWindow);
	transformDialog = new AtenTransform(mainWindow);

	// Connect Finished signal of tool windows to finished slots in structure
	QObject::connect(atomlistDialog, SIGNAL(finished(int)), atomlistDialog, SLOT(dialogFinished(int)));
	QObject::connect(buildDialog, SIGNAL(finished(int)), buildDialog, SLOT(dialogFinished(int)));
	QObject::connect(cellDefineDialog, SIGNAL(finished(int)), cellDefineDialog, SLOT(dialogFinished(int)));
	QObject::connect(cellTransformDialog, SIGNAL(finished(int)), cellTransformDialog, SLOT(dialogFinished(int)));
	QObject::connect(disorderDialog, SIGNAL(finished(int)), disorderDialog, SLOT(dialogFinished(int)));
	QObject::connect(forcefieldsDialog, SIGNAL(finished(int)), forcefieldsDialog, SLOT(dialogFinished(int)));
	QObject::connect(glyphsDialog, SIGNAL(finished(int)), glyphsDialog, SLOT(dialogFinished(int)));
	QObject::connect(gridsDialog, SIGNAL(finished(int)), gridsDialog, SLOT(dialogFinished(int)));
	QObject::connect(minimiserDialog, SIGNAL(finished(int)), minimiserDialog, SLOT(dialogFinished(int)));
	QObject::connect(positionDialog, SIGNAL(finished(int)), positionDialog, SLOT(dialogFinished(int)));
	QObject::connect(transformDialog, SIGNAL(finished(int)), transformDialog, SLOT(dialogFinished(int)));

	// Set the modality of some dialogs
	prefsDialog->setModal(TRUE);
	forcefieldEditorDialog->setModal(TRUE);
	loadModelDialog->setModal(TRUE);
	selectPatternDialog->setModal(TRUE);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainWindow->finaliseUi();
	prefsDialog->finaliseUi();
	forcefieldEditorDialog->finaliseUi();
	loadModelDialog->finaliseUi();
	selectPatternDialog->finaliseUi();

	// Temporarily disable drawing on the main canvas again
	gui.mainView.disableDrawing();

	// Set controls in the windows
	mainWindow->setControls();
	prefsDialog->setControls();
	forcefieldEditorDialog->setControls();
	loadModelDialog->setControls();
	selectPatternDialog->setControls();

	// Show the widgets in the GUI and flag it as existing
	mainWindow->show();
	doesExist_ = TRUE;

	// Make first loaded model the current one
	master.setCurrentModel(master.models());

	// Refresh the necessary windows
	gridsDialog->refresh();
	forcefieldsDialog->refresh();
	disorderDialog->refresh();
	cellDefineDialog->refresh();
	cellTransformDialog->refresh();
	updateTrajControls();

	gui.mainView.enableDrawing();

	// Add loaded models to tabbar (and reset its view while we're here)
	int tabid;
	for (Model *m = master.models(); m != NULL; m = m->next)
	{
		tabid = mainWindow->ui.ModelTabs->addTab(m->name());
		m->resetView();
		m->calculateViewMatrix();
		m->projectAll();
	}

	gui.mainView.postRedisplay();

	int n = app->exec();

	dbgEnd(Debug::Calls,"GuiQt::run");
}

// Returns if the GUI has been created
bool GuiQt::exists()
{
	return doesExist_;
}

/*
// Models
*/

// Add model to GUI list, in this case a tab in the ModelTabs widget
void GuiQt::addModel(Model *m)
{
	if (!doesExist_) return;
	// Create new tab in ModelTabs QTabBar
	int tabid = mainWindow->ui.ModelTabs->addTab(m->name());
	mainWindow->ui.ModelTabs->setCurrentIndex(tabid);
	m->resetView();
	gui.modelChanged(TRUE,TRUE,TRUE);
}

// Remove model from list
void GuiQt::removeModel(int id)
{
	if (!doesExist_) return;
	mainWindow->ui.ModelTabs->removeTab(id);
	gui.modelChanged();
}

/* Redraw main window canvas
void GuiQt::refresh()
{
	if (!doesExist_) return;
	// Select tab corresponding to current mdoel
	int id = master.currentModelIndex();
	if (id <= mainWindow->ui.ModelTabs->count()) mainWindow->ui.ModelTabs->setCurrentIndex(id);
	else printf("GUI_ERROR: Current model index (%i) is out of bounds of tab list.\n",id);
	// Update the disorder page
	mainWindow->refreshDisorderPage();
	// Update pattern list in forcefield window
	mainWindow->refreshForcefieldPatterns();
	// Enable View->Trajectory menu item if a trajectory is associated
	mainWindow->ui.actionViewTrajectory->setEnabled( master.currentModel()->currentFrame() == NULL ? FALSE : TRUE);
} */

// Update GUI after model change (or different model selected)
void GuiQt::modelChanged(bool updateAtoms, bool updateCell, bool updateForcefield)
{
	if (!doesExist_) return;
	// Update status bar
	QString s;
	Model *m = master.currentModel();
	// Trajectory information label
	if (m->totalFrames() != 0)
	{
		s = "(Frame ";
		s += (m->renderSource() == m ? "Main" : itoa(m->framePosition()));
		s += " of ";
		s += itoa(m->totalFrames());
		s += ") ";
	}
	m = m->renderSource();
	// Model information
	s += itoa(m->nAtoms());
	s += " Atoms ";
	if (m->nSelected() != 0)
	{
		s += "(<b>";
		s += itoa(m->nSelected());
		s += " selected</b>) ";
	}
	s += ftoa(m->mass());
	s += " g mol<sup>-1</sup> ";
	Cell::CellType ct = m->cell()->type();
	if (ct != Cell::NoCell)
	{
		s += "(";
		s += Cell::cellType(ct);
		s += ", ";
		s += ftoa(m->density());
		switch (prefs.densityUnit())
		{
			case (Prefs::GramsPerCm):
				s += " g cm<sup>-3</sup>)";
				break;
			case (Prefs::AtomsPerAngstrom):
				s += " atoms &#8491;<sup>-3</sup>)";
				break;
		}
	}
	mainWindow->statusLabel->setText(s);
	// Update save button status
	mainWindow->ui.actionFileSave->setEnabled( m->isModified() );
	// Update contents of the atom list
	if (updateAtoms) atomlistDialog->refresh();
	// Update the contents of the cell page
	if (updateCell)
	{
		cellDefineDialog->refresh();
		cellTransformDialog->refresh();
	}
	// Update forcefields in the forcefield window
	if (updateForcefield) forcefieldsDialog->refresh();
	// Enable the Atom menu if one or more atoms are selected
	mainWindow->ui.AtomMenu->setEnabled( master.currentModel()->nSelected() == 0 ? FALSE : TRUE);
	// Update Undo Redo lists
	mainWindow->updateUndoRedo();
	// Request redraw of the main canvas
	mainView.postRedisplay();
}

// Update trajectory controls
void GuiQt::updateTrajControls()
{
	// First see if the model has a trajectory associated to it
	Model *m = master.currentModel();
	if (m->totalFrames() == 0)
	{
		mainWindow->ui.actionFrameFirst->setDisabled(TRUE);
		mainWindow->ui.actionFramePrevious->setDisabled(TRUE);
		mainWindow->ui.actionFrameNext->setDisabled(TRUE);
		mainWindow->ui.actionFrameLast->setDisabled(TRUE);
		mainWindow->ui.actionPlayPause->setDisabled(TRUE);
		mainWindow->ui.actionViewTrajectory->setDisabled(TRUE);
	}
	else
	{
		// Make sure the trajectory toolbar is visible
		mainWindow->ui.TrajectoryToolbar->setVisible(TRUE);
		// If the trajectory is playing, desensitise all but the play/pause button
		if (trajectoryPlaying_)
		{
			mainWindow->ui.actionFrameFirst->setDisabled(TRUE);
			mainWindow->ui.actionFramePrevious->setDisabled(TRUE);
			mainWindow->ui.actionFrameNext->setDisabled(TRUE);
			mainWindow->ui.actionFrameLast->setDisabled(TRUE);
			mainWindow->ui.actionPlayPause->setDisabled(FALSE);
		}
		else
		{
			mainWindow->ui.actionFrameFirst->setDisabled(FALSE);
			mainWindow->ui.actionFramePrevious->setDisabled(FALSE);
			mainWindow->ui.actionFrameNext->setDisabled(FALSE);
			mainWindow->ui.actionFrameLast->setDisabled(FALSE);
			mainWindow->ui.actionPlayPause->setDisabled(FALSE);
		}
		mainWindow->ui.actionViewTrajectory->setDisabled(FALSE);
		// Select the correct view action
		if (m->renderSource() == m) mainWindow->ui.actionViewModel->setChecked(TRUE);
		else mainWindow->ui.actionViewTrajectory->setChecked(TRUE);
	}
}

// Update model lists
void GuiQt::updateModelLists()
{
}

void GuiQt::printMessage(const char *s)
{
	static char str[8096];
	static int n;
	if (!doesExist_) return;
	// Remove the '\n' from the end of s (if it has one)
	for (n=0; s[n] != '\0'; n++) str[n] = (s[n] == '\n' ? ' ' : s[n]);
	str[n] = '\0';
	mainWindow->ui.TextDisplay->append(str);
}

bool GuiQt::saveBeforeClose()
{
	// Check the status of all models, asking to save before close if necessary
	char text[512];
	int returnvalue;
	Filter *f;
	for (Model *m = master.models(); m != NULL; m = m->next)
	{
		if (m->isModified())
		{
			// Create a model message dialog
			sprintf(text, "Model '%s' has been modified.\n", m->name());
			returnvalue = QMessageBox::warning(mainWindow, "Aten", text, QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
			switch (returnvalue)
			{
				// Discard changes
				case (QMessageBox::Discard):
					break;
				// Cancel quit and return to app
				case (QMessageBox::Cancel):
					return FALSE;
				// Save model before quit
				case (QMessageBox::Save):
					// If model has a filter set, just save it
					f = m->filter();
					if (f != NULL) f->execute(m->filename());
					else if (mainWindow->runSaveModelDialog())
					{
						m->setFilter(mainWindow->saveModelFilter);
						m->setFilename(mainWindow->saveModelFilename.get());
						mainWindow->saveModelFilter->execute(m->filename());
					}
					else return FALSE;
					break;
			}
		}
	}
	return TRUE;
}

/*
// Keycode Conversion
*/

// Convert Qt keysym to key_code
Canvas::KeyCode GuiQt::convertToKeyCode(int sym)
{
	Canvas::KeyCode result = Canvas::OtherKey;
	switch (sym)
	{
		case (Qt::Key_Left):
			result = Canvas::LeftKey;
			break;
		case (Qt::Key_Right):
			result = Canvas::RightKey;
			break;
		case (Qt::Key_Up):
			result = Canvas::UpKey;
			break;
		case (Qt::Key_Down):
			result = Canvas::DownKey;
			break;
		case (Qt::Key_Shift):
			result = Canvas::LeftShiftKey;
			break;
		case (Qt::Key_Control):
			result = Canvas::LeftControlKey;
			break;
		case (Qt::Key_Alt):
			result = Canvas::LeftAltKey;
			break;
	}
	return result;
}

/*
// Progress Dialogs
*/

// Instantiate a progress dialog
void GuiQt::progressCreate(const char *jobtitle, int stepstodo)
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		gui.textProgressCreate(jobtitle, stepstodo);
		return;
	}
	// Check that a progress dialog isn't already running
	if (mainWindow->progressIndicator->isVisible())
	{
		printf("Weird programmatical event - second progress dialog creation request!\n");
		return;
	}
	mainWindow->progressBar->setMaximum(stepstodo);
	mainWindow->progressBar->setValue(0);
	mainWindow->progressLabel->setText(jobtitle);
	mainWindow->progressIndicator->setVisible(TRUE);
	progressCanceled_ = FALSE;
	// Disable some key widgets on the main form
	mainWindow->ui.ViewFrame->setEnabled(FALSE);
	mainWindow->ui.WindowToolbar->setEnabled(FALSE);
}

// Update the progress dialog
bool GuiQt::progressUpdate(int currentstep)
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		gui.textProgressUpdate(currentstep);
		return TRUE;
	}
	if (!mainWindow->progressIndicator->isVisible())
	{
		printf("Weird programmatical event - tried to update a non-existent progress dialog!\n");
		return TRUE;
	}
	mainWindow->progressBar->setValue(currentstep);
	app->processEvents();
	// Check to see if the abort button was pressed
	return (!progressCanceled_);
}

// Terminate the progress dialog
void GuiQt::progressTerminate()
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		gui.textProgressTerminate();
		return;
	}
	if (!mainWindow->progressIndicator->isVisible())
	{
		printf("Weird programmatical event - tried to terminate a non-existent progress dialog!\n");
		return;
	}
	// Hide the progress bar and re-enable widgets
	mainWindow->progressIndicator->setVisible(FALSE);
	mainWindow->ui.ViewFrame->setEnabled(TRUE);
	mainWindow->ui.WindowToolbar->setEnabled(TRUE);
}

// Notify that the progress indicator should be canceled
void GuiQt::notifyProgressCanceled()
{
	progressCanceled_ = TRUE;
}

// Instantiate text-based progress dialog
void GuiQt::textProgressCreate(const char *jobtitle, int stepstodo)
{
	// Reset the counters
	textProgressStepsToDo_ = stepstodo;
	textProgressPercent_ = 0;
	// Print out the empty progress indicator
	printf("--- %s\n", jobtitle);
	printf("Progress [-]                              (  0%%)");
}

// Update the text progress dialog
void GuiQt::textProgressUpdate(int currentstep)
{
	static char *twister = "-\\|/";
	static char *c = twister;
	static int n, ndots;
	static double dpercent;
	static int percent;
	// Work out percentage and print dots and spaces
	dpercent = double(currentstep) / double(textProgressStepsToDo_);
	percent = int(dpercent * 100.0);
	ndots = int(dpercent * 30.0);
	dpercent *= 100.0;
	if (percent != textProgressPercent_)
	{
		// Print the header
		printf("\rProgress [%c]",*c);
		// Increase the twoster character
		c ++;
		if (*c == '\0') c = twister;
		for (n=0; n<ndots; n++) printf(".");
		for (n=ndots; n<30; n++) printf(" ");
		// Lastly, print percentage
		printf("(%-3i%%)",percent);
		textProgressPercent_ = percent;
	}
}

// Terminate the text progress dialog
void GuiQt::textProgressTerminate()
{
	printf("\n");
}

/*
// Trajectory
*/

// Return state of trajectory playback
bool GuiQt::trajectoryPlaying()
{
	return trajectoryPlaying_;
}

// Set state of trajectory playback
void GuiQt::setTrajectoryPlaying(bool b)
{
	trajectoryPlaying_ = b;
}

// Return trajectory timer id
int GuiQt::trajectoryTimerId()
{
	return trajectoryTimerId_;
}

// Set state of trajectory playback
void GuiQt::setTrajectoryTimerId(int i)
{
	trajectoryTimerId_ = i;
}

// Stop trajectory playback
void GuiQt::stopTrajectoryPlayback()
{
	mainWidget->killTimer(trajectoryTimerId_);
	mainWindow->ui.actionPlayPause->setChecked(FALSE);
	trajectoryPlaying_ = FALSE;
	gui.updateTrajControls();
	modelChanged();
}
