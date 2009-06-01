/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007-2009

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

#include "main/aten.h"
#include "gui/canvas.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/selectpattern.h"
#include "gui/selectelement.h"
#include "gui/about.h"
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
#include "base/sysfunc.h"
#include <QtGui/QMessageBox>
#include <QtCore/QTextStream>
#include <QtGui/QProgressBar>

// External Declarations
GuiQt gui;

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char *bitmapFormatFilters[GuiQt::nBitmapFormats] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *bitmapFormatExtensions[GuiQt::nBitmapFormats] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
GuiQt::BitmapFormat GuiQt::bitmapFormat(const char *s)
{
	return (GuiQt::BitmapFormat) enumSearch("bitmap format",GuiQt::nBitmapFormats,bitmapFormatExtensions,s);
}
GuiQt::BitmapFormat GuiQt::bitmapFormatFromFilter(const char *s)
{
	return (GuiQt::BitmapFormat) enumSearch("bitmap format",GuiQt::nBitmapFormats,bitmapFormatFilters,s);
}
const char *GuiQt::bitmapFormatFilter(GuiQt::BitmapFormat bf)
{
	return bitmapFormatFilters[bf];
}
const char *GuiQt::bitmapFormatExtension(GuiQt::BitmapFormat bf)
{
	return bitmapFormatExtensions[bf];
}

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
	mainWidget->probeFeatures();
	mainWidget->setGeometry(0,0,800,600);
	// Set the main gui widgetcanvas to be associated to the GUIs TCanvas (and vice versa)
	mainView.setWidget(mainWidget);
	mainWidget->setCanvas(&mainView);
	mainView.enableDrawing();
}

// Initialise and create GUI
void GuiQt::run()
{
	msg.enter("GuiQt::run");

	// If no model loaded, add one
	if (aten.nModels() == 0)
	{
		Model *m = aten.addModel();
		m->enableUndoRedo();
	}

	// Initialise Qt's icons resource
	Q_INIT_RESOURCE(icons);

	// Create GUI window, dialog windows, and sub windows
	mainWindow = new AtenForm;
	// ...dialog windows...
	prefsDialog = new AtenPrefs(mainWindow);
	forcefieldEditorDialog = new AtenForcefieldEditor(mainWindow);
	loadModelDialog = new AtenLoadModel(mainWindow);
	selectPatternDialog = new AtenSelectPattern(mainWindow);
	selectElementDialog = new AtenSelectElement(mainWindow);
	aboutDialog = new AtenAbout(mainWindow);
	// ...tool windows
	atomlistWindow = new AtenAtomlist(mainWindow, Qt::Window|Qt::Tool);
	buildWindow = new AtenBuild(mainWindow, Qt::Window|Qt::Tool);
	cellDefineWindow = new AtenCellDefine(mainWindow, Qt::Window|Qt::Tool);
	cellTransformWindow = new AtenCellTransform(mainWindow, Qt::Window|Qt::Tool);
	disorderWindow = new AtenDisorder(mainWindow, Qt::Window|Qt::Tool);
	forcefieldsWindow = new AtenForcefields(mainWindow, Qt::Window|Qt::Tool);
	glyphsWindow = new AtenGlyphs(mainWindow, Qt::Window|Qt::Tool);
	gridsWindow = new AtenGrids(mainWindow, Qt::Window|Qt::Tool);
	minimiserWindow = new AtenMinimiser(mainWindow, Qt::Window|Qt::Tool);
	positionWindow = new AtenPosition(mainWindow, Qt::Window|Qt::Tool);
	transformWindow = new AtenTransform(mainWindow, Qt::Window|Qt::Tool);

	// Connect Finished signal of tool windows to finished slots in structure
	QObject::connect(atomlistWindow, SIGNAL(finished(int)), atomlistWindow, SLOT(dialogFinished(int)));
	QObject::connect(buildWindow, SIGNAL(finished(int)), buildWindow, SLOT(dialogFinished(int)));
	QObject::connect(cellDefineWindow, SIGNAL(finished(int)), cellDefineWindow, SLOT(dialogFinished(int)));
	QObject::connect(cellTransformWindow, SIGNAL(finished(int)), cellTransformWindow, SLOT(dialogFinished(int)));
	QObject::connect(disorderWindow, SIGNAL(finished(int)), disorderWindow, SLOT(dialogFinished(int)));
	QObject::connect(forcefieldsWindow, SIGNAL(finished(int)), forcefieldsWindow, SLOT(dialogFinished(int)));
	QObject::connect(glyphsWindow, SIGNAL(finished(int)), glyphsWindow, SLOT(dialogFinished(int)));
	QObject::connect(gridsWindow, SIGNAL(finished(int)), gridsWindow, SLOT(dialogFinished(int)));
	QObject::connect(minimiserWindow, SIGNAL(finished(int)), minimiserWindow, SLOT(dialogFinished(int)));
	QObject::connect(positionWindow, SIGNAL(finished(int)), positionWindow, SLOT(dialogFinished(int)));
	QObject::connect(transformWindow, SIGNAL(finished(int)), transformWindow, SLOT(dialogFinished(int)));

	// Set the modality of some dialogs
	prefsDialog->setModal(TRUE);
	forcefieldEditorDialog->setModal(TRUE);
	loadModelDialog->setModal(TRUE);
	selectPatternDialog->setModal(TRUE);
	selectElementDialog->setModal(TRUE);
// 	atomlistWindow->setModal(TRUE);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainWindow->finaliseUi();
	prefsDialog->finaliseUi();
	forcefieldEditorDialog->finaliseUi();
	loadModelDialog->finaliseUi();
	selectPatternDialog->finaliseUi();
	selectElementDialog->finaliseUi();

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
	aten.setCurrentModel(aten.models());

	// Refresh the necessary windows
	gridsWindow->refresh();
	forcefieldsWindow->refresh();
	disorderWindow->refresh();
	cellDefineWindow->refresh();
	cellTransformWindow->refresh();
	updateTrajControls();

	gui.mainView.enableDrawing();

	// Add loaded models to tabbar (and reset the view while we're here)
	int tabid;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		tabid = mainWindow->ui.ModelTabs->addTab(m->name());
		if (!prefs.keepView()) m->resetView();
		m->calculateViewMatrix();
		//m->projectAll();
	}

	gui.mainView.postRedisplay();

	// Display message box warning if there was a filter load error
	if (aten.nFiltersFailed() == -1)
	{
		int ret = QMessageBox::warning(NULL, "Aten", "Filters could not be found.\nNo import/export will be possible.\nSet the environment variable ATENDATA to point to Aten's /data directory, or run with --atendata <dir>.\n", QMessageBox::Ok, QMessageBox::Ok);
	}
	else if (aten.nFiltersFailed() > 0)
	{
		// Construct the messagebox text
		QString text("One or more filters could not be loaded properly on startup.\nCheck shell output or run Settings->Reload Filters to diagnose the problem.\nFilters with errors were:\n");
		for (Dnchar *d = aten.failedFilters(); d != NULL; d = d->next)
		{
			text += "\t";
			text += d->get();
			if (d->next != NULL) text += "\n";
		}
		int ret = QMessageBox::warning(NULL, "Aten", text, QMessageBox::Ok, QMessageBox::Ok);
	}

	// Enter main message processing loop
	int n = app->exec();

	msg.exit("GuiQt::run");
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

// Update GUI after model change (or different model selected)
void GuiQt::modelChanged(bool updateAtoms, bool updateCell, bool updateForcefield)
{
	if (!doesExist_) return;
	// Update status bar
	QString s;
	Model *m = aten.currentModel();
	// First label - atom and trajectory frame information
	if (m->hasTrajectory())
	{
		s = "(Frame ";
		s += (m->renderSource() == m ? "Main" : itoa(m->frameIndex()+1));
		s += " of ";
		s += itoa(m->nFrames());
		s += ") ";
		// Toolbar
		mainWindow->updateTrajectoryToolbar();
	}
	m = m->renderSource();
	s += itoa(m->nAtoms());
	s += " Atoms ";
	// Add on unknown atom information
	if (m->nUnknownAtoms() != 0)
	{
		s += " (<b>";
		s += itoa(m->nUnknownAtoms());
		s += " unknown</b>) ";
	}
	if (m->nSelected() != 0)
	{
		s += "(<b>";
		s += itoa(m->nSelected());
		s += " selected</b>) ";
	}
	s += ftoa(m->mass());
	s += " g mol<sup>-1</sup> ";
	mainWindow->infoLabel1->setText(s);
	// Second label - cell information
	Cell::CellType ct = m->cell()->type();
	if (ct != Cell::NoCell)
	{
		s = Cell::cellType(ct);
		s += ", ";
		s += ftoa(m->density());
		switch (prefs.densityUnit())
		{
			case (Prefs::GramsPerCm):
				s += " g cm<sup>-3</sup>";
				break;
			case (Prefs::AtomsPerAngstrom):
				s += " atoms &#8491;<sup>-3</sup>";
				break;
		}
	}
	else s = "Non-periodic";
	mainWindow->infoLabel2->setText(s);
	// Update save button status
	mainWindow->ui.actionFileSave->setEnabled( m->changeLog.isModified() );
	// Update contents of the atom list
	if (updateAtoms) atomlistWindow->refresh();
	// Update the contents of the cell page
	if (updateCell)
	{
		cellDefineWindow->refresh();
		cellTransformWindow->refresh();
		disorderWindow->refresh();
	}
	// Update forcefields in the forcefield window
	if (updateForcefield) forcefieldsWindow->refresh();
	// Enable the Atom menu if one or more atoms are selected
	mainWindow->ui.AtomContextMenu->setEnabled( m->renderSource()->nSelected() == 0 ? FALSE : TRUE);
	// Update Undo Redo lists
	mainWindow->updateUndoRedo();
	// Update main window title
	gui.updateWindowTitle();
	// Request redraw of the main canvas
	mainView.postRedisplay();
}

/*
// Main Window Refresh Functions
*/

// Refresh window title
void GuiQt::updateWindowTitle()
{
	if (!doesExist_) return;
	Model *m = aten.currentModel();
	static char title[512];
	sprintf(title, "Aten (%s) - %s (%s)%s", ATENVERSION, m->name(), m->filename()[0] == '\0' ? "<<no filename>>" : m->filename(), m->changeLog.isModified() ? " [Modified]" : "");
	mainWindow->setWindowTitle(title);
}

// Update trajectory controls
void GuiQt::updateTrajControls()
{
	if (!doesExist_) return;
	// First see if the model has a trajectory associated to it
	Model *m = aten.currentModel();
	if (m->nFrames() == 0) mainWindow->ui.TrajectoryToolbar->setDisabled(TRUE);
	else
	{
		// Make sure the trajectory toolbar is visible
		mainWindow->ui.TrajectoryToolbar->setDisabled(FALSE);
		mainWindow->ui.TrajectoryToolbar->setVisible(TRUE);
		// If the trajectory is playing, desensitise all but the play/pause button
		if (trajectoryPlaying_)
		{
			mainWindow->ui.actionFrameFirst->setDisabled(TRUE);
			mainWindow->ui.actionFramePrevious->setDisabled(TRUE);
			mainWindow->ui.actionFrameNext->setDisabled(TRUE);
			mainWindow->ui.actionFrameLast->setDisabled(TRUE);
			mainWindow->ui.actionPlayPause->setDisabled(FALSE);
			mainWindow->setTrajectoryToolbarActive(FALSE);
		}
		else
		{
			mainWindow->ui.actionFrameFirst->setDisabled(FALSE);
			mainWindow->ui.actionFramePrevious->setDisabled(FALSE);
			mainWindow->ui.actionFrameNext->setDisabled(FALSE);
			mainWindow->ui.actionFrameLast->setDisabled(FALSE);
			mainWindow->ui.actionPlayPause->setDisabled(FALSE);
			mainWindow->setTrajectoryToolbarActive(TRUE);
		}
		mainWindow->ui.actionViewTrajectory->setDisabled(FALSE);
		// Select the correct view action
		if (m->renderSource() == m) mainWindow->ui.actionViewModel->setChecked(TRUE);
		else mainWindow->ui.actionViewTrajectory->setChecked(TRUE);
		// Set slider and spinbox
		mainWindow->updateTrajectoryToolbar();
	}
}

// Update statusbar
void GuiQt::updateStatusBar(bool clear)
{
	static Dnchar text(512);
	static Canvas::UserAction lastAction = Canvas::NoAction;
	// Initialise string if NoAction
	if (lastAction == Canvas::NoAction) text.clear();
	// If current action is not the same as the last action, recreate string
	if (lastAction != mainView.selectedMode())
	{
		lastAction = mainView.selectedMode();
		text.clear();
		text.cat("<b>");
		text.cat(UserActionTexts[lastAction].name);
		text.cat(":</b> ");
		text.cat(UserActionTexts[lastAction].unModified);
		if (UserActionTexts[lastAction].shiftModified[0] != '\0')
		{
			text.cat(", <b>+shift</b> ");
			text.cat(UserActionTexts[lastAction].shiftModified);
		}
		if (UserActionTexts[lastAction].ctrlModified[0] != '\0')
		{
			text.cat(", <b>+ctrl</b> ");
			text.cat(UserActionTexts[lastAction].ctrlModified);
		}
		if (UserActionTexts[lastAction].altModified[0] != '\0')
		{
			text.cat(", <b>+alt</b> ");
			text.cat(UserActionTexts[lastAction].altModified);
		}
	}
	// Set text in statusbar widget
	if (clear) mainWindow->messageLabel->setText("");
	else mainWindow->messageLabel->setText(text.get());
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
	mainWindow->ui.TextDisplay->verticalScrollBar()->setValue(mainWindow->ui.TextDisplay->verticalScrollBar()->maximum());
}

bool GuiQt::saveBeforeClose()
{
	// Check the status of all models, asking to save before close if necessary
	char text[512];
	int returnvalue;
	ReturnValue rv;
	Tree *f;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		if (m->changeLog.isModified())
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
					if (f != NULL) f->executeWrite(m->filename(), rv);
					else if (mainWindow->runSaveModelDialog())
					{
						m->setFilter(mainWindow->saveModelFilter);
						m->setFilename(mainWindow->saveModelFilename.get());
						mainWindow->saveModelFilter->executeWrite(m->filename(), rv);
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
	// Reset our QTime object...
	time_.setHMS(0,0,0);
	time_.start();
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		gui.textProgressCreate(jobtitle, stepstodo);
		return;
	}
	// Check that a progress dialog isn't already running
// 	if (mainWindow->progressIndicator->isVisible())
// 	{
// 		printf("Weird programmatical event - second progress dialog creation request!\n");
// 		return;
// 	}
	mainWindow->progressBar->setMaximum(stepstodo);
	mainWindow->progressBar->setValue(0);
	mainWindow->progressLabel->setText(jobtitle);
// 	mainWindow->progressIndicator->setVisible(TRUE);
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
		textProgressCurrentStep_ = (currentstep == -1 ? textProgressCurrentStep_+1 : currentstep);
		gui.textProgressUpdate();
		return TRUE;
	}
// 	if (!mainWindow->progressIndicator->isVisible())
// 	{
// 		printf("Weird programmatical event - tried to update a non-existent progress dialog!\n");
// 		return TRUE;
// 	}
	// Show the progress bar if enough time has elapsed since the start of the operation...
	if (time_.elapsed() >= 2000) mainWindow->progressIndicator->setVisible(TRUE);
	if (currentstep != -1) mainWindow->progressBar->setValue(currentstep);
	else mainWindow->progressBar->setValue(mainWindow->progressBar->value() + 1);
	if (time_.elapsed() >= 2000) app->processEvents();
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
// 	if (!mainWindow->progressIndicator->isVisible())
// 	{
// 		printf("Weird programmatical event - tried to terminate a non-existent progress dialog!\n");
// 		return;
// 	}
	// Hide the progress bar and re-enable widgets
	mainWindow->progressIndicator->setVisible(FALSE);
	mainWindow->ui.ViewFrame->setEnabled(TRUE);
	mainWindow->ui.WindowToolbar->setEnabled(TRUE);
	app->processEvents();
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
	textProgressCurrentStep_ = 0;
	// Don't print anything if we're in quiet mode
	if (msg.isQuiet() || (time_.elapsed() < 2000) ) return;
	// Print out the empty progress indicator
	printf("--- %s\n", jobtitle);
	printf("Progress [-]                              (  0%%)");
	fflush(stdout);
}

// Update the text progress dialog
void GuiQt::textProgressUpdate()
{
// 	static char *twister = "-\\|/";
	static char twister[4] = { '-', '\\', '|', '/' };
// 	static char *c = twister;
	static int n, ndots, c;
	static double dpercent;
	static int percent;
	// Don't print anything if we're in quiet mode
	if (msg.isQuiet() || (time_.elapsed() < 2000) ) return;
	// Work out percentage and print dots and spaces
	dpercent = double(textProgressCurrentStep_) / double(textProgressStepsToDo_);
	percent = int(dpercent * 100.0);
	ndots = int(dpercent * 30.0);
	dpercent *= 100.0;
	// Always print the header and twister character
	printf("\rProgress [%c]", twister[c]);
	// Increase the twister character
	c ++;
	c = c%4;
	// New dots or percentage to output?
	if (percent != textProgressPercent_)
	{
		for (n=0; n<ndots; n++) printf(".");
		for (n=ndots; n<30; n++) printf(" ");
		// Lastly, print percentage
		printf("(%-3i%%)",percent);
		fflush(stdout);
		textProgressPercent_ = percent;
	}
}

// Terminate the text progress dialog
void GuiQt::textProgressTerminate()
{
	if (textProgressPercent_ == -1) return;
	printf("\n");
	textProgressPercent_ = -1;
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

/*
// Image Saving
*/

// Save image of current view
bool GuiQt::saveImage(const char *filename, BitmapFormat bf, int width, int height, int quality)
{
	if (bf == GuiQt::nBitmapFormats)
	{
		msg.print("Invalid bitmap format given to Gui::saveImage().\n");
		return FALSE;
	}

	// Create a QPixmap of the current scene setting and restoring the original view object bitvectors
	int screenbits = prefs.screenObjects();
	prefs.setScreenObjects(prefs.offScreenObjects());
	QPixmap pixmap;
	// Get current widget geometry if none was specified
	if (width == 0) width = mainWidget->width();
	if (height == 0) height = mainWidget->height();
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / mainWidget->height()) ));
	prefs.setLabelSize(newlabelsize);
	mainView.postRedisplay();
	mainView.setOffScreenRendering(TRUE);

	// Flag any surfaces to be rerendered for use in this context
	aten.current.rs->rerenderGrids();

	pixmap = mainWidget->renderPixmap(width, height, FALSE);

	mainView.setOffScreenRendering(FALSE);
	prefs.setScreenObjects(screenbits);

	// Flag any surfaces to be rerendered so they are redisplayed correctly in the GUI's original GLcontext
	aten.current.rs->rerenderGrids();

	// Reconfigure canvas to widget size (necessary if image size was changed)
	mainView.configure(mainWidget->width(), mainWidget->height());

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	pixmap.save(filename, GuiQt::bitmapFormatExtension(bf), quality);
	msg.print("Saved current view as '%s' [%ix%i %s]\n", filename, width, height, GuiQt::bitmapFormatFilter(bf));
	return TRUE;
}
