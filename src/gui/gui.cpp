/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007-2010

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
#include "main/version.h"
#include "render/canvas.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/selectpattern.h"
#include "gui/selectelement.h"
#include "gui/commandhelp.h"
#include "gui/about.h"
#include "gui/ffeditor.h"
#include "gui/tcanvas.uih"
#include "gui/grids.h"
#include "gui/disorder.h"
#include "gui/atomlist.h"
#include "gui/forcefields.h"
#include "gui/celldefine.h"
#include "gui/celltransform.h"
#include "gui/command.h"
#include "gui/build.h"
#include "gui/geometry.h"
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
GuiQt::BitmapFormat GuiQt::bitmapFormat(const char *s, bool reporterror)
{
	GuiQt::BitmapFormat bf = (GuiQt::BitmapFormat) enumSearch("bitmap format",GuiQt::nBitmapFormats,bitmapFormatExtensions,s);
	if ((bf == GuiQt::nBitmapFormats) && reporterror) enumPrintValid(GuiQt::nBitmapFormats,bitmapFormatExtensions);
	return bf;
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
	doesExist_ = FALSE;
	isAvailable_ = FALSE;
	trajectoryPlaying_ = FALSE;
	trajectoryTimerId_ = -1;
	mainWindow = NULL;
	mainWidget = NULL;
	prefsDialog = NULL;
	forcefieldEditorDialog = NULL;
	loadModelDialog = NULL;
	selectFilterDialog = NULL;
	selectPatternDialog = NULL;
	selectElementDialog = NULL;
	aboutDialog = NULL;
	atomlistWindow = NULL;
	buildWindow = NULL;
	cellDefineWindow = NULL;
	cellTransformWindow = NULL;
	commandWindow = NULL;
	disorderWindow = NULL;
	forcefieldsWindow = NULL;
	geometryWindow = NULL;
	glyphsWindow = NULL;
	gridsWindow = NULL;
	minimiserWindow = NULL;
	positionWindow = NULL;
	transformWindow = NULL;
}

// Destructor
GuiQt::~GuiQt()
{
// 	if (mainWindow != NULL) delete mainWindow;
// 	mainWidget = NULL;
// 	prefsDialog = NULL;
// 	forcefieldEditorDialog = NULL;
// 	loadModelDialog = NULL;
// 	selectFilterDialog = NULL;
// 	selectPatternDialog = NULL;
// 	selectElementDialog = NULL;
// 	commandHelpDialog = NULL;
// 	aboutDialog = NULL;
// 	// ...tool windows
// 	atomlistWindow = NULL;
// 	buildWindow = NULL;
// 	cellDefineWindow = NULL;
// 	cellTransformWindow = NULL;
// 	commandWindow = NULL;
// 	disorderWindow = NULL;
// 	forcefieldsWindow = NULL;
// 	glyphsWindow = NULL;
// 	gridsWindow = NULL;
// 	minimiserWindow = NULL;
// 	positionWindow = NULL;
// 	transformWindow = NULL;
}

/*
// General Functions
*/

// Returns if the GUI has been created
bool GuiQt::exists()
{
	return doesExist_;
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
	selectFilterDialog = new AtenSelectFilter(mainWindow);
	selectPatternDialog = new AtenSelectPattern(mainWindow);
	selectElementDialog = new AtenSelectElement(mainWindow);
	commandHelpDialog = new AtenCommandHelp(mainWindow);
	aboutDialog = new AtenAbout(mainWindow);
	// ...tool windows
	atomlistWindow = new AtenAtomlist(mainWindow, Qt::Window|Qt::Tool);
	buildWindow = new AtenBuild(mainWindow, Qt::Window|Qt::Tool);
	cellDefineWindow = new AtenCellDefine(mainWindow, Qt::Window|Qt::Tool);
	cellTransformWindow = new AtenCellTransform(mainWindow, Qt::Window|Qt::Tool);
	commandWindow = new AtenCommand(mainWindow, Qt::Window|Qt::Tool);
	disorderWindow = new AtenDisorder(mainWindow, Qt::Window|Qt::Tool);
	forcefieldsWindow = new AtenForcefields(mainWindow, Qt::Window|Qt::Tool);
	geometryWindow = new AtenGeometry(mainWindow, Qt::Window|Qt::Tool);
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
	QObject::connect(commandWindow, SIGNAL(finished(int)), commandWindow, SLOT(dialogFinished(int)));
	QObject::connect(disorderWindow, SIGNAL(finished(int)), disorderWindow, SLOT(dialogFinished(int)));
	QObject::connect(forcefieldsWindow, SIGNAL(finished(int)), forcefieldsWindow, SLOT(dialogFinished(int)));
	QObject::connect(geometryWindow, SIGNAL(finished(int)), geometryWindow, SLOT(dialogFinished(int)));
	QObject::connect(glyphsWindow, SIGNAL(finished(int)), glyphsWindow, SLOT(dialogFinished(int)));
	QObject::connect(gridsWindow, SIGNAL(finished(int)), gridsWindow, SLOT(dialogFinished(int)));
	QObject::connect(minimiserWindow, SIGNAL(finished(int)), minimiserWindow, SLOT(dialogFinished(int)));
	QObject::connect(positionWindow, SIGNAL(finished(int)), positionWindow, SLOT(dialogFinished(int)));
	QObject::connect(transformWindow, SIGNAL(finished(int)), transformWindow, SLOT(dialogFinished(int)));

	// Set the modality of some dialogs
	prefsDialog->setModal(TRUE);
	forcefieldEditorDialog->setModal(TRUE);
	loadModelDialog->setModal(TRUE);
	selectFilterDialog->setModal(TRUE);
	selectPatternDialog->setModal(TRUE);
	selectElementDialog->setModal(TRUE);
	commandHelpDialog->setModal(TRUE);
// 	atomlistWindow->setModal(TRUE);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainWindow->finaliseUi();
	prefsDialog->finaliseUi();
	forcefieldEditorDialog->finaliseUi();
	loadModelDialog->finaliseUi();
	selectFilterDialog->finaliseUi();
	selectPatternDialog->finaliseUi();
	selectElementDialog->finaliseUi();
	commandHelpDialog->finaliseUi();

	// Temporarily disable drawing on the main canvas again
	gui.mainView.disableDrawing();

	// Set controls in the windows
	mainWindow->setControls();
	prefsDialog->setControls();
	forcefieldEditorDialog->setControls();
	loadModelDialog->setControls();
	selectFilterDialog->setControls();
	selectPatternDialog->setControls();

	// Show the widgets in the GUI and flag it as existing
	mainWindow->show();
	doesExist_ = TRUE;

	// Refresh the necessary windows
	gridsWindow->refresh();
	forcefieldsWindow->refresh();
	disorderWindow->refresh();
	cellDefineWindow->refresh();
	cellTransformWindow->refresh();
	mainWindow->update();

	gui.mainView.enableDrawing();

	// Add loaded models to tabbar (and reset the view while we're here)
	// Must remember the current model, since adding the tabs will change it (in the currentChanged callback)
	Model *currentm = aten.currentModel();
	int tabid, currenttab;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		tabid = mainWindow->addModelTab(m);
		if (m == currentm) currenttab = tabid;
		if (!prefs.keepView()) m->resetView();
		m->calculateViewMatrix();
		//m->projectAll();
	}
	mainWindow->ui.ModelTabs->setCurrentIndex(currenttab);

	gui.mainView.postRedisplay();

	// Display message box warning if there was a filter load error
	if (aten.nFiltersFailed() == -1)
	{
		int ret = QMessageBox::warning(NULL, "Aten", "Filters could not be found.\nNo import/export will be possible.\nSet the environment variable ATENDATA to point to Aten's data directory (e.g. 'export ATENDATA=/usr/local/aten/data'), or run with --atendata <dir>.\n", QMessageBox::Ok, QMessageBox::Ok);
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

	// Display message for 1056
	if (prefs.warning1056())
	{
		int ret = QMessageBox::information(NULL, "Aten", "Note that v1.5 (revision 1056) introduced a rewrite of the typing language, along with some subtle changes to the syntax.\nYour own forcefield files may need to be checked in order to ensure consistency.\n(Disable this warning with 'aten.prefs.warn1056 = FALSE;' in your prefs file)", QMessageBox::Ok, QMessageBox::Ok);
	}

	// Add GNU GPL message to statusbox
	msg.print("Aten version %s (%s@%s) built on %s, Copyright (C) 2007-2010  T. Youngs.\n", ATENVERSION, ATENURL, ATENREVISION, ATENDATE);
	msg.print("Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	msg.print("Aten comes with ABSOLUTELY NO WARRANTY.\n");
	msg.print("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	msg.print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	// Enter main message processing loop
	int n = app->exec();

	msg.exit("GuiQt::run");
}

/*
// Refresh Wrapper
*/

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenForm's functinn)
void GuiQt::update(bool updateAtoms, bool updateCell, bool updateForcefield)
{
	if (!doesExist_) return;
	// Refresh aspects of main window
	mainWindow->update();
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
	// Update context menu items
	updateContextMenu();
	// Update geometry page
	geometryWindow->refresh();
	// Request redraw of the main canvas
	gui.mainView.postRedisplay();
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


/*
// Methods
*/

// Add model to GUI list, in this case a tab in the ModelTabs widget
void GuiQt::addModel(Model *m)
{
	if (!doesExist_) return;
	mainWindow->addModelTab(m);
	m->resetView();
	gui.update(TRUE,TRUE,TRUE);
}

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

// Remove model from list
void GuiQt::removeModel(int id)
{
	if (!doesExist_) return;
	mainWindow->ui.ModelTabs->removeTab(id);
	gui.update();
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

	if (prefs.useFrameBuffer() == FALSE) pixmap = mainWidget->renderPixmap(width, height, FALSE);
	else
	{
		QImage image = mainWidget->grabFrameBuffer();
		pixmap = QPixmap::fromImage(image);
	}

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

// Enable / disable GUI (except progress bar group)
void GuiQt::setWindowsEnabled(bool b)
{
	// Disable some key widgets on the main form
	mainWindow->ui.ViewFrame->setEnabled(b);
	atomlistWindow->setEnabled(b);
	buildWindow->setEnabled(b);
	cellDefineWindow->setEnabled(b);
	cellTransformWindow->setEnabled(b);
	commandWindow->setEnabled(b);
	disorderWindow->setEnabled(b);
	forcefieldsWindow->setEnabled(b);
	geometryWindow->setEnabled(b);
	glyphsWindow->setEnabled(b);
	gridsWindow->setEnabled(b);
	minimiserWindow->setEnabled(b);
	positionWindow->setEnabled(b);
	transformWindow->setEnabled(b);
	mainWindow->ui.BondToolbar->setEnabled(b);
	mainWindow->ui.DrawToolbar->setEnabled(b);
	mainWindow->ui.EditToolbar->setEnabled(b);
	mainWindow->ui.FileToolbar->setEnabled(b);
	mainWindow->ui.ForcefieldsToolbar->setEnabled(b);
	mainWindow->ui.MeasureToolbar->setEnabled(b);
	mainWindow->ui.MouseToolbar->setEnabled(b);
	mainWindow->ui.SelectToolbar->setEnabled(b);
	mainWindow->ui.TrajectoryToolbar->setEnabled(b);
	mainWindow->ui.WindowToolbar->setEnabled(b);
	mainWindow->setWidgetsEnabled(b);
	app->processEvents();
}

/*
// Progress Dialogs
*/

// Notify that the progress indicator should be canceled
void GuiQt::notifyProgressCanceled()
{
	progressCanceled_ = TRUE;
}

// Instantiate a progress dialog
void GuiQt::progressCreate(const char *jobtitle, int stepstodo)
{
	// Reset our QTime object...
	time_.setHMS(0,0,0);
	time_.start();
	progressCurrentStep_ = 0;
	progressStepsToDo_ = stepstodo;
	progressPercent_ = 0;
	progressCanceled_ = FALSE;
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		// Don't print anything if we're in quiet mode
		if (msg.isQuiet() || (time_.elapsed() < 2000) ) return;
		// Print out the empty progress indicator
		printf("--- %s\n", jobtitle);
		printf("Progress [-]                              (  0%%)");
		fflush(stdout);
	}
	else
	{
		mainWindow->progressBar->setMaximum(stepstodo);
		mainWindow->progressBar->setValue(0);
		mainWindow->progressTitle->setText(jobtitle);
		mainWindow->progressEta->setText("");
	}
}

// Terminate the progress dialog
void GuiQt::progressTerminate()
{
	// If the GUI doesn't exist, call the text-based progress indicator
	if (!doesExist_)
	{
		if (progressPercent_ == -1) return;
		if (time_.elapsed() >= 500) printf("\n");
	}
	else
	{
		// Hide the progress bar and re-enable widgets
		mainWindow->progressIndicator->setVisible(FALSE);
		setWindowsEnabled(TRUE);
		app->processEvents();
	}
	progressPercent_ = -1;
}

// Update the progress dialog
bool GuiQt::progressUpdate(int currentstep, Dnchar *shorttext)
{
	progressCurrentStep_ = (currentstep == -1 ? progressCurrentStep_+1 : currentstep);
	static double dpercent;
	static QTime remtime;
	static char etatext[64];
	dpercent = double(progressCurrentStep_) / double(progressStepsToDo_);
	// Show the progress bar if enough time has elapsed since the start of the operation...
	// If the GUI doesn't exist, call the text-based progress indicator instead
	// Calculate ETA
	remtime.setHMS(0,0,0);
	remtime = remtime.addMSecs( time_.elapsed() * ((1.0 - dpercent) / dpercent) );
	if (!doesExist_)
	{
		static char twister[4] = { '-', '\\', '|', '/' };
		static int n, ndots, c, percent;
		// Don't print anything if we're in quiet mode
		if (msg.isQuiet() || (time_.elapsed() < 500) ) return TRUE;
		// Work out percentage and print dots and spaces
		percent = int(dpercent * 100.0);
		ndots = int(dpercent * 30.0);
		dpercent *= 100.0;
		// Always print the header and twister character
		if (shorttext == NULL) printf("\rProgress [%c]", twister[c]);
		// Increase the twister character
		++c;
		c = c%4;
		// New dots or percentage to output?
		if (percent != progressPercent_)
		{
			if (etatext == NULL)
			{
				for (n=0; n<ndots; n++) printf(".");
				for (n=ndots; n<30; n++) printf(" ");
			}
			// Lastly, print percentage and ETA
			sprintf(etatext, "(%-3i%%, ETA %02i:%02i:%02i)",percent, remtime.hour(), remtime.minute(), remtime.second());
			if (shorttext == NULL) printf("%s", etatext);
			else shorttext->set(etatext);
			fflush(stdout);
			progressPercent_ = percent;
		}
	}
	else if (time_.elapsed() >= 500)
	{
		static char s[64];
		setWindowsEnabled(FALSE);
		mainWindow->progressIndicator->setVisible(TRUE);
		mainWindow->progressBar->setValue(progressCurrentStep_);
		sprintf(s, "ETA %02i:%02i:%02i", remtime.hour(), remtime.minute(), remtime.second());
		mainWindow->progressEta->setText(s);
		app->processEvents();
	}
	// Check to see if the abort button was pressed
	return (!progressCanceled_);
}

/*
// Trajectory
*/

// Return state of trajectory playback
bool GuiQt::isTrajectoryPlaying()
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
	mainWindow->ui.actionTrajectoryPlayPause->setChecked(FALSE);
	trajectoryPlaying_ = FALSE;
	mainWindow->updateTrajectoryControls();
	update();
}
