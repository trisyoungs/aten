/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007-2011

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
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/selectfilter.h"
#include "gui/selectpattern.h"
#include "gui/selectvariable.h"
#include "gui/selectelement.h"
#include "gui/about.h"
#include "gui/ffeditor.h"
#include "gui/tcanvas.uih"
#include "gui/grids.h"
#include "gui/disorder.h"
#include "gui/atomlist.h"
#include "gui/fragments.h"
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "gui/command.h"
#include "gui/build.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/md.h"
#include "gui/forcefields.h"
#include "gui/modellist.h"
#include "gui/toolbox.h"
#include "gui/transform.h"
#include "gui/select.h"
#include "gui/position.h"
#include "gui/vibrations.h"
#include "gui/viewbasis.h"
#include "gui/vieweigenvector.h"
#include "gui/zmatrix.h"
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
	app = NULL;
	mainWindow = NULL;
	mainWidget = NULL;
	prefsDialog = NULL;
	forcefieldEditorDialog = NULL;
	loadModelDialog = NULL;
	selectFilterDialog = NULL;
	selectPatternDialog = NULL;
	selectVariableDialog = NULL;
	selectElementDialog = NULL;
	viewBasisDialog = NULL;
	viewEigenvectorDialog = NULL;
	aboutDialog = NULL;
	disorderWindow = NULL;
	zmatrixWindow = NULL;
	
	atomListWidget = NULL;
	buildWidget = NULL;
	cellDefinitionWidget = NULL;
	cellTransformWidget = NULL;
	commandWidget = NULL;
	forcefieldsWidget = NULL;
	fragmentsWidget = NULL;
	geometryWidget = NULL;
	glyphsWidget = NULL;
	gridsWidget = NULL;
	mdWidget = NULL;
	positionWidget = NULL;
	toolBoxWidget = NULL;
	transformWidget = NULL;
	vibrationsWidget = NULL;
}

// Destructor
GuiQt::~GuiQt()
{
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
	#if QT_VERSION >= 0x040600
	QGL::setPreferredPaintEngine(QPaintEngine::OpenGL);
	#endif
	app = new QApplication(argc, argv);

	// Initialise application name, organisation and author
	QCoreApplication::setOrganizationName("ProjectAten");
	QCoreApplication::setOrganizationDomain("www.projectaten.net");
	QCoreApplication::setApplicationName("Aten");

	// Create GUI window here (used to be done in GuiQt::run(), but this would cause GLX crashes under some circumstances, apparently as a result of the lack of ownership of the TCanvas)
	mainWindow = new AtenForm;

	// Create the main QGLWidget
	QGLFormat format;
	format.setSampleBuffers(TRUE);
	QGLContext *ctxt = new QGLContext(format);

	// Create the widget
	mainWidget = new TCanvas(ctxt, mainWindow);
	mainWidget->probeFeatures();
	mainWidget->setGeometry(0,0,800,600);
	mainWidget->setCursor(Qt::ArrowCursor);
	mainWidget->enableDrawing();
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

	// ...dialog windows...
	prefsDialog = new AtenPrefs(mainWindow);
	forcefieldEditorDialog = new AtenForcefieldEditor(mainWindow);
	loadModelDialog = new AtenLoadModel(mainWindow);
	selectFilterDialog = new AtenSelectFilter(mainWindow);
	selectPatternDialog = new AtenSelectPattern(mainWindow);
	selectVariableDialog = new AtenSelectVariable(mainWindow);
	selectElementDialog = new AtenSelectElement(mainWindow);
	aboutDialog = new AtenAbout(mainWindow);
	viewBasisDialog = new AtenViewBasis(mainWindow);
	viewEigenvectorDialog = new AtenViewEigenvector(mainWindow);
	disorderWindow = new AtenDisorder(mainWindow, Qt::Window|Qt::Tool);
	zmatrixWindow = new AtenZMatrix(mainWindow);
	
	// ...dock widgets
	atomListWidget = new AtomListWidget(mainWindow, Qt::Tool);
	buildWidget = new BuildWidget(mainWindow, Qt::Tool);
	cellDefinitionWidget = new CellDefinitionWidget(mainWindow, Qt::Tool);
	cellTransformWidget = new CellTransformWidget(mainWindow, Qt::Tool);
	commandWidget = new CommandWidget(mainWindow, Qt::Tool);
	forcefieldsWidget = new ForcefieldsWidget(mainWindow, Qt::Tool);
	fragmentsWidget = new FragmentsWidget(mainWindow, Qt::Tool);
	geometryWidget = new GeometryWidget(mainWindow, Qt::Tool);
	glyphsWidget = new GlyphsWidget(mainWindow, Qt::Tool);
	gridsWidget = new GridsWidget(mainWindow, Qt::Tool);
	mdWidget = new MDWidget(mainWindow, Qt::Tool);
	modelListWidget = new ModelListWidget(mainWindow, Qt::Tool);
	positionWidget = new PositionWidget(mainWindow, Qt::Tool);
	selectWidget = new SelectWidget(mainWindow, Qt::Tool);
	toolBoxWidget = new ToolBoxWidget(mainWindow, Qt::Tool);
	transformWidget = new TransformWidget(mainWindow, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(mainWindow, Qt::Tool);
	dockWidgets_ << atomListWidget << buildWidget << cellDefinitionWidget << cellTransformWidget << commandWidget << forcefieldsWidget << fragmentsWidget << geometryWidget << glyphsWidget << gridsWidget << mdWidget << positionWidget << selectWidget << toolBoxWidget << transformWidget << vibrationsWidget;
	toolBoxWidget->show();
	
	// Connect Finished signal of tool windows to finished slots in structure
	foreach( QObject *obj, dockWidgets_)
	{
		QObject::connect(obj, SIGNAL(visibilityChanged(bool)), toolBoxWidget, SLOT(dockWidgetVisibilityChanged(bool)));
		QObject::connect(obj, SIGNAL(topLevelChanged(bool)), toolBoxWidget, SLOT(dockWidgetTopLevelChanged(bool)));
	}
	QObject::connect(disorderWindow, SIGNAL(finished(int)), disorderWindow, SLOT(dialogFinished(int)));	// TGAY
	QObject::connect(zmatrixWindow, SIGNAL(finished(int)), zmatrixWindow, SLOT(dialogFinished(int)));// TGAY

	// Set the modality of some dialogs
	prefsDialog->setModal(TRUE);
	forcefieldEditorDialog->setModal(TRUE);
	loadModelDialog->setModal(TRUE);
	selectFilterDialog->setModal(TRUE);
	selectPatternDialog->setModal(TRUE);
	selectVariableDialog->setModal(TRUE);
	selectElementDialog->setModal(TRUE);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainWindow->finaliseUi();
	glyphsWidget->finaliseUi();
	prefsDialog->finaliseUi();
	forcefieldEditorDialog->finaliseUi();
	loadModelDialog->finaliseUi();
	selectFilterDialog->finaliseUi();
	selectPatternDialog->finaliseUi();
	selectVariableDialog->finaliseUi();
	selectElementDialog->finaliseUi();

	// Temporarily disable drawing on the main canvas again
	gui.mainWidget->disableDrawing();

	// Set controls in the windows
	mainWindow->setControls();
	prefsDialog->setControls();
	forcefieldEditorDialog->setControls();
	loadModelDialog->setControls();
	selectFilterDialog->setControls();
	selectPatternDialog->setControls();
	selectVariableDialog->setControls();
	fragmentsWidget->refresh();
	commandWidget->refresh();

	// Set central widget of main window, show the main window, and flag it as existing
	mainWindow->setCentralWidget(mainWindow->ui.ViewFrame);
	mainWindow->show();
	doesExist_ = TRUE;

	// Refresh the necessary windows
	gridsWidget->refresh();
	forcefieldsWidget->refresh();
	disorderWindow->refresh();
	mdWidget->refresh();
	cellDefinitionWidget->refresh();
	cellTransformWidget->refresh();
	mainWindow->update();
	commandWidget->refreshScripts();
	modelListWidget->refresh();

	// Reset view of all loaded models
	for (Model *m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView();

	gui.mainWidget->enableDrawing();
	gui.mainWidget->postRedisplay();

	// Display message box warning if there was a filter load error
	if (aten.nFiltersFailed() == -1)
	{
		QMessageBox::warning(NULL, "Aten", "Filters could not be found.\nNo import/export will be possible.\nSet the environment variable ATENDATA to point to Aten's data directory (e.g. 'export ATENDATA=/usr/local/aten/data'), or run with --atendata <dir>.\n", QMessageBox::Ok, QMessageBox::Ok);
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
		QMessageBox::warning(NULL, "Aten", text, QMessageBox::Ok, QMessageBox::Ok);
	}

	// Display message for 1056
	if (prefs.warning1056())
	{
		QMessageBox::information(NULL, "Aten", "Note that v1.5 (revision 1056) introduced a rewrite of the typing language, along with some subtle changes to the syntax.\nYour own forcefield files may need to be checked in order to ensure consistency.\n(Disable this warning with 'aten.prefs.warn1056 = FALSE;' in your prefs file)", QMessageBox::Ok, QMessageBox::Ok);
	}

	// Add GNU GPL message to statusbox
	msg.print("Aten version %s (%s@%s) built on %s, Copyright (C) 2007-2011  T. Youngs.\n", ATENVERSION, ATENURL, ATENREVISION, ATENDATE);
	msg.print("Aten uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	msg.print("Aten comes with ABSOLUTELY NO WARRANTY.\n");
	msg.print("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	msg.print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	// Enter main message processing loop
	app->exec();

	// delete mainWindow;

	msg.exit("GuiQt::run");
}

/*
// Refresh Wrapper
*/

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenForm's function)
void GuiQt::update(bool updateAtoms, bool updateCell, bool updateForcefield, bool updateGlyphs, bool updateGrids)
{
	if (!doesExist_) return;
	// Refresh aspects of main window
	mainWindow->update();
	// Update contents of the atom list
	if (updateAtoms) atomListWidget->refresh();
	// Update contents of the glyph list
	if (updateGlyphs) glyphsWidget->refresh();
	// Update contents of the grid window
	if (updateGrids) gridsWidget->refresh();
	// Update selection window
	selectWidget->refresh();
	// Update vibrations window
	vibrationsWidget->refresh();
	// Update the contents of the cell page
	if (updateCell)
	{
		cellDefinitionWidget->refresh();
		cellTransformWidget->refresh();
		disorderWindow->refresh();
	}
	// Update forcefields in the forcefield widget
	if (updateForcefield) forcefieldsWidget->refresh();
	// Update context menu items
	updateContextMenu();
	// Update geometry page
	geometryWidget->refresh();
	// Request redraw of the main canvas
	gui.mainWidget->postRedisplay();
}

// Update statusbar
void GuiQt::updateStatusBar(bool clear)
{
	static Dnchar text(512);
	static UserAction::Action lastAction = UserAction::NoAction;
	// Initialise string if NoAction
	if (lastAction == UserAction::NoAction) text.clear();
	// If current action is not the same as the last action, recreate string
	if (lastAction != mainWidget->selectedMode())
	{
		lastAction = mainWidget->selectedMode();
		text.clear();
		text.strcat("<b>");
		text.strcat(UserActions[lastAction].name);
		text.strcat(":</b> ");
		text.strcat(UserActions[lastAction].unModified);
		if (UserActions[lastAction].shiftModified[0] != '\0')
		{
			text.strcat(", <b>+shift</b> ");
			text.strcat(UserActions[lastAction].shiftModified);
		}
		if (UserActions[lastAction].ctrlModified[0] != '\0')
		{
			text.strcat(", <b>+ctrl</b> ");
			text.strcat(UserActions[lastAction].ctrlModified);
		}
		if (UserActions[lastAction].altModified[0] != '\0')
		{
			text.strcat(", <b>+alt</b> ");
			text.strcat(UserActions[lastAction].altModified);
		}
	}
	// Set text in statusbar widget
	if (clear) mainWindow->messageLabel->setText("");
	else mainWindow->messageLabel->setText(text.get());
}


/*
// Methods
*/

// // Add model to GUI list, in this case a tab in the ModelTabs widget
// void GuiQt::addModel(Model *m)
// {
// 	if (!doesExist_) return;
// 	mainWindow->addModelTab(m);
// 	m->resetView();		TGAY
// 	gui.update(TRUE,TRUE,TRUE);
// }

void GuiQt::printMessage(const char *s)
{
	static char str[8096];
	static int n;
	if (!doesExist_) return;
	// Remove the '\n' from the end of s (if it has one)
	for (n=0; s[n] != '\0'; n++) str[n] = (s[n] == '\n' ? ' ' : s[n]);
	str[n] = '\0';
// 	mainWindow->ui.TextDisplay->append(str);			// TGAY
// 	mainWindow->ui.TextDisplay->verticalScrollBar()->setValue(mainWindow->ui.TextDisplay->verticalScrollBar()->maximum());
}

// // Remove model from list
// void GuiQt::removeModel(int id)
// {
// 	if (!doesExist_) return;
// 	mainWindow->ui.ModelTabs->removeTab(id);			TGAY
// 	gui.update();
// }

bool GuiQt::saveBeforeClose()
{
	// Check the status of all models, asking to save before close if necessary
	Dnchar text;
	int returnvalue;
	ReturnValue rv;
	Tree *f;
	for (Model *m = aten.models(); m != NULL; m = m->next)
	{
		if (m->changeLog.isModified())
		{
			// Create a model message dialog
			text.sprintf("Model '%s' has been modified.\n", m->name());
			returnvalue = QMessageBox::warning(mainWindow, "Aten", text.get(), QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
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

	mainWidget->setOffScreenRendering(TRUE);
	mainWidget->postRedisplay();

	// Flag any surfaces to be rerendered for use in this context
	aten.current.rs->rerenderGrids();

	if (prefs.useFrameBuffer() == FALSE) pixmap = mainWidget->renderPixmap(width, height, FALSE);
	else
	{
		QImage image = mainWidget->grabFrameBuffer();
		pixmap = QPixmap::fromImage(image);
	}

	mainWidget->setOffScreenRendering(FALSE);
	if (!prefs.reusePrimitiveQuality()) mainWidget->reinitialisePrimitives();
	prefs.setScreenObjects(screenbits);

	// Flag any surfaces to be rerendered so they are redisplayed correctly in the GUI's original GLcontext
	aten.current.rs->rerenderGrids();

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
	disorderWindow->setEnabled(b);

	// ...and all the dock widgets...
	foreach( QObject *obj, dockWidgets_) obj->setProperty( "enabled", b);

	// ...and the main toolbar
	mainWindow->ui.MainToolbar->setEnabled(b);
	
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
	double dpercent = double(progressCurrentStep_) / double(progressStepsToDo_);
	static QTime remtime;
	static Dnchar etatext;
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
			if (shorttext == NULL)
			{
				for (n=0; n<ndots; n++) printf(".");
				for (n=ndots; n<30; n++) printf(" ");
			}
			// Lastly, print percentage and ETA
			etatext.sprintf("(%-3i%%, ETA %02i:%02i:%02i)",percent, remtime.hour(), remtime.minute(), remtime.second());
			if (shorttext == NULL) printf("%s", etatext.get());
			else shorttext->set(etatext);
			fflush(stdout);
			progressPercent_ = percent;
		}
	}
	else if (time_.elapsed() >= 500)
	{
		setWindowsEnabled(FALSE);
		mainWindow->progressIndicator->setVisible(TRUE);
		mainWindow->progressBar->setValue(progressCurrentStep_);
		etatext.sprintf("ETA %02i:%02i:%02i", remtime.hour(), remtime.minute(), remtime.second());
		mainWindow->progressEta->setText(etatext.get());
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
	gui.mainWidget->setEditable(TRUE);
	mainWindow->updateTrajectoryControls();
	update();
}
