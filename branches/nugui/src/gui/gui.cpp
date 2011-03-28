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
#include "gui/disorderwizard.h"
#include "gui/atomlist.h"
#include "gui/build.h"
#include "gui/celldefinition.h"
#include "gui/celltransform.h"
#include "gui/command.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/geometry.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/md.h"
#include "gui/messages.h"
#include "gui/modellist.h"
#include "gui/position.h"
#include "gui/select.h"
#include "gui/toolbox.h"
#include "gui/trajectory.h"
#include "gui/transform.h"
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
	zmatrixWindow = NULL;
	
	atomListWidget = NULL;
	buildWidget = NULL;
	cellDefinitionWidget = NULL;
	cellTransformWidget = NULL;
	commandWidget = NULL;
	disorderWizard = NULL;
	forcefieldsWidget = NULL;
	fragmentsWidget = NULL;
	geometryWidget = NULL;
	glyphsWidget = NULL;
	gridsWidget = NULL;
	mdWidget = NULL;
	messagesWidget = NULL;
	modelListWidget = NULL;
	positionWidget = NULL;
	toolBoxWidget = NULL;
	trajectoryWidget = NULL;
	transformWidget = NULL;
	vibrationsWidget = NULL;
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
	zmatrixWindow = new AtenZMatrix(mainWindow);
	
	// ...dock widgets
	atomListWidget = new AtomListWidget(mainWindow, Qt::Tool);
	buildWidget = new BuildWidget(mainWindow, Qt::Tool);
	cellDefinitionWidget = new CellDefinitionWidget(mainWindow, Qt::Tool);
	cellTransformWidget = new CellTransformWidget(mainWindow, Qt::Tool);
	commandWidget = new CommandWidget(mainWindow, Qt::Tool);
	disorderWizard = new DisorderWizard(mainWindow);
	forcefieldsWidget = new ForcefieldsWidget(mainWindow, Qt::Tool);
	fragmentsWidget = new FragmentsWidget(mainWindow, Qt::Tool);
	geometryWidget = new GeometryWidget(mainWindow, Qt::Tool);
	glyphsWidget = new GlyphsWidget(mainWindow, Qt::Tool);
	gridsWidget = new GridsWidget(mainWindow, Qt::Tool);
	mdWidget = new MDWidget(mainWindow, Qt::Tool);
	messagesWidget = new MessagesWidget(mainWindow, Qt::Tool);
	modelListWidget = new ModelListWidget(mainWindow, Qt::Tool);
	positionWidget = new PositionWidget(mainWindow, Qt::Tool);
	selectWidget = new SelectWidget(mainWindow, Qt::Tool);
	toolBoxWidget = new ToolBoxWidget(mainWindow, Qt::Tool);
	trajectoryWidget = new TrajectoryWidget(mainWindow, Qt::Tool);
	transformWidget = new TransformWidget(mainWindow, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(mainWindow, Qt::Tool);
	dockWidgets_ << atomListWidget << buildWidget << cellDefinitionWidget << cellTransformWidget << commandWidget << fragmentsWidget << geometryWidget << glyphsWidget << gridsWidget << mdWidget << messagesWidget << modelListWidget << positionWidget << selectWidget << toolBoxWidget << trajectoryWidget << transformWidget << vibrationsWidget;
	toolBoxWidget->show();
	
	// Connect Finished signal of tool windows to finished slots in structure
	foreach( QDockWidget *obj, dockWidgets_)
	{
		QObject::connect(obj, SIGNAL(visibilityChanged(bool)), toolBoxWidget, SLOT(dockWidgetVisibilityChanged(bool)));
		QObject::connect(obj, SIGNAL(topLevelChanged(bool)), toolBoxWidget, SLOT(dockWidgetTopLevelChanged(bool)));
		// Add every dock widget to a dock area (annoying to have to do it, but prevents 'stuck' dock widgets on some versions)
		mainWindow->addDockWidget(Qt::RightDockWidgetArea, obj);
	}
	QObject::connect(zmatrixWindow, SIGNAL(finished(int)), zmatrixWindow, SLOT(dialogFinished(int)));// TGAY

	// Set the modality of some dialogs
	disorderWizard->setModal(FALSE);
	prefsDialog->setModal(TRUE);
	forcefieldEditorDialog->setModal(TRUE);
	loadModelDialog->setModal(TRUE);
	selectFilterDialog->setModal(TRUE);
	selectPatternDialog->setModal(TRUE);
	selectVariableDialog->setModal(TRUE);
	selectElementDialog->setModal(TRUE);

	// Set up misc things for Qt (QActionGroups etc.) that we couldn't do in Designer
	mainWindow->finaliseUi();
	loadModelDialog->finaliseUi();

	// Temporarily disable drawing on the main canvas again
	gui.mainWidget->disableDrawing();

	// Set controls in the windows
	mainWindow->setControls();
	prefsDialog->setControls();
	loadModelDialog->setControls();
	fragmentsWidget->refresh();
	commandWidget->refresh();

	// Show the main window, and flag it as existing
	mainWindow->show();
	doesExist_ = TRUE;

	// Refresh the necessary windows
	gridsWidget->refresh();
	forcefieldsWidget->refresh();
	mdWidget->refresh();
	cellDefinitionWidget->refresh();
	cellTransformWidget->refresh();
	mainWindow->update();
	commandWidget->refreshScripts();
	modelListWidget->refresh();
	atomListWidget->refresh();
	toolBoxWidget->updateButtons();

	// Reset view of all loaded models
	for (Model *m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView();

	gui.mainWidget->enableDrawing();
	gui.mainWidget->postRedisplay(TRUE);

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
	msg.print("<b>Aten</b> version %s (%s@%s) built on %s, Copyright (C) 2007-2011  T. Youngs.\n", ATENVERSION, ATENURL, ATENREVISION, ATENDATE);
	msg.print("<b>Aten</b> uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	msg.print("<b>Aten</b> comes with ABSOLUTELY NO WARRANTY.\n");
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
void GuiQt::update(int targets)
{
	if (!doesExist_) return;

	// Refresh aspects of main window and dock widgets
	mainWindow->update();
	updateContextMenu();
	
	if (targets&GuiQt::ModelsTarget) modelListWidget->refresh();
	if (targets&GuiQt::GeometryTarget) geometryWidget->refresh();
	if (targets&GuiQt::SelectTarget) selectWidget->refresh();
	if (targets&GuiQt::VibrationsTarget) vibrationsWidget->refresh();
	if (targets&GuiQt::TrajectoryTarget) trajectoryWidget->refresh();

	// Update contents of the atom list
	if (targets&GuiQt::AtomsTarget) atomListWidget->refresh();

	// Update contents of the glyph list
	if (targets&GuiQt::GlyphsTarget) glyphsWidget->refresh();

	// Update contents of the grid window
	if (targets&GuiQt::GridsTarget) gridsWidget->refresh();

	// Update the contents of the cell page
	if (targets&GuiQt::CellTarget)
	{
		cellDefinitionWidget->refresh();
		cellTransformWidget->refresh();
	}

	// Update forcefields in the forcefield widget
	if (targets&GuiQt::ForcefieldsTarget) forcefieldsWidget->refresh();

	if (targets&GuiQt::StatusBarTarget)
	{
		static Dnchar text(512);
		static UserAction::Action lastAction = UserAction::NoAction;
		
		// Initialise string if NoAction
		if (lastAction == UserAction::NoAction) text.clear();
		
		// If current action is not the same as the last action, recreate string
		if (lastAction != mainWidget->selectedMode())
		{
			lastAction = mainWidget->selectedMode();
			text.sprintf("<b>%s:</b> %s", UserActions[lastAction].name, UserActions[lastAction].unModified);
			if (UserActions[lastAction].shiftModified[0] != '\0') text.strcatf(", <b>+shift</b> %s", UserActions[lastAction].shiftModified);
			if (UserActions[lastAction].ctrlModified[0] != '\0') text.strcatf(", <b>+ctrl</b> %s", UserActions[lastAction].ctrlModified);
			if (UserActions[lastAction].altModified[0] != '\0') text.strcatf(", <b>+alt</b> %s", UserActions[lastAction].altModified);
		}
		// Set text in statusbar widget
// 		if (clear) mainWindow->setMessageLabel("");  TGAY
		mainWindow->setMessageLabel(text);
	}
	
	// Request redraw of the main canvas
	if (targets&GuiQt::CanvasTarget) gui.mainWidget->postRedisplay();
}

// Return the PID of Aten
int GuiQt::pid()
{
	return (app == NULL ? 0 : app->applicationPid());
}

// Process application messages
void GuiQt::processMessages()
{
	if (app != NULL) app->processEvents();
}

/*
// Methods
*/

// Print message
void GuiQt::printMessage(const char *s)
{
	static char str[8096];
	static int n;
	if (!doesExist_) return;
	// Remove the '\n' from the end of s (if it has one)
	for (n=0; s[n] != '\0'; n++) str[n] = (s[n] == '\n' ? ' ' : s[n]);
	str[n] = '\0';
	messagesWidget->ui.MessagesBrowser->append(str);
// 	mainWindow->ui.TextDisplay->verticalScrollBar()->setValue(mainWindow->ui.TextDisplay->verticalScrollBar()->maximum());  TGAY
}

// Check the status of all models, asking to save before close if necessary
bool GuiQt::saveBeforeClose()
{
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

	QPixmap pixmap;
	// Get current widget geometry if none was specified
	if (width == 0) width = mainWidget->width();
	if (height == 0) height = mainWidget->height();
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / mainWidget->height()) ));
	prefs.setLabelSize(newlabelsize);

	mainWidget->setOffScreenRendering(TRUE);
	mainWidget->postRedisplay(TRUE);

	// Flag any surfaces to be rerendered for use in this context
// 	aten.current.rs()->rerenderGrids();

	if (prefs.useFrameBuffer() == FALSE) pixmap = mainWidget->renderPixmap(width, height, FALSE);
	else
	{
		QImage image = mainWidget->grabFrameBuffer();
		pixmap = QPixmap::fromImage(image);
	}

	mainWidget->setOffScreenRendering(FALSE);
	if (!prefs.reusePrimitiveQuality()) mainWidget->reinitialisePrimitives();

	// Flag any surfaces to be rerendered so they are redisplayed correctly in the GUI's original GLcontext
// 	aten.current.rs()->rerenderGrids();

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	pixmap.save(filename, GuiQt::bitmapFormatExtension(bf), quality);
	msg.print("Saved current view as '%s' [%ix%i %s]\n", filename, width, height, GuiQt::bitmapFormatFilter(bf));
	return TRUE;
}

// Enable / disable GUI (except progress bar group)
void GuiQt::setWindowsEnabled(bool b)
{
	// Disable/enable some key widgets on the main form
	mainWindow->ui.ViewFrame->setEnabled(b);

	// ...and all the dock widgets...
	foreach( QObject *obj, dockWidgets_) obj->setProperty( "enabled", b);

	// ...and the main toolbar
	mainWindow->ui.MainToolbar->setEnabled(b);
	
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
	else mainWindow->updateProgressIndicator(TRUE, stepstodo, 0, jobtitle, "");
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
		mainWindow->updateProgressIndicator(FALSE);
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
		etatext.sprintf("ETA %02i:%02i:%02i", remtime.hour(), remtime.minute(), remtime.second());
		mainWindow->updateProgressIndicator(TRUE, -1, progressCurrentStep_, NULL, etatext);
		app->processEvents();
	}
	app->processEvents();
	// Check to see if the abort button was pressed
	return (!progressCanceled_);
}
