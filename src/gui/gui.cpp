/*
	*** Qt user interface functions
	*** src/gui/gui.cpp
	Copyright T. Youngs 2007-2012

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
#include "gui/progress.h"
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
#include "gui/pores.h"
#include "gui/position.h"
#include "gui/scriptmovie.h"
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

// External Declarations
GuiQt gui;

// Bitmap Image Formats (conform to allowable pixmap formats in Qt)
const char *bitmapFormatFilters[GuiQt::nBitmapFormats] = { "Windows Bitmap (*.bmp)", "Joint Photographic Experts Group (*.jpg)", "Portable Network Graphics (*.png)", "Portable Pixmap (*.ppm)", "X11 Bitmap (*.xbm)", "X11 Pixmap (*.xpm)" };
const char *bitmapFormatExtensions[GuiQt::nBitmapFormats] = { "bmp", "jpg", "png", "ppm", "xbm", "xpm" };
GuiQt::BitmapFormat GuiQt::bitmapFormat(const char *s, bool reportError)
{
	GuiQt::BitmapFormat bf = (GuiQt::BitmapFormat) enumSearch("bitmap format",GuiQt::nBitmapFormats,bitmapFormatExtensions,s);
	if ((bf == GuiQt::nBitmapFormats) && reportError) enumPrintValid(GuiQt::nBitmapFormats,bitmapFormatExtensions);
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
	application_ = NULL;
	mainWindow_ = NULL;
	mainCanvas_ = NULL;
	mainContext_ = NULL;
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
	zmatrixDialog = NULL;
	
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
	scriptMovieWidget = NULL;
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
	msg.enter("GuiQt::initialise");
	// Create the QApplication
	#if QT_VERSION >= 0x040600
	QGL::setPreferredPaintEngine(QPaintEngine::OpenGL);
	#endif
	
	#ifdef Q_OS_LINUX
	applicationType_ = getenv("DISPLAY") == NULL ? QApplication::Tty : QApplication::GuiClient;
	#else
	applicationType_ = QApplication::GuiClient;
	#endif
	application_ = new QApplication(argc, argv, applicationType_);
	
	// Initialise application name, organisation and author
	QCoreApplication::setOrganizationName("ProjectAten");
	QCoreApplication::setOrganizationDomain("www.projectaten.net");
	QCoreApplication::setApplicationName("Aten");

	// Create GUI window here (used to be done in GuiQt::run(), but this would cause GLX crashes under some circumstances, apparently as a result of the lack of ownership of the TCanvas)
	if (applicationType_ == QApplication::GuiClient)
	{
		mainWindow_ = new AtenForm;

		// Create the main QGLWidget
		QGLFormat format;
		format.setSampleBuffers(TRUE);
		mainContext_ = new QGLContext(format);

		// Create the widget
		mainCanvas_ = new TCanvas(mainContext_, mainWindow_);
		mainCanvas_->probeFeatures();
		mainCanvas_->setGeometry(0,0,800,600);
		mainCanvas_->setCursor(Qt::ArrowCursor);
	}
	
	msg.exit("GuiQt::initialise");
}

// Initialise and create GUI
void GuiQt::run()
{
	msg.enter("GuiQt::run");
	
	// Check application type
	if (applicationType_ == QApplication::Tty)
	{
		msg.print("Can't launch GUI when running in console mode.\n");
		msg.exit("GuiQt::run");
		return;
	}

	// If no model loaded, add one
	if (aten.nModels() == 0)
	{
		Model *m = aten.addModel();
		m->enableUndoRedo();
		m->regenerateIcon();
	}

	// Initialise Qt's icons resource
	Q_INIT_RESOURCE(icons);

	// ...dialog windows...
	prefsDialog = new AtenPrefs(mainWindow_);
	forcefieldEditorDialog = new AtenForcefieldEditor(mainWindow_);
	loadModelDialog = new AtenLoadModel(mainWindow_);
	selectFilterDialog = new AtenSelectFilter(mainWindow_);
	selectPatternDialog = new AtenSelectPattern(mainWindow_);
	selectVariableDialog = new AtenSelectVariable(mainWindow_);
	selectElementDialog = new AtenSelectElement(mainWindow_);
	aboutDialog = new AtenAbout(mainWindow_);
	viewBasisDialog = new AtenViewBasis(mainWindow_);
	viewEigenvectorDialog = new AtenViewEigenvector(mainWindow_);
	progressDialog = new AtenProgress(mainWindow_);
	zmatrixDialog = new AtenZMatrix(mainWindow_);
	
	// ...dock widgets
	atomListWidget = new AtomListWidget(mainWindow_, Qt::Tool);
	buildWidget = new BuildWidget(mainWindow_, Qt::Tool);
	cellDefinitionWidget = new CellDefinitionWidget(mainWindow_, Qt::Tool);
	cellTransformWidget = new CellTransformWidget(mainWindow_, Qt::Tool);
	commandWidget = new CommandWidget(mainWindow_, Qt::Tool);
	disorderWizard = new DisorderWizard(mainWindow_);
	forcefieldsWidget = new ForcefieldsWidget(mainWindow_, Qt::Tool);
	fragmentsWidget = new FragmentsWidget(mainWindow_, Qt::Tool);
	geometryWidget = new GeometryWidget(mainWindow_, Qt::Tool);
	glyphsWidget = new GlyphsWidget(mainWindow_, Qt::Tool);
	gridsWidget = new GridsWidget(mainWindow_, Qt::Tool);
	mdWidget = new MDWidget(mainWindow_, Qt::Tool);
	messagesWidget = new MessagesWidget(mainWindow_, Qt::Tool);
	modelListWidget = new ModelListWidget(mainWindow_, Qt::Tool);
	positionWidget = new PositionWidget(mainWindow_, Qt::Tool);
	poresWidget = new PoresWidget(mainWindow_, Qt::Tool);
	scriptMovieWidget = new ScriptMovieWidget(mainWindow_, Qt::Tool);
	selectWidget = new SelectWidget(mainWindow_, Qt::Tool);
	toolBoxWidget = new ToolBoxWidget(mainWindow_, Qt::Tool);
	trajectoryWidget = new TrajectoryWidget(mainWindow_, Qt::Tool);
	transformWidget = new TransformWidget(mainWindow_, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(mainWindow_, Qt::Tool);
	dockWidgets_ << atomListWidget << buildWidget << cellDefinitionWidget << cellTransformWidget << commandWidget << fragmentsWidget << geometryWidget << glyphsWidget << gridsWidget << mdWidget << messagesWidget << modelListWidget << poresWidget << positionWidget << scriptMovieWidget << selectWidget << toolBoxWidget << trajectoryWidget << transformWidget << vibrationsWidget;
// 	toolBoxWidget->show();
	
	// Connect Finished signal of tool windows to finished slots in structure
	foreach (QDockWidget *obj, dockWidgets_)
	{
		QObject::connect(obj, SIGNAL(visibilityChanged(bool)), toolBoxWidget, SLOT(dockWidgetVisibilityChanged(bool)));
		QObject::connect(obj, SIGNAL(topLevelChanged(bool)), toolBoxWidget, SLOT(dockWidgetTopLevelChanged(bool)));
		// Add every dock widget to a dock area (annoying to have to do it, but prevents 'stuck' dock widgets on some versions)
		mainWindow_->addDockWidget(Qt::RightDockWidgetArea, obj);
	}
	QObject::connect(zmatrixDialog, SIGNAL(finished(int)), zmatrixDialog, SLOT(dialogFinished(int)));// TGAY

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
	mainWindow_->finaliseUi();
	loadModelDialog->finaliseUi();

	// Set controls in the windows
	mainWindow_->setControls();
	prefsDialog->setControls();
	loadModelDialog->setControls();
	fragmentsWidget->refresh();
	commandWidget->refresh();

	// Show the main window, and flag it as existing
	mainWindow_->show();
	doesExist_ = TRUE;

	// Refresh the necessary windows, including the mainwindow
	gridsWidget->refresh();
	forcefieldsWidget->refresh();
	mdWidget->refresh();
	cellDefinitionWidget->refresh();
	cellTransformWidget->refresh();
	mainWindow_->update();
	commandWidget->refreshScripts();
	modelListWidget->refresh();
	atomListWidget->refresh();
	toolBoxWidget->updateButtons();
	gui.mainWindow()->updateControls();

	// Reset view of all loaded models
	for (Model *m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView();

	gui.mainCanvas()->setDrawingTarget(TCanvas::ScreenTarget);
	gui.mainCanvas()->postRedisplay(TRUE);

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
	msg.print("<b>Aten</b> version %s (from %s) built on %s, Copyright (C) 2007-2012  T. Youngs.\n", ATENVERSION, ATENURL, ATENDATE);
	msg.print("<b>Aten</b> uses Space Group Info (c) 1994-96 Ralf W. Grosse-Kunstleve.\n");
	msg.print("<b>Aten</b> comes with ABSOLUTELY NO WARRANTY.\n");
	msg.print("This is free software, and you are welcome to redistribute it under certain conditions.\n");
	msg.print("For more details read the GPL at <http://www.gnu.org/copyleft/gpl.html>.\n\n");

	// Set some preferences back to their default values
	prefs.setZMapType(ElementMap::AutoZMap, FALSE);
	prefs.setKeepView(FALSE);

	// Attempt to detect corrupt screen (requiring manualswapbuffers to be set in order to fix it)
// 	QTimer::singleShot(2000, mainCanvas_, SLOT(isRenderingOk()));
	
	// Enter main message processing loop
	application_->exec();

	msg.exit("GuiQt::run");
}

// Return the PID of Aten
int GuiQt::pid()
{
	#if QT_VERSION >= 0x040400
	return (application_ == NULL ? 0 : application_->applicationPid());
	#else
	static int pid = AtenMath::random(50000)+1000;
	return pid;
	#endif
}

// Process application messages
void GuiQt::processMessages()
{
	if (application_ != NULL) QCoreApplication::processEvents(QEventLoop::AllEvents, 50);
}

// Set interactivity (to full or zero), except for main view camera changes
void GuiQt::setInteractive(bool interactive)
{
	// Set enabled status of all the dock widgets..
	foreach( QObject *obj, dockWidgets_) obj->setProperty("enabled", interactive);

	// ...and the main toolbar...
	mainWindow_->ui.MainToolbar->setEnabled(interactive);
	
	// ...and set the canvas 'editability'
	mainCanvas_->setEditable(interactive);
}

/*
// Refresh Functions
*/

// Update GUI after model change (or different model selected) (accessible wrapper to call AtenForm's function)
void GuiQt::update(int targets)
{
	if (!doesExist_) return;

	// Refresh aspects of main window and dock widgets
	mainWindow_->update();
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
		if (lastAction != mainCanvas_->selectedMode())
		{
			lastAction = mainCanvas_->selectedMode();
			text.sprintf("<b>%s:</b> %s", UserActions[lastAction].name, UserActions[lastAction].unModified);
			if (UserActions[lastAction].shiftModified[0] != '\0') text.strcatf(", <b>+shift</b> %s", UserActions[lastAction].shiftModified);
			if (UserActions[lastAction].ctrlModified[0] != '\0') text.strcatf(", <b>+ctrl</b> %s", UserActions[lastAction].ctrlModified);
			if (UserActions[lastAction].altModified[0] != '\0') text.strcatf(", <b>+alt</b> %s", UserActions[lastAction].altModified);
		}
		// Set text in statusbar widget
// 		if (clear) mainWindow_->setMessageLabel("");  TGAY
		mainWindow_->setMessageLabel(text);
	}
	
	// Request redraw of the main canvas
	if (targets&GuiQt::CanvasTarget) gui.mainCanvas()->postRedisplay();
}

// Initialise (but don't show) the progress dialog
void GuiQt::initialiseProgressDialog()
{
	progressDialog->initialise();
	
	application_->processEvents();
}

// Update progress dialog, showing the window and disabling GUI in the process
void GuiQt::updateProgressDialog()
{
	if (progressDialog->isHidden()) progressDialog->show();
	progressDialog->updateProgress();
}

// Send signal to close progress dialog
void GuiQt::terminateProgressDialog()
{
	progressDialog->terminate();
	application_->processEvents();
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
	messagesWidget->ui.MessagesBrowser->moveCursor(QTextCursor::End);
	messagesWidget->ui.MessagesBrowser->append(str);
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
			aten.setCurrentModel(m, TRUE);
			// Create a model message dialog
			text.sprintf("Model '%s' has been modified.\n", m->name());
			returnvalue = QMessageBox::warning(mainWindow_, "Aten", text.get(), QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
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
					// Temporarily disable undo/redo for the model, save, and re-enable
					m->disableUndoRedo();
					// If model has a filter set, just save it
					f = m->filter();
					if (f != NULL) f->executeWrite(m->filename(), rv);
					else if (mainWindow_->runSaveModelDialog())
					{
						m->setFilter(mainWindow_->saveModelFilter);
						m->setFilename(mainWindow_->saveModelFilename.get());
						mainWindow_->saveModelFilter->executeWrite(m->filename(), rv);
					}
					else
					{
						m->enableUndoRedo();
						return FALSE;
					}
					m->enableUndoRedo();
					break;
			}
		}
	}
	return TRUE;
}

// Save image of current view
bool GuiQt::saveImage(const char *filename, BitmapFormat bf, int width, int height, int quality)
{
	msg.enter("GuiQt::saveImage");
	if (bf == GuiQt::nBitmapFormats)
	{
		msg.print("Invalid bitmap format given to Gui::saveImage().\n");
		msg.exit("GuiQt::saveImage");
		return FALSE;
	}

	QPixmap pixmap;
	// Get current mainCanvas_ geometry if none was specified
	if (width == 0) width = mainCanvas_->width();
	if (height == 0) height = mainCanvas_->height();
	// Temporarily adjust label size...
	int oldlabelsize = prefs.labelSize();
	int newlabelsize = int (oldlabelsize*( (1.0*height / mainCanvas_->height()) ));
	prefs.setLabelSize(newlabelsize);

	pixmap = mainCanvas_->generateImage(width, height, TRUE);

	// Restore label size
	prefs.setLabelSize(oldlabelsize);

	pixmap.save(filename, GuiQt::bitmapFormatExtension(bf), quality);
	msg.print("Saved current view as '%s' [%ix%i %s]\n", filename, width, height, GuiQt::bitmapFormatFilter(bf));

	msg.exit("GuiQt::saveImage");
	return TRUE;
}

// Return main view Widget
TCanvas *GuiQt::mainCanvas()
{
	return mainCanvas_;
}

// Main application structure
QApplication *GuiQt::application()
{
	return application_;
}

// Return type of application initialised
QApplication::Type GuiQt::applicationType()
{
	return applicationType_;
}

// Main Window
AtenForm *GuiQt::mainWindow()
{
	return mainWindow_;
}
