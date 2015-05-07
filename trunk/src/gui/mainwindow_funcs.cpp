/*
	*** Main Window - Functions
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

#include <QtWidgets/QMessageBox>
#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/prefs.h"
#include "gui/loadmodel.h"
#include "gui/trajectory.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/about.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "model/undostate.h"
#include "parser/commandnode.h"
#include <QtWidgets/QFileDialog>
#include <QKeyEvent>
#include <QtWidgets/QProgressBar>
#include "base/sysfunc.h"
#include "main/version.h"
#include <iostream>
#include <fstream>
#include "gui/popupbuildaddh.h"
#include "gui/popupbuildgrow.h"
#include "gui/popupbuildrebond.h"
#include "gui/popupbuildtransmute.h"
#include "gui/popupcellangles.h"
#include "gui/popupcelllengths.h"
#include "gui/popupcellmatrix.h"
#include "gui/popupcellmiller.h"
#include "gui/popupcellreplicate.h"
#include "gui/popupcellscale.h"
#include "gui/popupelementcommon.h"
#include "gui/popupelementtable.h"
#include "gui/popupmeasureangle.h"
#include "gui/popupmeasuredistance.h"
#include "gui/popupmeasuretorsion.h"
#include "gui/popupcellspacegroup.h"
#include "gui/popuptransformangle.h"
#include "gui/popuptransformcentre.h"
#include "gui/popuptransformdistance.h"
#include "gui/popuptransformflip.h"
#include "gui/popuptransformreposition.h"
#include "gui/popuptransformshift.h"
#include "gui/popuptransformtorsion.h"
#include "gui/popuptransformtranslate.h"
#include "gui/popupviewreset.h"

#include "gui/atomlist.h"
#include "gui/command.h"
#include "gui/disorderwizard.h"
#include "gui/forcefields.h"
#include "gui/fragments.h"
#include "gui/glyphs.h"
#include "gui/grids.h"
#include "gui/pores.h"
#include "gui/position.h"
#include "gui/scriptmovie.h"
#include "gui/select.h"
#include "gui/trajectory.h"
#include "gui/transform.h"
#include "gui/vibrations.h"

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
	modelListRefreshing_ = false;
	messageDisplay_ = MessagesUnderScene;
	refreshing_ = false;

	// Public variables
	infoLabel1_ = NULL;
	infoLabel2_ = NULL;
	messageLabel_ = NULL;

	// Create dock widgets
	atomListWidget = new AtomListWidget(*this, Qt::Tool);
	commandWidget = new CommandWidget(*this, Qt::Tool);
	disorderWizard = new DisorderWizard(*this);
	forcefieldsWidget = new ForcefieldsWidget(*this, Qt::Tool);
	fragmentsWidget = new FragmentsWidget(*this, Qt::Tool);
	glyphsWidget = new GlyphsWidget(*this, Qt::Tool);
	gridsWidget = new GridsWidget(*this, Qt::Tool);
	positionWidget = new PositionWidget(*this, Qt::Tool);
	poresWidget = new PoresWidget(*this, Qt::Tool);
	scriptMovieWidget = new ScriptMovieWidget(*this, Qt::Tool);
	selectWidget = new SelectWidget(*this, Qt::Tool);
	trajectoryWidget = new TrajectoryWidget(*this, Qt::Tool);
	transformWidget = new TransformWidget(*this, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(*this, Qt::Tool);
	dockWidgets_ << atomListWidget << commandWidget << fragmentsWidget << glyphsWidget << gridsWidget << poresWidget << positionWidget << scriptMovieWidget << selectWidget << transformWidget << vibrationsWidget;

	int n;
	ReturnValue rv;

	// Set up recent files list (create all actions first)
	for (n=0; n<MAXRECENTFILES; n++)
	{
		actionRecentFile[n] = new QAction(this);
		actionRecentFile[n]->setVisible(false);
		QObject::connect(actionRecentFile[n], SIGNAL(triggered()), this, SLOT(loadRecent()));
		ui.RecentMenu->addAction(actionRecentFile[n]);
	}

	// Create QActionGroup for model / trajectory render source
	QActionGroup* group = new QActionGroup(this);
	actionGroups_.add(group);
	group->addAction(ui.actionTrajectoryModel);
	group->addAction(ui.actionTrajectoryFrames);

	// Add style tool buttons to their button group
	ui.ViewStyleLineButton->setGroup("Styles");
	ui.ViewStyleTubeButton->setGroup("Styles");
	ui.ViewStyleSphereButton->setGroup("Styles");
	ui.ViewStyleScaledButton->setGroup("Styles");
	ui.ViewStyleOwnButton->setGroup("Styles");

	// Add colour scheme tool buttons to their button group
	ui.ViewSchemeElementButton->setGroup("Schemes");
	ui.ViewSchemeChargeButton->setGroup("Schemes");
	ui.ViewSchemeForceButton->setGroup("Schemes");
	ui.ViewSchemeVelocityButton->setGroup("Schemes");
	ui.ViewSchemeOwnButton->setGroup("Schemes");

	// Add view tool buttons to their button group
	ui.ViewControlPerspectiveButton->setGroup("View");
	ui.ViewControlOrthographicButton->setGroup("View");

	// Add buttons related to user actions to our button group, and add popup widgets to those buttons that have them

	// -- Build Panel (Select)
	ui.BuildSelectAtomsButton->setGroup("UserActions", UserAction::SelectAction);
	ui.BuildSelectBoundButton->setGroup("UserActions", UserAction::SelectBoundAction);
	ui.BuildSelectElementButton->setGroup("UserActions", UserAction::SelectElementAction);
	// -- Build Panel (Build)
	ui.BuildDrawDrawButton->setGroup("UserActions", UserAction::DrawAtomsAction);
	ui.BuildDrawFragmentButton->setGroup("UserActions", UserAction::DrawFragmentsAction);
	ui.BuildDrawDeleteButton->setGroup("UserActions", UserAction::DrawDeleteAction);
	ui.BuildDrawTransmuteButton->setGroup("UserActions", UserAction::DrawTransmuteAction);
	ui.BuildDrawTransmuteButton->setPopupWidget(new TransmutePopup(*this, ui.BuildDrawTransmuteButton));
	ui.BuildDrawAddHButton->setGroup("UserActions", UserAction::DrawAddHydrogenAction);
	ui.BuildDrawAddHButton->setPopupWidget(new AddHPopup(*this, ui.BuildDrawAddHButton));
	ui.BuildDrawGrowButton->setGroup("UserActions", UserAction::DrawGrowAtomsAction);
	ui.BuildDrawGrowButton->setPopupWidget(new GrowPopup(*this, ui.BuildDrawGrowButton));
	// -- Build Panel (Element)
	ui.BuildElementTableButton->setPopupWidget(new ElementTablePopup(*this, ui.BuildElementTableButton));
	ui.BuildElementTableButton->setGroup("CurrentElement");
	ui.BuildElementTableButton->callPopupMethod("setSelectedElement", rv = 6);
	ui.BuildElementCommonButton->setPopupWidget(new ElementCommonPopup(*this, ui.BuildElementCommonButton));
	ui.BuildElementCommonButton->setGroup("CurrentElement");
	ui.BuildElementCommonButton->callPopupMethod("setSelectedElement", rv = 8);
	// -- Build Panel (Bonding)
	ui.BuildBondingRebondButton->setPopupWidget(new RebondPopup(*this, ui.BuildBondingRebondButton));

	// -- Cell Panel (Define)
	ui.CellDefineAnglesButton->setPopupWidget(new CellAnglesPopup(*this, ui.CellDefineAnglesButton), true);
	ui.CellDefineLengthsButton->setPopupWidget(new CellLengthsPopup(*this, ui.CellDefineLengthsButton), true);
	ui.CellDefineMatrixButton->setPopupWidget(new CellMatrixPopup(*this, ui.CellDefineMatrixButton), true);
	// -- Cell Panel (Spacegroup)
	ui.CellSpacegroupSetButton->setPopupWidget(new CellSpacegroupPopup(*this, ui.CellSpacegroupSetButton));
	// -- Cell Panel (Transform)
	ui.CellTransformReplicateButton->setPopupWidget(new CellReplicatePopup(*this, ui.CellTransformReplicateButton));
	ui.CellTransformScaleButton->setPopupWidget(new CellScalePopup(*this, ui.CellTransformScaleButton));
	// -- Cell Panel (Miller)
	ui.CellMillerDefineButton->setPopupWidget(new CellMillerPopup(*this, ui.CellMillerDefineButton), true);

	// -- View Panel (Control)
	ui.ViewControlResetButton->setPopupWidget(new ResetViewPopup(*this, ui.ViewControlResetButton));

	// -- Calculate Panel (Measure)
	ui.CalculateMeasureDistanceButton->setGroup("UserActions", UserAction::MeasureDistanceAction);
	ui.CalculateMeasureDistanceButton->setPopupWidget(new MeasureDistancePopup(*this, ui.CalculateMeasureDistanceButton));
	ui.CalculateMeasureAngleButton->setGroup("UserActions", UserAction::MeasureAngleAction);
	ui.CalculateMeasureAngleButton->setPopupWidget(new MeasureAnglePopup(*this, ui.CalculateMeasureAngleButton));
	ui.CalculateMeasureTorsionButton->setGroup("UserActions", UserAction::MeasureTorsionAction);
	ui.CalculateMeasureTorsionButton->setPopupWidget(new MeasureTorsionPopup(*this, ui.CalculateMeasureTorsionButton));

	// -- Transform Panel (Set)
	ui.TransformSetDistanceButton->setPopupWidget(new TransformDistancePopup(*this, ui.TransformSetDistanceButton));
	ui.TransformSetAngleButton->setPopupWidget(new TransformAnglePopup(*this, ui.TransformSetAngleButton));
	ui.TransformSetTorsionButton->setPopupWidget(new TransformTorsionPopup(*this, ui.TransformSetTorsionButton));
	// -- Transform Panel (Position)
	ui.TransformPositionCentreButton->setPopupWidget(new TransformCentrePopup(*this, ui.TransformPositionCentreButton));
	ui.TransformPositionFlipButton->setPopupWidget(new TransformFlipPopup(*this, ui.TransformPositionFlipButton), true);
	ui.TransformPositionTranslateButton->setPopupWidget(new TransformTranslatePopup(*this, ui.TransformPositionTranslateButton));
	ui.TransformPositionShiftButton->setPopupWidget(new TransformShiftPopup(*this, ui.TransformPositionShiftButton));
	ui.TransformPositionRepositionButton->setPopupWidget(new TransformRepositionPopup(*this, ui.TransformPositionRepositionButton));
	
	
	
	
	
	

	// -- From Position Dock Widget
// 	uaButtons_.addButton(positionWidget->ui.ShiftPickVectorButton, UserAction::ShiftPickVectorAction);
// 	// -- From Transform Dock Widget
// 	uaButtons_.addButton(transformWidget->ui.TransformPickAButton, UserAction::TransformPickAAction);
// 	uaButtons_.addButton(transformWidget->ui.TransformPickBButton, UserAction::TransformPickBAction);
// 	uaButtons_.addButton(transformWidget->ui.TransformPickCButton, UserAction::TransformPickCAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickAButton, UserAction::ConvertSourcePickAAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickBButton, UserAction::ConvertSourcePickBAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertSourcePickCButton, UserAction::ConvertSourcePickCAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickAButton, UserAction::ConvertTargetPickAAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickBButton, UserAction::ConvertTargetPickBAction);
// 	uaButtons_.addButton(transformWidget->ui.ConvertTargetPickCButton, UserAction::ConvertTargetPickCAction);

	/*
	 * Statusbar
	 */
	// Fix up the statusbar with a single big frame and no size grip
	ui.MainWindowStatusBar->setSizeGripEnabled(false);
	QFrame *frame = new QFrame(this);
	ui.MainWindowStatusBar->addPermanentWidget(frame,1);
	// Message label
	QHBoxLayout *lablayout = new QHBoxLayout(frame);
	messageLabel_ = new QLabel(this);
	messageLabel_->setTextFormat(Qt::RichText);
	messageLabel_->setWordWrap(true);
	QFont font = messageLabel_->font();
	font.setPointSize(8);
	messageLabel_->setFont(font);
	lablayout->addWidget(messageLabel_, 100);
	QFrame *sep = new QFrame;
	sep->setFrameStyle(QFrame::VLine);
	lablayout->addWidget(sep,0);
	// Info labels
	QVBoxLayout *infolayout = new QVBoxLayout;
	infolayout->setSizeConstraint(QLayout::SetMaximumSize);
	infoLabel1_ = new QLabel(this);
	infoLabel1_->setFont(font);
	infolayout->addWidget(infoLabel1_);
	infoLabel2_ = new QLabel(this);
	infoLabel2_->setFont(font);
	infolayout->addWidget(infoLabel2_);
	lablayout->addLayout(infolayout,0);

	// Create glyph actions for Selection (atom context) menu
	QMenu *menu = new QMenu(this);
	for (int n=0; n<Glyph::nGlyphTypes; ++n)
	{
		createGlyphActions[n] = menu->addAction(Glyph::glyphTypeName( (Glyph::GlyphType) n));
		QObject::connect(createGlyphActions[n], SIGNAL(triggered()), this, SLOT(createGlyph()));
	}
	ui.actionCreateGlyph->setMenu(menu);

	// Load Qt Settings
	loadSettings();

	// Set controls in some windows
	fragmentsWidget->refresh();
	commandWidget->refresh();

	// Refresh the necessary windows, including the mainwindow
	gridsWidget->refresh();
	forcefieldsWidget->refresh();
	commandWidget->refreshScripts();
	atomListWidget->refresh();
	updateControls();
	updateWidgets();

	// Reset view of all loaded models
	for (Model* m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());

	postRedisplay();

	// Set some preferences back to their default values
	prefs.setZMapType(ElementMap::AutoZMap, false);
	prefs.setKeepView(false);

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
void AtenWindow::closeEvent(QCloseEvent* event)
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
	if (m->isModified())
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
				return false;
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
						return false;
					}
				}
				else
				{
					m->enableUndoRedo();
					return false;
				}
				break;
		}
	}

	// Remove model and update gui
	aten_.removeModel(m);

	// Update GUI
	updateWidgets(AtenWindow::AllTarget);
	
	return true;
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

		// Update GUI
		updateWidgets(AtenWindow::AllTarget);
	}
	else
	{
		// Remove file from recent files list
		int last, n;
		for (last=0; last<MAXRECENTFILES; last++) if (!actionRecentFile[last]->isVisible()) break;
		for (n=last+1; n<MAXRECENTFILES; ++n)
		{
			if (actionRecentFile[last]->isVisible())
			{
				actionRecentFile[n-1]->setText(actionRecentFile[n]->text());
				actionRecentFile[n-1]->setData(actionRecentFile[n]->data());
			}
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
	actionRecentFile[last]->setVisible(true);
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
	// Menu items
	ui.actionManualSwapBuffers->setChecked(prefs.manualSwapBuffers());
	ui.actionDetectDisplayHBonds->setChecked(prefs.drawHydrogenBonds());

	// Style
	TMenuButton::setGroupButtonChecked("Styles", Prefs::drawStyle(prefs.renderStyle()));

	// Colour scheme
	TMenuButton::setGroupButtonChecked("Schemes", Prefs::colouringScheme(prefs.colourScheme()));

	// View type
	prefs.hasPerspective() ? ui.ViewControlPerspectiveButton->setChecked(true) : ui.ViewControlOrthographicButton->setChecked(true);
}

// Update undo/redo actions in Edit menu
void AtenWindow::updateUndoRedo()
{
	Model* currentModel = aten_.currentModelOrFrame();

	// Check the model's state pointers
	if (currentModel && currentModel->currentUndoState())
	{
		ui.actionEditUndo->setText("Undo (" + currentModel->currentUndoState()->description() + ")");
		ui.actionEditUndo->setEnabled(true);
	}
	else
	{
		ui.actionEditUndo->setText("Undo");
		ui.actionEditUndo->setEnabled(false);
	}
	if (currentModel && currentModel->currentRedoState())
	{
		ui.actionEditRedo->setText("Redo (" + currentModel->currentRedoState()->description() + ")");
		ui.actionEditRedo->setEnabled(true);
	}
	else
	{
		ui.actionEditRedo->setText("Redo");
		ui.actionEditRedo->setEnabled(false);
	}
}

// Set action/button to reflect supplied user action
void AtenWindow::setActiveUserAction(UserAction::Action ua)
{
	// Set (check) relevant action or button based on supplied UserAction
	if (!TMenuButton::setGroupButtonChecked("UserActions", ua)) printf("AtenWindow::setActiveUserAction() - No button associated to user action %i.\n", ua);
}

// Set message label text
void AtenWindow::setMessageLabel(QString message)
{
	messageLabel_->setText(message);
}
