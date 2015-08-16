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

#include "main/aten.h"
#include "gui/mainwindow.h"
#include "gui/loadmodel.h"
#include "gui/ffeditor.h"
#include "gui/selectpattern.h"
#include "gui/tdoublespindelegate.hui"
#include "model/model.h"
#include "model/clipboard.h"
#include "model/undostate.h"
#include "parser/commandnode.h"
#include <QtWidgets/QFileDialog>
#include <QKeyEvent>
#include <QtWidgets/QProgressBar>
#include <QShortcut>
#include "base/sysfunc.h"
#include <iostream>
#include <fstream>
#include "gui/popupbuildaddh.h"
#include "gui/popupbuildfragments.h"
#include "gui/popupbuildgrow.h"
#include "gui/popupbuildrebond.h"
#include "gui/popupbuildtransmute.h"
#include "gui/popupcellangles.h"
#include "gui/popupcelllengths.h"
#include "gui/popupcellmatrix.h"
#include "gui/popupcellmiller.h"
#include "gui/popupcellreplicate.h"
#include "gui/popupcellscale.h"
#include "gui/popupcolour.h"
#include "gui/popupelementcommon.h"
#include "gui/popupelementtable.h"
#include "gui/popupfileaten.h"
#include "gui/popupfileopen.h"
#include "gui/popupfilesave.h"
#include "gui/popupforcefieldsassign.h"
#include "gui/popupforcefieldsminimise.h"
#include "gui/popupgridmatrix.h"
#include "gui/popupgridorigin.h"
#include "gui/popupgridstyle.h"
#include "gui/popupmeasureangle.h"
#include "gui/popupmeasuredistance.h"
#include "gui/popupmeasuretorsion.h"
#include "gui/popupcellspacegroup.h"
#include "gui/popuptransformangle.h"
#include "gui/popuptransformcentre.h"
#include "gui/popuptransformconvert.h"
#include "gui/popuptransformdistance.h"
#include "gui/popuptransformflip.h"
#include "gui/popuptransformmultiply.h"
#include "gui/popuptransformreposition.h"
#include "gui/popuptransformrotate.h"
#include "gui/popuptransformshift.h"
#include "gui/popuptransformtorsion.h"
#include "gui/popuptransformtranslate.h"
#include "gui/popupviewcolourscheme.h"
#include "gui/popupviewreset.h"
#include "gui/popupviewstyle.h"

#include "gui/command.h"
#include "gui/disorderwizard.h"
#include "gui/glyphs.h"
#include "gui/pores.h"
#include "gui/scriptmovie.h"
#include "gui/vibrations.h"

// Constructor
AtenWindow::AtenWindow(Aten& aten) : QMainWindow(NULL), aten_(aten), exportImageDialog_(*this), disorderWizard_(*this)
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
	messageDisplay_ = MessagesUnderScene;
	refreshing_ = false;
	trajectoryTimerId_ = -1;

	// Public variables
	infoLabel1_ = NULL;
	infoLabel2_ = NULL;
	messageLabel_ = NULL;

	// Models List setup
	connect(ui.ModelsList, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(modelsListContextMenuRequested(QPoint)));

	// Atoms Table variables / setup
	atomsTableStructurePoint_ = -1;
	atomsTableSelectionPoint_ = -1;
	atomsTableLastModel_ = NULL;
	atomsTableShouldRefresh_ = true;
	atomsTablePrevClicked_ = NULL;
	atomsTableLastClicked_ = NULL;
	atomsTableLastHovered_ = NULL;
	atomsTableMaxRows_ = 0;
	atomsTableCurrentRootId_ = 0;
	atomsTableItemDelegates_[AtenWindow::AtomIdItem] = NULL;
	atomsTableItemDelegates_[AtenWindow::AtomElementItem] = NULL;
	atomsTableItemDelegates_[AtenWindow::AtomTypeItem] = NULL;
	atomsTableItemDelegates_[AtenWindow::AtomXItem] = new TDoubleSpinDelegate(this);
	atomsTableItemDelegates_[AtenWindow::AtomYItem] = new TDoubleSpinDelegate(this);
	atomsTableItemDelegates_[AtenWindow::AtomZItem] = new TDoubleSpinDelegate(this);
	atomsTableItemDelegates_[AtenWindow::AtomQItem] = new TDoubleSpinDelegate(this);
	// -- Set initial display items
	atomsTableVisibleItems_[AtenWindow::AtomIdItem] = ui.AtomsViewIdCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomElementItem] = ui.AtomsViewElementCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomTypeItem] = ui.AtomsViewTypeCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomXItem] = ui.AtomsViewXCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomYItem] = ui.AtomsViewYCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomZItem] = ui.AtomsViewZCheck->isChecked();
	atomsTableVisibleItems_[AtenWindow::AtomQItem] = ui.AtomsViewChargeCheck->isChecked();
	// -- Connect mouse-tracking signals to AtomTable
	QObject::connect(ui.AtomsTable, SIGNAL(tableMousePressed(QMouseEvent*)), this, SLOT(atomsTableMousePressEvent(QMouseEvent*)));
	QObject::connect(ui.AtomsTable, SIGNAL(tableMouseReleased(QMouseEvent*)), this, SLOT(atomsTableMouseReleaseEvent(QMouseEvent*)));
	QObject::connect(ui.AtomsTable, SIGNAL(tableMouseMoved(QMouseEvent*)), this, SLOT(atomsTableMouseMoveEvent(QMouseEvent*)));
	QObject::connect(ui.AtomsTable, SIGNAL(tableMouseWheeled(QWheelEvent*)), this, SLOT(atomsTableMouseWheelEvent(QWheelEvent*)));
	QObject::connect(ui.AtomsTable, SIGNAL(tableMouseDoubleClicked(QMouseEvent*)), this, SLOT(atomsTableMouseDoubleClickEvent(QMouseEvent*)));
	QObject::connect(ui.AtomsTable, SIGNAL(itemChanged(QTableWidgetItem*)), this, SLOT(atomsTableItemChanged(QTableWidgetItem*)));

	// Create dock widgets
	commandWidget = new CommandWidget(*this, Qt::Tool);
	glyphsWidget = new GlyphsWidget(*this, Qt::Tool);
	poresWidget = new PoresWidget(*this, Qt::Tool);
	scriptMovieWidget = new ScriptMovieWidget(*this, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(*this, Qt::Tool);
	dockWidgets_ << commandWidget << glyphsWidget << poresWidget << scriptMovieWidget << vibrationsWidget;

	int n;
	ReturnValue rv;

	// Add buttons related to user actions to our button group, add popup widgets to those buttons that have them, and set up anything else we need to
	// -- Home Panel (File)
	ui.HomeFileOpenButton->setPopupWidget(new FileOpenPopup(*this, ui.HomeFileOpenButton));
	ui.HomeFileSaveButton->setPopupWidget(new FileSavePopup(*this, ui.HomeFileSaveButton));
	ui.HomeFileAtenButton->setPopupWidget(new FileAtenPopup(*this, ui.HomeFileAtenButton), true);
	// -- Home Panel (Appearance)
	ui.HomeAppearanceLineButton->setGroup("ViewStyles", Prefs::LineStyle);
	ui.HomeAppearanceTubeButton->setGroup("ViewStyles", Prefs::TubeStyle);
	ui.HomeAppearanceSphereButton->setGroup("ViewStyles", Prefs::SphereStyle);
	ui.HomeAppearanceScaledButton->setGroup("ViewStyles", Prefs::ScaledStyle);
	ui.HomeAppearanceOwnStyleButton->setGroup("ViewStyles", Prefs::OwnStyle);
	ui.HomeAppearanceElementButton->setGroup("ColourSchemes", Prefs::ElementScheme);
	ui.HomeAppearanceChargeButton->setGroup("ColourSchemes", Prefs::ChargeScheme);
	ui.HomeAppearanceForceButton->setGroup("ColourSchemes", Prefs::ForceScheme);
	ui.HomeAppearanceVelocityButton->setGroup("ColourSchemes", Prefs::VelocityScheme);
	ui.HomeAppearanceOwnColourButton->setGroup("ColourSchemes", Prefs::OwnScheme);
	// -- View Panel (Control)
	ui.HomeViewResetButton->setPopupWidget(new ResetViewPopup(*this, ui.HomeViewResetButton));

	// -- Build Panel (Select)
	ui.BuildSelectAtomsButton->setGroup("UserActions", UserAction::SelectAction);
	ui.BuildSelectBoundButton->setGroup("UserActions", UserAction::SelectBoundAction);
	ui.BuildSelectElementButton->setGroup("UserActions", UserAction::SelectElementAction);
	// -- Build Panel (Build)
	ui.BuildDrawDrawButton->setGroup("UserActions", UserAction::DrawAtomsAction);
	ui.BuildDrawFragmentButton->setGroup("UserActions", UserAction::DrawFragmentsAction);
	ui.BuildDrawFragmentButton->setPopupWidget(new BuildFragmentsPopup(*this, ui.BuildDrawFragmentButton));
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
	ui.TransformPositionTranslateButton->setPopupWidget(new TransformTranslatePopup(*this, ui.TransformPositionTranslateButton), true);
	ui.TransformPositionShiftButton->setPopupWidget(new TransformShiftPopup(*this, ui.TransformPositionShiftButton), true);
	ui.TransformPositionRepositionButton->setPopupWidget(new TransformRepositionPopup(*this, ui.TransformPositionRepositionButton));
	// -- Transform Panel (Position)
	ui.TransformTransformRotateButton->setPopupWidget(new TransformRotatePopup(*this, ui.TransformTransformRotateButton), true);
	ui.TransformTransformMultiplyButton->setPopupWidget(new TransformMultiplyPopup(*this, ui.TransformTransformMultiplyButton));
	ui.TransformTransformConvertButton->setPopupWidget(new TransformConvertPopup(*this, ui.TransformTransformConvertButton));

	// -- Grids Panel (Manage)
	connect(ui.GridsList, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(gridsListContextMenuRequested(QPoint)));
	// -- Grids Panel (Define)
	ui.GridsDefineAxesButton->setPopupWidget(new GridMatrixPopup(*this, ui.GridsDefineAxesButton), true);
	ui.GridsDefineOriginButton->setPopupWidget(new GridOriginPopup(*this, ui.GridsDefineOriginButton), true);
	// -- Grids Panel (Primary Cutoff)
	ui.GridsPrimaryColourButton->setPopupWidget(new ColourPopup(*this, ui.GridsPrimaryColourButton), true);
	ui.GridsPrimaryStyleButton->setPopupWidget(new GridStylePopup(*this, ui.GridsPrimaryStyleButton, true), true);
	// -- Grids Panel (Secondary Cutoff)
	ui.GridsSecondaryColourButton->setPopupWidget(new ColourPopup(*this, ui.GridsSecondaryColourButton), true);
	ui.GridsSecondaryStyleButton->setPopupWidget(new GridStylePopup(*this, ui.GridsSecondaryStyleButton, false), true);

	// -- Select Panel (ID/Element)
	ui.SelectNETAElementButton->setPopupWidget(new ElementTablePopup(*this, ui.SelectNETAElementButton), true);
	ui.SelectNETAElementButton->callPopupMethod("setSelectedElement", rv = 6);

	// -- Forcefields Panel (Manage)
	ui.ForcefieldsManageAssignButton->setPopupWidget(new ForcefieldsAssignPopup(*this, ui.ForcefieldsManageAssignButton));
	ui.ForcefieldsCalculateMinimiseButton->setPopupWidget(new ForcefieldsMinimisePopup(*this, ui.ForcefieldsCalculateMinimiseButton));

	// -- Selection Panel (Appearance)
	ui.SelectionAppearanceStyleButton->setPopupWidget(new ViewStylePopup(*this, ui.SelectionAppearanceStyleButton), false, true);
	ui.SelectionAppearanceStyleButton->callPopupMethod("updateButtonIcon", rv = QString(Prefs::drawStyle(Prefs::SphereStyle)));
	ui.SelectionAppearanceColourButton->setPopupWidget(new ColourPopup(*this, ui.SelectionAppearanceColourButton), false);

	// Setup Shortcuts
	QShortcut* shortcut;
	// -- Model List
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Left), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.ModelsPreviousButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Right), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.ModelsNextButton, SLOT(click()));
	// Home Panel (File)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::ALT + Qt::Key_N), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileNewButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_O), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileOpenButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_S), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileSaveButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::ALT + Qt::Key_S), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileSaveAsButton, SLOT(click()));
	// Home Panel (Edit)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_C), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditCopyButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_X), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditCutButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_V), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditPasteButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Backspace), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditDeleteButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Z), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditUndoButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_Z), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditRedoButton, SLOT(click()));
	// Home Panel (View)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_R), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeViewResetButton, SLOT(click()));
	// Select Panel
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_A), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicAllButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_N), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicNoneButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_I), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicInvertButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_E), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicExpandButton, SLOT(click()));
	// Transform Panel
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::ALT + Qt::Key_C), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.TransformPositionZeroButton, SLOT(click()));
	// Main Window
	shortcut = new QShortcut(QKeySequence(Qt::Key_F10), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.QuickCommandToggleButton, SLOT(click()));


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

	// Create Context Menu
	createContextMenu();

	// Link QuickCommand line edit to run slot
	connect(ui.QuickCommandCombo->lineEdit(), SIGNAL(returnPressed()), this, SLOT(quickCommandRun()));
	ui.QuickCommandFrame->setVisible(false);

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

	// Load Qt Settings
	loadSettings();

	// Set controls in some windows
	commandWidget->refresh();

	// Reset view of all loaded models
	for (Model* m = aten.models(); m != NULL; m = m->next) if (!prefs.keepView()) m->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());

	// Refresh everything
	commandWidget->refreshScripts();
	updateWidgets(AtenWindow::AllTarget);

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

void AtenWindow::resizeEvent(QResizeEvent* event)
{
	// Update row information in AtomsTable
	atomsTableRecalculateRowSize();
}

void AtenWindow::wheelEvent(QWheelEvent* event)
{
// 	// Take the wheel event if the mouse was over the AtomsTable, otherwise ignore it
// 	printf("Geom = %i %i %i %i, pos = %i %i\n", ui.AtomsTable->geometry().x(), ui.AtomsTable->geometry().y(), ui.AtomsTable->geometry().width(), ui.AtomsTable->geometry().height(), event->pos().x(), event->pos().y());
// 	if (ui.AtomsTable->geometry().contains(ui.AtomsTable->mapFromParent(event->pos())))
// 	{
// 		printf("WHeel on atoms table.\n");
// 	}
// 	else event->ignore();
}

/*
 * Methods
 */

// Set interactivity (to full or zero), except for main view camera changes
void AtenWindow::setInteractive(bool interactive)
{
	// Set enabled status of all the dock widgets..
	foreach( QObject *obj, dockWidgets_) obj->setProperty("enabled", interactive);
	
	// ...and set the canvas 'editability'
	ui.MainView->setEditable(interactive);
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

// Timer event
void AtenWindow::timerEvent(QTimerEvent* event)
{
	static bool trajectoryDrawFlag = false;

	if (event->timerId() == trajectoryTimerId_)
	{
		// Move on to the next frame in the trajectory
		// Check that we're not still drawing the last frame from the last timerEvent
		if (trajectoryDrawFlag) printf("Still drawing previous frame.\n");
		else
		{
			trajectoryDrawFlag = true;
			Model* currentModel = aten_.currentModel();
			if (currentModel)
			{
				currentModel->seekNextTrajectoryFrame();
				if (currentModel->trajectoryFrameIndex() == currentModel->nTrajectoryFrames()-1) ui.TrajectoryControlPlayButton->click();
				updateWidgets(AtenWindow::MainViewTarget);
			}
			trajectoryDrawFlag = false;
		}
	}
}