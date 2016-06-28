/*
	*** Main Window - Functions
	*** src/gui/mainwindow_funcs.cpp
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

#include "main/aten.h"
#include "gui/mainwindow.h"
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
// Popups
#include "gui/popupbuildaddatom.h"
#include "gui/popupbuildaddh.h"
#include "gui/popupbuildclear.h"
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
#include "gui/popupelementtable.h"
#include "gui/popupfileaten.h"
#include "gui/popupfileimage.h"
#include "gui/popupfileopen.h"
#include "gui/popupfilesave.h"
#include "gui/popupfilesession.h"
#include "gui/popupforcefieldsassign.h"
#include "gui/popupforcefieldsminimise.h"
#include "gui/popupforcefieldsopen.h"
#include "gui/popupgridmatrix.h"
#include "gui/popupgridorigin.h"
#include "gui/popupgridset.h"
#include "gui/popupgridshift.h"
#include "gui/popupgridstyle.h"
#include "gui/popupgridsopen.h"
#include "gui/popupmeasureangle.h"
#include "gui/popupmeasuredistance.h"
#include "gui/popupmeasuretorsion.h"
#include "gui/popupcellspacegroup.h"
#include "gui/popupporesdrill.h"
#include "gui/popupporesscheme.h"
#include "gui/popupscriptsopen.h"
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
// OLD
#include "gui/glyphs.h"
#include "gui/vibrations.h"

// Constructor
AtenWindow::AtenWindow(Aten& aten) : QMainWindow(NULL), aten_(aten), exportFilmDialog_(*this), exportImageDialog_(*this), disorderWizard_(*this), progressDialog_(*this), zMatrixDialog_(*this),
	openGridDialog_(this, aten_.workDir(), aten_.pluginStore().filePlugins(PluginTypes::GridFilePlugin)),
	openModelDialog_(this, aten_.workDir(), aten_.pluginStore().filePlugins(PluginTypes::ModelFilePlugin)),
	openTrajectoryDialog_(this, aten_.workDir(), aten_.pluginStore().filePlugins(PluginTypes::TrajectoryFilePlugin)),
	saveModelDialog_(this, aten_.workDir(), aten_.pluginStore().filePlugins(PluginTypes::ModelFilePlugin))
{
	Messenger::enter("AtenWindow::AtenWindow()");

	// Initialise Qt's icon resource
	Q_INIT_RESOURCE(icons);

	// Seutp user interface
	ui.setupUi(this);

	// Set pointers to Aten and AtenWindow in objects that need them
	ui.MainView->setAten(&aten_);
	ui.MainView->setAtenWindow(this);
	TMenuButton::setAtenWindow(this);

	// Private variables
	contextAtom_ = NULL;
	messageDisplay_ = MessagesUnderScene;
	refreshing_ = false;
	shown_ = false;
	trajectoryTimerId_ = -1;
	lastSelectionType_ = nSelectTargetTypes;

	// Interaction / User Modes
	clickedAtom_ = NULL;
	pickEnabled_ = false;
	actionBeforePick_ = UserAction::NoAction;
	activeMode_ = UserAction::NoAction;
	selectedMode_ = UserAction::SelectAction;
	currentDrawDepth_ = -5.0;
	buildGeometry_ = Atom::TetrahedralGeometry;
	editable_ = true;

	// Public variables
	massLabel_ = NULL;
	densityLabel_ = NULL;
	cellInfoLabel_ = NULL;
	atomsLabel_ = NULL;
	selectionLabel_ = NULL;
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
	glyphsWidget = new GlyphsWidget(*this, Qt::Tool);
	vibrationsWidget = new VibrationsWidget(*this, Qt::Tool);

	int n;
	ReturnValue rv;

	// Add buttons related to user actions to our button group, add popup widgets to those buttons that have them, and set up anything else we need to
	// -- Home Panel (File)
	ui.HomeFileOpenButton->setPopupWidget(new FileOpenPopup(*this, ui.HomeFileOpenButton));
	ui.HomeFileSaveButton->setPopupWidget(new FileSavePopup(*this, ui.HomeFileSaveButton));
	ui.HomeFileImageButton->setPopupWidget(new FileImagePopup(*this, ui.HomeFileImageButton));
	ui.HomeFileAtenButton->setPopupWidget(new FileAtenPopup(*this, ui.HomeFileAtenButton), true);
	ui.HomeFileSessionButton->setPopupWidget(new FileSessionPopup(*this, ui.HomeFileSessionButton), true);
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
	ui.BuildDrawAddAtomButton->setPopupWidget(new AddAtomPopup(*this, ui.BuildDrawAddAtomButton), true);
	ui.BuildDrawElementButton->setPopupWidget(new ElementTablePopup(*this, ui.BuildDrawElementButton), true);
	ui.BuildDrawElementButton->setGroup("CurrentElement");
	ui.BuildDrawElementButton->callPopupMethod("setCurrentElement", rv = 6);
	// -- Build Panel (Bonding)
	ui.BuildBondingRebondButton->setPopupWidget(new RebondPopup(*this, ui.BuildBondingRebondButton));
	ui.BuildBondingClearButton->setPopupWidget(new ClearPopup(*this, ui.BuildBondingClearButton));

	// -- Cell Panel (Define)
	ui.CellDefineAnglesButton->setPopupWidget(new CellAnglesPopup(*this, ui.CellDefineAnglesButton), true);
	ui.CellDefineLengthsButton->setPopupWidget(new CellLengthsPopup(*this, ui.CellDefineLengthsButton), true);
	ui.CellDefineMatrixButton->setPopupWidget(new CellMatrixPopup(*this, ui.CellDefineMatrixButton), true);
	// -- Cell Panel (Spacegroup)
	ui.CellSpacegroupSetButton->setPopupWidget(new CellSpacegroupPopup(*this, ui.CellSpacegroupSetButton), true);
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
	ui.TransformGeometryDistanceButton->setPopupWidget(new TransformDistancePopup(*this, ui.TransformGeometryDistanceButton));
	ui.TransformGeometryAngleButton->setPopupWidget(new TransformAnglePopup(*this, ui.TransformGeometryAngleButton));
	ui.TransformGeometryTorsionButton->setPopupWidget(new TransformTorsionPopup(*this, ui.TransformGeometryTorsionButton));
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
	ui.GridsManageOpenButton->setPopupWidget(new GridsOpenPopup(*this, ui.GridsManageOpenButton));
	connect(ui.GridsList, SIGNAL(customContextMenuRequested(QPoint)), this, SLOT(gridsListContextMenuRequested(QPoint)));
	// -- Grids Panel (Define)
	ui.GridsDefineAxesButton->setPopupWidget(new GridMatrixPopup(*this, ui.GridsDefineAxesButton), true);
	ui.GridsDefineOriginButton->setPopupWidget(new GridOriginPopup(*this, ui.GridsDefineOriginButton), true);
	// -- Grids Panel (Primary Surface)
	ui.GridsPrimarySetButton->setPopupWidget(new GridSetPopup(*this, ui.GridsPrimarySetButton, true), true);
	ui.GridsPrimaryColourButton->setPopupWidget(new ColourPopup(*this, ui.GridsPrimaryColourButton), true);
	ui.GridsPrimaryStyleButton->setPopupWidget(new GridStylePopup(*this, ui.GridsPrimaryStyleButton, true), true);
	// -- Grids Panel (Secondary Surface)
	ui.GridsSecondarySetButton->setPopupWidget(new GridSetPopup(*this, ui.GridsSecondarySetButton, false), true);
	ui.GridsSecondaryColourButton->setPopupWidget(new ColourPopup(*this, ui.GridsSecondaryColourButton), true);
	ui.GridsSecondaryStyleButton->setPopupWidget(new GridStylePopup(*this, ui.GridsSecondaryStyleButton, false), true);
	// -- Grids Panel (Transform)
	ui.GridsTransformTranslateButton->setPopupWidget(new GridShiftPopup(*this, ui.GridsTransformTranslateButton), true);

	// -- Select Panel (Intelligent)
	ui.SelectIntelligentElementButton->setPopupWidget(new ElementTablePopup(*this, ui.SelectIntelligentElementButton), true);
	ui.SelectIntelligentElementButton->callPopupMethod("setCurrentElement", rv = 6);

	// -- Forcefields Panel (Manage)
	ui.ForcefieldsManageOpenButton->setPopupWidget(new ForcefieldsOpenPopup(*this, ui.ForcefieldsManageOpenButton));
	ui.ForcefieldsManageAssignButton->setPopupWidget(new ForcefieldsAssignPopup(*this, ui.ForcefieldsManageAssignButton));
	ui.ForcefieldsCalculateMinimiseButton->setPopupWidget(new ForcefieldsMinimisePopup(*this, ui.ForcefieldsCalculateMinimiseButton));

	// -- Selection Panel (Appearance)
	ui.SelectionAppearanceStyleButton->setPopupWidget(new ViewStylePopup(*this, ui.SelectionAppearanceStyleButton), false);
	ui.SelectionAppearanceStyleButton->callPopupMethod("updateButtonIcon", rv = QString(Prefs::drawStyle(Prefs::SphereStyle)));
	ui.SelectionAppearanceColourButton->setPopupWidget(new ColourPopup(*this, ui.SelectionAppearanceColourButton), false);

	// -- Tools Panel (Scripts)
	ui.ToolsScriptsOpenButton->setPopupWidget(new ScriptsOpenPopup(*this, ui.ToolsScriptsOpenButton));
	// -- Tools Panel (Pores)
	ui.ToolsPoresDrillButton->setPopupWidget(new PoresDrillPopup(*this, ui.ToolsPoresDrillButton), true);
	ui.ToolsPoresSchemeButton->setPopupWidget(new PoresSchemePopup(*this, ui.ToolsPoresSchemeButton, aten_.poresPartitioningScheme()), true);

	// Setup Shortcuts
	QShortcut* shortcut;
	// -- Model List
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Up), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.ModelsPreviousButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Down), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.ModelsNextButton, SLOT(click()));
	// Home Panel (File)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_N), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileNewButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_O), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileOpenButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_S), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileSaveButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_S), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileSaveAsButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_W), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeFileCloseButton, SLOT(click()));
	// Home Panel (Edit)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_C), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditCopyButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_X), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditCutButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_V), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditPasteButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Delete), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditDeleteButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_Z), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditUndoButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_Z), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeEditRedoButton, SLOT(click()));
	// Home Panel (Style)
	shortcut = new QShortcut(QKeySequence(Qt::Key_F1), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceLineButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F2), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceTubeButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F3), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceSphereButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F4), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceScaledButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F5), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceOwnStyleButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F1), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceElementButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F2), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceChargeButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F3), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceForceButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F4), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceVelocityButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F5), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceOwnColourButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_H), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeAppearanceShowAllButton, SLOT(click()));
	// Home Panel (View)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_R), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeViewResetButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F8), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.HomeViewHBondsButton, SLOT(click()));
	// Cell Panel (Fold)
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_F), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.CellFoldAtomsButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::SHIFT + Qt::Key_F), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.CellFoldMoleculesButton, SLOT(click()));
	// Select Panel
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_A), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicAllButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_I), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicInvertButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_E), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectBasicExpandButton, SLOT(click()));
	// Selection Panel
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_H), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.SelectionAppearanceHideButton, SLOT(click()));
	// Transform Panel
	shortcut = new QShortcut(QKeySequence(Qt::CTRL + Qt::Key_0), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.TransformPositionZeroButton, SLOT(click()));
	// Main Window
	shortcut = new QShortcut(QKeySequence(Qt::Key_F10), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), ui.QuickCommandToggleButton, SLOT(click()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F11), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), this, SLOT(recreateGridsForView()));
	shortcut = new QShortcut(QKeySequence(Qt::Key_F12), this, 0, 0, Qt::ApplicationShortcut);
	connect(shortcut, SIGNAL(activated()), this, SLOT(snapshotCurrentView()));

	// Create Context Menu
	createContextMenu();

	// Connect additional signals
	// -- Link QuickCommand line edit to run slot
	connect(ui.QuickCommandCombo->lineEdit(), SIGNAL(returnPressed()), this, SLOT(quickCommandRun()));
	ui.QuickCommandFrame->setVisible(false);
	// -- Connect Select panel's intelligent select combo lineEdit to the 'Add' button
	connect(ui.SelectIntelligentTargetCombo->lineEdit(), SIGNAL(returnPressed()), ui.SelectIntelligentAddButton, SLOT(click()));

	/*
	 * Statusbar
	 */
	// Fix up the statusbar with a single big frame and no size grip
	ui.MainWindowStatusBar->setSizeGripEnabled(false);
	QFrame* frame = new QFrame(this);
	ui.MainWindowStatusBar->addPermanentWidget(frame,1);
	// Message label
	QHBoxLayout* lablayout = new QHBoxLayout(frame);
	messageLabel_ = new QLabel(this);
	messageLabel_->setTextFormat(Qt::RichText);
	messageLabel_->setWordWrap(true);
	QFont font = messageLabel_->font();
	font.setPointSize(8);
	messageLabel_->setFont(font);
	lablayout->addWidget(messageLabel_, 100);
	QFrame* sep = new QFrame;
	sep->setFrameStyle(QFrame::VLine);
	lablayout->addWidget(sep,0);
	// Info labels
	QGridLayout* infolayout = new QGridLayout;
	infolayout->setSizeConstraint(QLayout::SetMaximumSize);
	infolayout->setHorizontalSpacing(6);
	infolayout->setVerticalSpacing(2);
	infolayout->setMargin(0);
	modelLabel_ = new QLabel(this);
	modelLabel_->setFont(font);
	infolayout->addWidget(modelLabel_, 0, 0);
	cellInfoLabel_ = new QLabel(this);
	cellInfoLabel_->setFont(font);
	infolayout->addWidget(cellInfoLabel_, 1, 0);
	atomsLabel_ = new QLabel(this);
	atomsLabel_->setFont(font);
	infolayout->addWidget(atomsLabel_, 0, 1);
	selectionLabel_ = new QLabel(this);
	selectionLabel_->setFont(font);
	infolayout->addWidget(selectionLabel_, 1, 1);
	massLabel_ = new QLabel(this);
	massLabel_->setFont(font);
	infolayout->addWidget(massLabel_, 0, 2);
	densityLabel_ = new QLabel(this);
	densityLabel_->setFont(font);
	infolayout->addWidget(densityLabel_, 1, 2);
	lablayout->addLayout(infolayout,0);

	// Turn on space-saving
	TSpaceSaver::beginSpaceSaving();

	// Load Qt Settings
	loadSettings();

	// Reset view of all loaded models (unless KeepView option was set)
	for (Model* m = aten.models(); m != NULL; m = m->next)
	{
		if (!m->plugin()) continue;
		if (!m->plugin()->standardOptions().keepView()) m->resetView(ui.MainView->contextWidth(), ui.MainView->contextHeight());
	}

	// Refresh everything
	updateWidgets(AtenWindow::AllTarget);

	// Set the widget colours in prefs
	QColor colour;
	colour = palette().background().color();
	prefs.setColour(Prefs::WidgetBackgroundColour, colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());
	colour = palette().foreground().color();
	prefs.setColour(Prefs::WidgetForegroundColour, colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());	

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

/*
 * File Dialogs
 */

// Return model export dialog
AtenSaveModel& AtenWindow::saveModelDialog()
{
	return saveModelDialog_;
}

/*
 * Methods
 */

// Set interactivity (to full or zero), except for main view camera changes
// ATEN2 TODO Update this!
void AtenWindow::setInteractive(bool interactive)
{
	// ...and set the canvas 'editability'
	editable_ = interactive;
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
				updateWidgets(AtenWindow::MainViewTarget+AtenWindow::TrajectoryPanelTarget);
			}
			trajectoryDrawFlag = false;
		}
	}
}
