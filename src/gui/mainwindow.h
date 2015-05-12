/*
	*** Main Window
	*** src/gui/mainwindow.h
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

#ifndef ATEN_MAINWINDOW_H
#define ATEN_MAINWINDOW_H

#include <QtWidgets/QButtonGroup>
#include <QSettings>
#include "base/atom.h"
#include "base/prefs.h"
#include "gui/ui_mainwindow.h"
#include "gui/ui_prefs.h"
#include "gui/useractions.h"
#include "templates/reflist.h"
#include "base/glyph.h"
#include "base/namespace.h"

#define MAXRECENTFILES 10

// Forward Declarations (Aten) 1 - Main Form and Windows
class AtenAbout;
class AtenPrefs;
class AtenForcefieldEditor;
class AtenProgress;

// Forward Declarations (Aten) 2 - Dialogs
class AtenLoadModel;
class AtenSelectFilter;
class AtenSelectPattern;
class AtenSelectElement;
class AtenSelectVariable;
class AtenViewBasis;
class AtenViewEigenvector;
class AtenZMatrix;

// Forward Declarations (Aten) 3 - Dock Widgets and Wizards
class AtomListWidget;
class CommandWidget;
class DisorderWizard;
class ForcefieldsWidget;
class FragmentsWidget;
class GlyphsWidget;
class GridsWidget;
class MDWidget;
class PoresWidget;
class PositionWidget;
class ScriptMovieWidget;
class SelectWidget;
class ToolBoxWidget;
class TrajectoryWidget;
class TransformWidget;
class VibrationsWidget;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Tree;
class Aten;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

class AtenWindow : public QMainWindow
{
	// All Qt declarations must include this macro
	Q_OBJECT

	public:
	// Constructor / Destructor
	AtenWindow(Aten& aten);
	~AtenWindow();
	// Main form declaration
	Ui::AtenWindow ui;

	protected:
	void closeEvent(QCloseEvent* event);


	/*
	 * Aten Reference
	 */
	private:
	// Aten reference
	Aten& aten_;

	public:
	// Return reference to Aten
	Aten& aten();


	/*
	 * Methods
	 */
	public:
	// Close specified model, saving first if requested
	bool closeModel(Model* m);
	// Save before close
	bool saveBeforeClose();
	// Set interactivity (to full or zero), except for main view camera changes
	void setInteractive(bool interactive);


	/*
	 * Update Functions
	 */
	private:
	// Whether window is currently refreshing
	bool refreshing_;

	public:
	// Initial update and show
	void initialUpdateAndShow();
	// Refresh main window
	void updateMainWindow();
	// Update trajectory control widgets
	void updateTrajectoryMenu();
	// Refreshes specified (or all) dock widgets
	void updateWidgets(int targets = 0);
	// Refresh main viewer
	void postRedisplay();


	/*
	 * File Menu / Actions
	 */
	public:
	bool runSaveModelDialog();

	private slots:
	void on_actionFileNew_triggered(bool checked);
	void on_actionFileOpen_triggered(bool checked);
	void on_actionFileSave_triggered(bool checked);
	void on_actionFileSaveAs_triggered(bool checked);
	void on_actionExportOptions_triggered(bool checked);
	void on_actionFileClose_triggered(bool checked);
	void on_actionFileSaveImage_triggered(bool checked);
	void on_actionFileQuit_triggered(bool checked);
	void on_actionFileOpenGrid_triggered(bool checked);

	
	/*
	 * Edit Menu / Actions
	 */
	private slots:
	void on_actionEditUndo_triggered(bool checked);
	void on_actionEditRedo_triggered(bool checked);
	void on_actionEditCut_triggered(bool checked);
	void on_actionEditCopy_triggered(bool checked);
	void on_actionEditPaste_triggered(bool checked);
	void on_actionEditPasteTranslated_triggered(bool checked);
	void on_actionEditDelete_triggered(bool checked);
	void on_actionSelectionAll_triggered(bool checked);
	void on_actionSelectionNone_triggered(bool checked);
	void on_actionSelectionExpand_triggered(bool checked);
	void on_actionSelectionInvert_triggered(bool checked);
	void on_actionEditQuickCommand_triggered(bool checked);


	/*
	 * Selection Menu / Actions (doubles as Atom Context Menu)
	 */
	public:
	void activateGlyphActions(int n);

	private:
	// Atom under mouse when context menu was called
	Atom* contextAtom_;
	void setAtomStyle(Prefs::DrawStyle ds);
	void setAtomLabel(Atom::AtomLabel al);
	void removeAtomLabels(bool all);
	void setAtomHidden(bool hidden);
	QAction *createGlyphActions[Glyph::nGlyphTypes];

	private slots:
	void on_actionAtomStyleStick_triggered(bool checked);
	void on_actionAtomStyleTube_triggered(bool checked);
	void on_actionAtomStyleSphere_triggered(bool checked);
	void on_actionAtomStyleScaled_triggered(bool checked);
	void on_actionAtomLabelID_triggered(bool checked);
	void on_actionAtomLabelCharge_triggered(bool checked);
	void on_actionAtomLabelFFType_triggered(bool checked);
	void on_actionAtomLabelElement_triggered(bool checked);
	void on_actionAtomLabelFFEquiv_triggered(bool checked);
	void on_actionAtomLabelClear_triggered(bool checked);
	void on_actionAtomLabelClearAll_triggered(bool checked);
	void on_actionAtomColourReset_triggered(bool checked);
	void on_actionAtomColourSet_triggered(bool checked);
	void on_actionOrderShiftUp_triggered(bool checked);
	void on_actionOrderShiftDown_triggered(bool checked);
	void on_actionOrderMoveToStart_triggered(bool checked);
	void on_actionOrderMoveToEnd_triggered(bool checked);
	void on_actionOrderReorder_triggered(bool checked);
	void on_actionAtomHide_triggered(bool checked);
	void on_actionAtomProbe_triggered(bool checked);
	void on_actionAtomFixPosition_triggered(bool checked);
	void on_actionAtomFreePosition_triggered(bool checked);
	void on_actionSetBondLength_triggered(bool checked);
	void on_actionSetBondAngle_triggered(bool checked);
	void on_actionSetTorsionAngle_triggered(bool checked);
	void on_actionCreateFragment_triggered(bool checked);
	void on_actionCentreAtOrigin_triggered(bool checked);
	void createGlyph();


	/*
	 * Model Menu
	 */
	private slots:
	void on_actionModelRename_triggered(bool checked);
	void on_actionModelFoldAtoms_triggered(bool checked);
	void on_actionModelFoldMolecules_triggered(bool checked);
	void on_actionModelNext_triggered(bool checked);
	void on_actionModelPrevious_triggered(bool checked);
	void on_actionModelShowAll_triggered(bool checked);
	void on_actionListMeasurements_triggered(bool checked);

	
	/*
	 * Trajectory Menu
	 */
	private slots:
	void on_actionTrajectoryOpen_triggered(bool checked);
	void on_actionTrajectoryRemove_triggered(bool checked);
	void on_actionTrajectoryInheritParentStyle_triggered(bool checked);
	void on_actionTrajectoryCopyStyleToParent_triggered(bool checked);
	void on_actionTrajectoryPropagateStyleFromHere_triggered(bool checked);
	void on_actionTrajectoryFirstFrame_triggered(bool checked);
	void on_actionTrajectoryLastFrame_triggered(bool checked);
	void on_actionTrajectoryPlayPause_triggered(bool checked);
	void on_actionTrajectoryModel_triggered(bool checked);
	void on_actionTrajectoryFrames_triggered(bool checked);
	void on_actionTrajectorySaveMovie_triggered(bool checked);


	/*
	 * Expression Menu
	 */
	private slots:
	void on_actionOpenForcefield_triggered(bool checked);
	void on_actionOpenExpression_triggered(bool checked);
	void on_actionSaveExpression_triggered(bool checked);
	void on_actionModelCreatePatterns_triggered(bool checked);
	void on_actionModelRemovePatterns_triggered(bool checked);
	void on_actionModelListPatterns_triggered(bool checked);
	void on_actionModelFFType_triggered(bool checked);
	void on_actionModelFFUntype_triggered(bool checked);
	void on_actionModelCreateExpression_triggered(bool checked);
	void on_actionModelAddDefaultPattern_triggered(bool checked);


	/*
	 * Settings Menu / Actions
	 */
	private slots:
	void on_actionPreferences_triggered(bool checked);
	void on_actionReloadFilters_triggered(bool checked);
	void on_actionStoreDefaultWindowState_triggered(bool checked);
	void on_actionManualSwapBuffers_triggered(bool checked);


	/*
	 * Help Menu / Actions
	 */
	private slots:
	void on_actionAboutAten_triggered(bool checked);
	void on_actionAboutQt_triggered(bool checked);


	/*
	 * Build Panel
	 */
	private:
	// Update build panel
	void updateBuildPanel(Model* sourceModel);

	private slots:
	// Select
	void on_BuildSelectAtomsButton_clicked(bool checked);
	void on_BuildSelectBoundButton_clicked(bool checked);
	void on_BuildSelectElementButton_clicked(bool checked);
	void on_BuildSelectExpandButton_clicked(bool checked);
	void on_BuildSelectInvertButton_clicked(bool checked);
	// Draw
	void on_BuildDrawDrawButton_clicked(bool checked);
	void on_BuildDrawFragmentButton_clicked(bool checked);
	void on_BuildDrawDeleteButton_clicked(bool checked);
	void on_BuildDrawTransmuteButton_clicked(bool checked);
	void on_BuildDrawAddHButton_clicked(bool checked);
	void on_BuildDrawGrowButton_clicked(bool checked);
	// Elements
	// Bonding
	void on_BuildBondingRebondButton_clicked(bool checked);
	void on_BuildBondingAugmentButton_clicked(bool checked);
	void on_BuildBondingClearButton_clicked(bool checked);

	public:
	// Return currently-selected build element
	int currentBuildElement();


	/*
	 * Cell Panel
	 */
	private:
	// Update cell panel
	void updateCellPanel(Model* sourceModel);

	private slots:
	// Define
	void on_CellDefinePeriodicButton_clicked(bool checked);
	// Transform
	void on_CellTransformReplicateButton_clicked(bool checked);
	void on_CellTransformScaleButton_clicked(bool checked);
	// Miller
	void on_CellMillerSelectButton_clicked(bool checked);


	/*
	 * View Panel
	 */
	private:
	// Update view panel
	void updateViewPanel(Model* sourceModel);

	private slots:
	// Control
	void on_ViewControlResetButton_clicked(bool checked);
	void on_ViewControlZoomInButton_clicked(bool checked);
	void on_ViewControlZoomOutButton_clicked(bool checked);
	void on_ViewControlPerspectiveButton_clicked(bool checked);
	void on_ViewControlOrthographicButton_clicked(bool checked);
	// Style
	void on_ViewStyleLineButton_clicked(bool checked);
	void on_ViewStyleTubeButton_clicked(bool checked);
	void on_ViewStyleSphereButton_clicked(bool checked);
	void on_ViewStyleScaledButton_clicked(bool checked);
	void on_ViewStyleOwnButton_clicked(bool checked);
	// Colour
	void on_ViewSchemeElementButton_clicked(bool checked);
	void on_ViewSchemeChargeButton_clicked(bool checked);
	void on_ViewSchemeForceButton_clicked(bool checked);
	void on_ViewSchemeVelocityButton_clicked(bool checked);
	void on_ViewSchemeOwnButton_clicked(bool checked);
	// Options
	void on_ViewOptionsHBondsCheck_clicked(bool checked);


	/*
	 * Calculate Panel
	 */
	private:
	// Update calculate panel
	void updateCalculatePanel(Model* sourceModel);

	private slots:
	void on_CalculateMeasureDistanceButton_clicked(bool checked);
	void on_CalculateMeasureAngleButton_clicked(bool checked);
	void on_CalculateMeasureTorsionButton_clicked(bool checked);
	void on_CalculateMeasureClearButton_clicked(bool checked);


	/*
	 * Transform Panel
	 */
	private:
	// Update transform panel
	void updateTransformPanel(Model* sourceModel);

	private slots:
	void on_TransformSetDistanceButton_clicked(bool checked);
	void on_TransformSetAngleButton_clicked(bool checked);
	void on_TransformSetTorsionButton_clicked(bool checked);


	/*
	 * Grids Panel
	 */
	private:
	// Update grid panel
	void updateGridsPanel(Model* sourceModel);
	// Update current grid information
	void updateGridInformation(Grid* sourceGrid);

	private slots:
	void on_GridsList_currentItemChanged(QListWidgetItem* current, QListWidgetItem* previous);
	void on_GridsPrimaryLowerCutoffSpin_valueChanged(double value);
	void on_GridsPrimaryUpperCutoffSpin_valueChanged(double value);
	void on_GridsPrimaryColourButton_clicked(bool checked);
	void on_GridsSecondarySurfaceCheck_clicked(bool checked);
	void on_GridsSecondaryLowerCutoffSpin_valueChanged(double value);
	void on_GridsSecondaryUpperCutoffSpin_valueChanged(double value);
	void on_GridsSecondaryColourButton_clicked(bool checked);


	/*
	 * Model List
	 */
	private:
	// Whether model list is currently refreshing
	bool modelListRefreshing_;

	private slots:
	void on_ModelList_itemSelectionChanged();

	public:
	// Refresh model list
	void updateModelList();


	/*
	 * Messages Control
	 */
	public:
	// Types of message display
	enum MessageDisplay { FullMessages, MessagesOverScene, MessagesUnderScene, NoMessages };
	// Current message display type
	MessageDisplay messageDisplay_;

	private slots:
	void on_MessagesCycleButton_clicked(bool checked);
	void on_MessagesCopyButton_clicked(bool checked);
	void on_MessagesClearButton_clicked(bool checked);
	void on_MessagesScroll_sliderMoved(int position);

	public:
	// Return current message display style
	MessageDisplay messageDisplay();
	// Update messages widgets
	void updateMessagesWidgets();
	// Return current position of messages scrollbar
	int messagesScrollPosition();


	/*
	 * Local Widgets and Routines
	 */
	private:
	// List of manually-created QActionGroups
	Reflist<QActionGroup,int> actionGroups_;
	// Text labels for model information and UI messages in status bar
	QLabel *infoLabel1_, *infoLabel2_, *messageLabel_;
	// Filter set from save model dialog
	Tree* saveModelFilter_;
	// Filename set from save model dialog
	QString saveModelFilename_;

	public:
	// Update any controls related to Prefs values etc.
	void updateControls();
	// Update undo/redo labels
	void updateUndoRedo();
	// Set action/button to reflect supplied user action
	void setActiveUserAction(UserAction::Action ua);
	// Set message label text
	void setMessageLabel(QString message);


	/*
	 * Context Menu
	 */
	public:
	// Update context menu
	void updateContextMenu();
	// Call the atom context menu
	void callContextMenu(Atom*, int, int);


	/*
	 * Settings
	 */
	private:
	// Settings structure
	QSettings settings_;
	// Load settings
	void loadSettings();
	// Save settings
	void saveSettings();


	/*
	 * Recent files
	 */
	private slots:
	// Load recent file
	void loadRecent();

	private:
	// Pointers to recent file actions
	QAction* actionRecentFile[MAXRECENTFILES];

	public:
	// Add file to top of recent list
	void addRecent(QString filename);


	/*
	 * Dock Widgets
	 */
	public:
	// Update Targets
	enum UpdateTarget { AtomsTarget = 1, ForcefieldsTarget = 4, GlyphsTarget = 8, GridsTarget = 16, CanvasTarget = 64, StatusBarTarget = 128, GeometryTarget = 256, VibrationsTarget = 512, SelectTarget = 1024, TrajectoryTarget = 2048, AllTarget = 4095 };

	private:
	// List of dock widgets
	QList<QDockWidget*> dockWidgets_;
	
	public:
	// Atom list dock widget
	AtomListWidget *atomListWidget;
	// Command dock widget
	CommandWidget *commandWidget;
	// Disorder wizard
	DisorderWizard *disorderWizard;
	// Forcefields dock widget
	ForcefieldsWidget *forcefieldsWidget;
	// Fragment Library dock widget
	FragmentsWidget *fragmentsWidget;
	// Glyphs dock widget
	GlyphsWidget *glyphsWidget;
	// Grids dock widget
	GridsWidget *gridsWidget;
	// Pore builder dock widget
	PoresWidget *poresWidget;
	// Atom positioning dock widget
	PositionWidget *positionWidget;
	// Scripted movie dock widget
	ScriptMovieWidget *scriptMovieWidget;
	// Atom selection dock widget
	SelectWidget *selectWidget;
	// Trajectory control dock widget
	TrajectoryWidget *trajectoryWidget;
	// Atom transformation dock widget
	TransformWidget *transformWidget;
	// Vibrations dock widget
	VibrationsWidget *vibrationsWidget;


	/*
	 * Image Generation
	 */
	public:
	// Bitmap formats
	enum BitmapFormat { BitmapBMP, BitmapPG, BitmapPNG, BitmapPPM, BitmapXBM, BitmapX11, nBitmapFormats };
	static BitmapFormat bitmapFormat(QString s, bool reportError = false);
	static BitmapFormat bitmapFormatFromFilter(const char* s);
	static const char* bitmapFormatFilter(BitmapFormat bf);
	static const char* bitmapFormatExtension(BitmapFormat bf);

	public:
	// Save image of current view
	QPixmap scenePixmap(int width, int height);
	// Return pixmap of specified model
	QPixmap modelPixmap(Model* model, QSize pixmapSize);
};

#endif
