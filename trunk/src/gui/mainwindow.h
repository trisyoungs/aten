/*
	*** Qt main window
	*** src/gui/mainwindow.h
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

#ifndef ATEN_MAINWINDOW_H
#define ATEN_MAINWINDOW_H

#include "classes/dnchar.h"
#include "gui/canvas.h"
#include "gui/ui_mainwindow.h"
#include "gui/ui_prefs.h"

// Stack Pages (must be in order of pages in the stack)
enum StackPage { SP_ATOMS, SP_EDIT, SP_TRANSFORM, SP_POSITION, SP_CELLDEFINE, SP_CELLMANIPULATE, SP_MINIMISER, SP_DISORDER, SP_FORCEFIELD, SP_GRID, SP_ANALYSE, SP_NITEMS };

// Bitmap Formats
enum bitmap_format { BIF_BMP, BIF_JPG, BIF_PNG, BIF_PPM, BIF_XBM, BIF_X11, BIF_NITEMS };
bitmap_format BIF_from_text(const char *);
const char *filter_from_BIF(bitmap_format);
const char *extension_from_BIF(bitmap_format);

#define MAXRECENTFILES 10

// Forward Declarations
class QFileDialog;
class QLabel;
class QTimer;
class QLineEdit;
class QProgressBar;
class QPushButton;
class QFrame;
class QSettings;
class QActionGroup;
class QButtonGroup;
class CommandList;
class Filter;

class AtenForm : public QMainWindow
{
	// All Qt declarations must include this macro
	Q_OBJECT

	/*
	// Widgets
	*/
	public:
	// Constructor
	AtenForm(QMainWindow *parent = 0);
	// Main form declaration
	Ui::MainWindow ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Set controls to reflect program variables
	void setControls();

	protected:
	void closeEvent(QCloseEvent *event);

	/*
	// Input
	*/
	public slots:
	void keyPressEvent(QKeyEvent*);
	void keyReleaseEvent(QKeyEvent*);

	/*
	// Model management functions
	*/
	private slots:
	// Model Management
	void on_ModelTabs_currentChanged(int value);
	public:
	// Refresh names in ModelTabs
	void refreshModelTabs();

	/*
	// Editing Actions
	*/
	private slots:
	void on_actionEditUndo_triggered(bool checked);
	void on_actionEditRedo_triggered(bool checked);
	void on_actionEditCut_triggered(bool checked);
	void on_actionEditCopy_triggered(bool checked);
	void on_actionEditPaste_triggered(bool checked);
	void on_actionEditDelete_triggered(bool checked);
	void on_actionEditSelectAll_triggered(bool checked);
	void on_actionEditSelectNone_triggered(bool checked);
	void on_actionEditSelectExpand_triggered(bool checked);
	void on_actionEditInvert_triggered(bool checked);

	/*
	// Draw style Actions
	*/
	private slots:
	void on_StyleToolBar_actionTriggered(QAction *action);

	/*
	// Mouse Toolbar
	*/
	private slots:
	void on_actionMouseInteract_triggered(bool checked);
	void on_actionMouseRotate_triggered(bool checked);
	void on_actionMouseTranslate_triggered(bool checked);

	/*
	// Select Toolbar
	*/
	private:
	void setUserAction(bool checked, Canvas::UserAction ua);
	private slots:
	void on_actionSelectAtoms_triggered(bool on);
	void on_actionSelectMolecules_triggered(bool on);
	void on_actionSelectElement_triggered(bool on);

	/*
	// File Actions
	*/
	public:
	bool runSaveModelDialog();
	private slots:
	void on_actionFileNew_triggered(bool checked);
	void on_actionFileOpen_triggered(bool checked);
	void on_actionFileAddTrajectory_triggered(bool checked);
	void on_actionFileSave_triggered(bool checked);
	void on_actionFileSaveAs_triggered(bool checked);
	void on_actionFileClose_triggered(bool checked);
	void on_actionFileSaveImage_triggered(bool checked);
	void on_actionFileQuit_triggered(bool checked);
	void on_actionFileOpenForcefield_triggered(bool checked);
	//void on_actionFileSaveForcefield_triggered(bool checked);
	void on_actionFileOpenGrid_triggered(bool checked);
	void on_actionFileSaveExpression_triggered(bool checked);

	/*
	// View Actions
	*/
	public:
	void setCartesianView(double x, double y, double z);
	void setCellView(double x, double y, double z);
	private slots:
	void on_actionViewReset_triggered(bool checked);
	void on_actionViewZoomIn_triggered(bool checked);
	void on_actionViewZoomOut_triggered(bool checked);
	void on_actionViewPerspective_triggered(bool checked);
	void on_actionViewOrthographic_triggered(bool checked);
	void on_actionViewModel_triggered(bool checked);
	void on_actionViewTrajectory_triggered(bool checked);
	void on_actionViewSetCartesianPosX_triggered(bool checked);
	void on_actionViewSetCartesianPosY_triggered(bool checked);
	void on_actionViewSetCartesianPosZ_triggered(bool checked);
	void on_actionViewSetCartesianNegX_triggered(bool checked);
	void on_actionViewSetCartesianNegY_triggered(bool checked);
	void on_actionViewSetCartesianNegZ_triggered(bool checked);
	void on_actionViewSetCellNegX_triggered(bool checked);
	void on_actionViewSetCellNegY_triggered(bool checked);
	void on_actionViewSetCellNegZ_triggered(bool checked);
	void on_actionViewSetCellPosX_triggered(bool checked);
	void on_actionViewSetCellPosY_triggered(bool checked);
	void on_actionViewSetCellPosZ_triggered(bool checked);

	/*
	// Model Actions
	*/
	private slots:
	void on_actionFFType_triggered(bool checked);
	void on_actionFFUntype_triggered(bool checked);
	void on_actionFoldAtoms_triggered(bool checked);
	void on_actionFoldMolecules_triggered(bool checked);
	void on_actionModelNext_triggered(bool checked);
	void on_actionModelPrevious_triggered(bool checked);
	void on_actionModelShowAll_triggered(bool checked);

	/*
	// Trajectory Actions
	*/
	private slots:
	void on_actionFrameNext_triggered(bool checked);
	void on_actionFramePrevious_triggered(bool checked);
	void on_actionFrameFirst_triggered(bool checked);
	void on_actionFrameLast_triggered(bool checked);
	void on_actionPlayPause_triggered(bool checked);

	/*
	// Command Actions
	*/
	private:
	QLineEdit *commandEdit_;
	private slots:
	void executeCommand();

	/*
	// Script Actions
	*/
	private:
	// Pointers to recent file actions
	Reflist<QAction, CommandList* > scriptActions_;
	private slots:
	void runScript();
	void on_actionLoadScript_triggered(bool v);
	public:
	void refreshScriptsMenu();

	/*
	// Toolbar Actions
	*/
	private slots:
	void on_actionFileToolBarVisibility_triggered(bool v);
	void on_actionEditToolBarVisibility_triggered(bool v);
	void on_actionStyleToolBarVisibility_triggered(bool v);
	void on_actionTrajectoryToolBarVisibility_triggered(bool v);
	void on_actionCommandToolBarVisibility_triggered(bool v);
	void on_actionMouseToolBarVisibility_triggered(bool v);
	void on_actionSelectToolBarVisibility_triggered(bool v);

	/*
	// Widget Stack Functions
	*/
	private:
	void switchStack(int buttonid, bool checked);
	QPushButton *stackButtons_[SP_NITEMS];
	private slots:
	void on_ShowAtomPageButton_clicked(bool checked);
	void on_ShowEditPageButton_clicked(bool checked);
	void on_ShowTransformPageButton_clicked(bool checked);
	void on_ShowPositionPageButton_clicked(bool checked);
	void on_ShowCellDefinePageButton_clicked(bool checked);
	void on_ShowCellManipulatePageButton_clicked(bool checked);
	void on_ShowMinimiserPageButton_clicked(bool checked);
	void on_ShowDisorderPageButton_clicked(bool checked);
	void on_ShowForcefieldsPageButton_clicked(bool checked);
	void on_ShowGridsPageButton_clicked(bool checked);
	void on_ShowAnalysePageButton_clicked(bool checked);

	// Atom Page Functions
	public:
	void refreshAtomPage();
	private:
	void peekScrollBar();
	void pokeScrollBar();
	private slots:
	void on_AtomTree_itemSelectionChanged();
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);

	// Edit Page Functions
	private:
	void setSketchElement(int el);
	private slots:
	void on_DrawAtomButton_clicked(bool on);
	void on_DrawChainButton_clicked(bool on);
	void on_DrawDeleteButton_clicked(bool on);
	void on_DrawTransmuteButton_clicked(bool on);
	void on_BondToleranceSpin_valueChanged(double d);
	void on_BondSingleButton_clicked(bool on);
	void on_BondDoubleButton_clicked(bool on);
	void on_BondTripleButton_clicked(bool on);
	void on_BondDeleteButton_clicked(bool on);
	void on_BondCalcButton_clicked(bool on);
	void on_BondClearButton_clicked(bool on);
	void on_BondCalcSelButton_clicked(bool on);
	void on_BondClearSelButton_clicked(bool on);
	void on_ElementHButton_clicked(bool on);
	void on_ElementCButton_clicked(bool on);
	void on_ElementNButton_clicked(bool on);
	void on_ElementUserButton_clicked(bool on);
	void on_ElementEdit_returnPressed();
	void on_BondAugmentButton_clicked(bool on);
	void on_AddHydrogenButton_clicked(bool on);
	void on_AtomAddHydrogenButton_clicked(bool on);
	void on_ProbeAtomButton_clicked(bool on);
	void on_AddAtomButton_clicked(bool on);

	// Analyse page functions
	private slots:
	void on_MeasureDistanceButton_clicked(bool on);
	void on_MeasureAngleButton_clicked(bool on);
	void on_MeasureTorsionButton_clicked(bool on);
	void on_RemoveMeasurementsButton_clicked(bool on);
	void on_MeasureDistanceSelectionButton_clicked(bool on);
	void on_MeasureAngleSelectionButton_clicked(bool on);
	void on_MeasureTorsionSelectionButton_clicked(bool on);

	// Transformation Page Functions
	private:
	void rotateSelection(double direction);
	void translateSelection(int axis, int dir);
	private slots:
	void on_RotateDefineOriginButton_clicked(bool on);
	void on_RotateDefineAxisButton_clicked(bool on);
	void on_RotateClockwiseButton_clicked(bool on);
	void on_RotateAnticlockwiseButton_clicked(bool on);
	void on_TranslatePosXButton_clicked(bool on);
	void on_TranslatePosYButton_clicked(bool on);
	void on_TranslatePosZButton_clicked(bool on);
	void on_TranslateNegXButton_clicked(bool on);
	void on_TranslateNegYButton_clicked(bool on);
	void on_TranslateNegZButton_clicked(bool on);

	// Position Page Functions
	private:
	void flipSelection(int axis);
	private slots:
	void on_FlipXButton_clicked(bool checked);
	void on_FlipYButton_clicked(bool checked);
	void on_FlipZButton_clicked(bool checked);
	void on_DefineCentreButton_clicked(bool checked);
	void on_CentreSelectionButton_clicked(bool checked);

	// Cell Definition Page Functions
	public:
	void refreshCellPages();
	void cellChanged();
	private slots:
	void on_CellDefinitionGroup_clicked(bool checked);
	void on_CellLengthASpin_valueChanged(double d);
	void on_CellLengthBSpin_valueChanged(double d);
	void on_CellLengthCSpin_valueChanged(double d);
	void on_CellAngleASpin_valueChanged(double d);
	void on_CellAngleBSpin_valueChanged(double d);
	void on_CellAngleCSpin_valueChanged(double d);
	void on_CellSpacegroupSetButton_clicked(bool checked);
	void on_CellSpacegroupEdit_returnPressed();
	void on_CellSpacegroupRemoveButton_clicked(bool checked);
	void on_CellSpacegroupPackButton_clicked(bool checked);

	// Cell Manipulate Page Functions
	private slots:
	void on_CellReplicateButton_clicked(bool checked);
	void on_CellScaleButton_clicked(bool checked);

	// Minimiser Page Functions
	private slots:
	void on_MinimiserMethodCombo_currentIndexChanged(int index);
	void on_MinimiseButton_clicked(bool checked);

	// Forcefield Page Functions
	public:
	void refreshForcefieldPage();
	void refreshForcefieldTypeList();
	void refreshForcefieldPatterns();
	private slots:
	void on_LoadForcefieldButton_clicked(bool checked);
	void on_RemoveForcefieldButton_clicked(bool checked);
	void on_EditForcefieldButton_clicked(bool checked);
	void on_AssignFFToCurrentButton_clicked(bool checked);
	void on_AssignFFToAllButton_clicked(bool checked);
	void on_AssignFFToPatternButton_clicked(bool clicked);
	void on_TypeModelButton_clicked(bool checked);
	void on_UntypeModelButton_clicked(bool checked);
	void on_ForcefieldList_currentRowChanged(int row);
	void on_ForcefieldList_itemClicked(QListWidgetItem *item);
	void on_ManualTypeSetButton_clicked(bool checked);
	void on_ManualTypeClearButton_clicked(bool checked);
	void on_ManualTypeTestButton_clicked(bool checked);
	void on_ManualTypeEdit_returnPressed();


	// Grid Page Functions
	public:
	void refreshGridsPage();
	private:
	void refreshGridInfo();
	void gridOriginChanged(int component, double value);
	void gridAxisChanged(int row, int component, double value);
	private slots:
	void on_LoadGridButton_clicked(bool checked);
	void on_RemoveGridButton_clicked(bool checked);
	void on_SaveGridButton_clicked(bool checked);
	void on_GridList_currentRowChanged(int row);
	void on_GridStyleCombo_currentIndexChanged(int index);
	void on_GridList_itemClicked(QListWidgetItem *item);
	void on_GridCutoffSpin_valueChanged(double d);
	void on_GridOriginXSpin_valueChanged(double d);
	void on_GridOriginYSpin_valueChanged(double d);
	void on_GridOriginZSpin_valueChanged(double d);
	void on_GridAxesAXSpin_valueChanged(double d);
	void on_GridAxesAYSpin_valueChanged(double d);
	void on_GridAxesAZSpin_valueChanged(double d);
	void on_GridAxesBXSpin_valueChanged(double d);
	void on_GridAxesBYSpin_valueChanged(double d);
	void on_GridAxesBZSpin_valueChanged(double d);
	void on_GridAxesCXSpin_valueChanged(double d);
	void on_GridAxesCYSpin_valueChanged(double d);
	void on_GridAxesCZSpin_valueChanged(double d);
	void on_GridColourButton_clicked(bool checked);
	void on_GridTransparencySpin_valueChanged(double d);

	// Disorder Page Functions
	public:
	void refreshDisorderPage();
	private:
	void refreshComponentData();
	void setComponentCoords(int centsize, int element, double value);
	private slots:
	void on_ComponentTable_itemSelectionChanged();
	void on_ComponentTable_itemChanged(QTableWidgetItem *item);
	//void on_PopulationSpin_valueChanged(int value);
	//void on_ComponentTranslateCheck_clicked(bool checked);
	//void on_ComponentRotateCheck_clicked(bool checked);
	void on_ComponentRegionCombo_currentIndexChanged(int index);
	void on_ShowRegionsCheck_clicked(bool checked);
	void on_DisorderStartButton_clicked(bool checked);
	void on_VDWScaleSpin_valueChanged(double d);
	void on_ComponentCentreXSpin_valueChanged(double d);
	void on_ComponentCentreYSpin_valueChanged(double d);
	void on_ComponentCentreZSpin_valueChanged(double d);
	void on_ComponentSizeXSpin_valueChanged(double d);
	void on_ComponentSizeYSpin_valueChanged(double d);
	void on_ComponentSizeZSpin_valueChanged(double d);

	/*
	// Atom Popup Functions
	*/
	private:
	void setAtomStyle(Atom::DrawStyle ds);
	void setAtomLabel(Atom::AtomLabel al);
	void removeAtomLabels(bool all);
	void setAtomHidden(bool hidden);

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
	void on_actionAtomHide_triggered(bool checked);

	/*
	// Settings Functions
	*/
	private slots:
	void on_actionPreferences_triggered(bool checked);

	/*
	// Local Routines
	*/
	private slots:
	// Cancel progress dialog
	void progressCancel();

	public:
	// Update undo/redo labels
	void updateUndoRedo();

	/*
	// Local Widgets
	*/
	public:
	// Main text label for status bar in main window
	QLabel *statusLabel;
	// Statusbar progress indicator widgets
	QProgressBar *progressBar;
	QLabel *progressLabel;
	QPushButton *progressButton;
	QFrame *progressIndicator;
	// File dialogs for filter types
	QFileDialog **dialog;
	// File dialogs for forcefields
	QFileDialog *openForcefieldDialog, *saveForcefieldDialog;
	// File dialog for save bitmap and save vector image
	QFileDialog *saveBitmapDialog, *saveVectorDialog;
	// File dialog for script loading
	QFileDialog *openScriptDialog;
	// Filter set from save model dialog
	Filter *saveModelFilter;
	// Filename set from save model dialog
	Dnchar saveModelFilename;
	// QButtonGroup for stackpage buttons
	QButtonGroup *uaGroup;
	// QActionGroup for SelectToolBar actions
	QActionGroup *selectGroup;
	// Dummy button for uaGroup (so we can have none selected)
	QPushButton *dummyButton;

	/*
	// Settings
	*/
	private:
	// Settings structure
	QSettings *settings_;
	// Save settings
	void saveSettings();

	/*
	// Recent files
	*/
	private slots:
	// Load recent file
	void loadRecent();

	private:
	// Pointers to recent file actions
	QAction *actionRecentFile[MAXRECENTFILES];

	public:
	// Add file to top of recent list
	void addRecent(const char*);
};

#endif
