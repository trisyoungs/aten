/*
	*** Qt main window
	*** src/gui/mainwindow.h
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

#ifndef ATEN_MAINWINDOW_H
#define ATEN_MAINWINDOW_H

#include "base/dnchar.h"
#include "gui/canvas.h"
#include "gui/ui_mainwindow.h"
#include "gui/ui_prefs.h"

// Stack Pages (must be in order of pages in the stack)
enum StackPage { SP_ATOMS, SP_EDIT, SP_TRANSFORM, SP_POSITION, SP_CELLDEFINE, SP_CELLMANIPULATE, SP_MINIMISER, SP_DISORDER, SP_FORCEFIELD, SP_GRID, SP_ANALYSE, SP_NITEMS };

#define MAXRECENTFILES 10

// Forward Declarations
class QDoubleSpinBox;
class QLabel;
class QTimer;
class QLineEdit;
class QProgressBar;
class QPushButton;
class QFrame;
class QSettings;
class QActionGroup;
class QButtonGroup;
class Forest;
class Tree;

class AtenForm : public QMainWindow
{
	// All Qt declarations must include this macro
	Q_OBJECT

	/*
	// Window Functions
	*/
	public:
	// Constructor
	AtenForm(QMainWindow *parent = 0);
	// Main form declaration
	Ui::MainWindow ui;
	// Finalise widgets (things that we couldn't do in Qt Designer)
	void finaliseUi();
	// Create filter combos on file dialogs
	void createDialogFilters();
	// Set controls to reflect program variables
	void setControls();
	protected:
	void closeEvent(QCloseEvent *event);
	public slots:
	void keyPressEvent(QKeyEvent*);
	void keyReleaseEvent(QKeyEvent*);

	/*
	// Model Management
	*/
	private slots:
	void on_ModelTabs_currentChanged(int value);
	void on_ModelTabs_doubleClicked(int tabid);
	public:
	// Refresh names in ModelTabs
	void refreshModelTabs();


	/*
	// Atom Context Menu Actions
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
	void on_actionAtomProbe_triggered(bool checked);

	/*
	// Bonding Actions
	*/
	private:
	QDoubleSpinBox *bondToleranceSpin_;
	private slots:
	void on_actionCalculateBonding_triggered(bool checked);
	void on_actionClearBonding_triggered(bool checked);
	void on_actionCalculateBondingSelection_triggered(bool checked);
	void on_actionClearBondingSelection_triggered(bool checked);
	void on_actionAugmentBonding_triggered(bool checked);
	void bondTolerance_valueChanged(double d);

	/*
	// Command Actions
	*/
	private:
	QLineEdit *commandEdit_;
	private slots:
	void executeCommand();

	/*
	// Draw Actions
	*/
	private:
	// Current custom element
	int customElement_;
	private slots:
	void on_actionDrawAtom_triggered(bool checked);
	void on_actionDrawChain_triggered(bool checked);
	void on_actionDeleteAtom_triggered(bool checked);
	void on_actionTransmuteAtom_triggered(bool checked);
	void on_actionBondSingle_triggered(bool checked);
	void on_actionBondDouble_triggered(bool checked);
	void on_actionBondTriple_triggered(bool checked);
	void on_actionDeleteBond_triggered(bool checked);
	void on_actionElementH_triggered(bool checked);
	void on_actionElementC_triggered(bool checked);
	void on_actionElementN_triggered(bool checked);
	void on_actionElementCustom_triggered(bool checked);
	void on_actionSelectCustomElement_triggered(bool checked);
	void on_actionAddHydrogen_triggered(bool checked);
	void on_actionAddHydrogenAtom_triggered(bool checked);

	/*
	// Draw style Actions
	*/
	private slots:
	void on_StyleToolbar_actionTriggered(QAction *action);

	/*
	// Edit Actions
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
	// Forcefield Actions
	*/
	private:
	QComboBox *forcefieldCombo_;
	private slots:
	void on_actionMinimise_triggered(bool checked);
	void on_actionCalculateEnergy_triggered(bool checked);
	void on_actionCalculateForces_triggered(bool checked);
	void forcefieldCombo_currentIndexChanged(int i);
	public:
	void refreshForcefieldCombo();

	/*
	// Measure Actions
	*/
	private slots:
	void on_actionMeasureDistance_triggered(bool checked);
	void on_actionMeasureAngle_triggered(bool checked);
	void on_actionMeasureTorsion_triggered(bool checked);
	void on_actionMeasureClearAll_triggered(bool checked);
	void on_actionMeasureDistanceSelection_triggered(bool checked);
	void on_actionMeasureAngleSelection_triggered(bool checked);
	void on_actionMeasureTorsionSelection_triggered(bool checked);
	void on_actionMeasureList_triggered(bool checked);

	/*
	// Model Menu
	*/
	private slots:
	void on_actionModelRename_triggered(bool checked);
	void on_actionModelCreatePatterns_triggered(bool checked);
	void on_actionModelRemovePatterns_triggered(bool checked);
	void on_actionModelListPatterns_triggered(bool checked);
	void on_actionModelFFType_triggered(bool checked);
	void on_actionModelFFUntype_triggered(bool checked);
	void on_actionModelFoldAtoms_triggered(bool checked);
	void on_actionModelFoldMolecules_triggered(bool checked);
	void on_actionModelNext_triggered(bool checked);
	void on_actionModelPrevious_triggered(bool checked);
	void on_actionModelShowAll_triggered(bool checked);

	/*
	// Mouse Actions
	*/
	private slots:
	void on_actionMouseInteract_triggered(bool checked);
	void on_actionMouseRotate_triggered(bool checked);
	void on_actionMouseTranslate_triggered(bool checked);

	/*
	// Script Actions
	*/
	private:
	// Pointers to recent file actions
	Reflist<QAction, Forest*> scriptActions_;
	private slots:
	void runScript();
	void on_actionLoadScript_triggered(bool v);
	public:
	void refreshScriptsMenu();

	/*
	// Selection actions
	*/
	private slots:
	void on_actionSelectAtoms_triggered(bool checked);
	void on_actionSelectMolecules_triggered(bool checked);
	void on_actionSelectElement_triggered(bool checked);

	/*
	// Trajectory Actions
	*/
	private:
	QSlider *trajectorySlider_;
	QSpinBox *trajectorySpin_;
	bool trajectoryToolbarRefreshing_;
	private slots:
	void on_actionFrameNext_triggered(bool checked);
	void on_actionFramePrevious_triggered(bool checked);
	void on_actionFrameFirst_triggered(bool checked);
	void on_actionFrameLast_triggered(bool checked);
	void on_actionPlayPause_triggered(bool checked);
	void trajectorySlider_sliderMoved(int i);
	void trajectorySpin_valueChanged(int i);
	public:
	void updateTrajectoryToolbar();
	void setTrajectoryToolbarActive(bool active);

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
	void on_actionSchemeElement_triggered(bool checked);
	void on_actionSchemeCharge_triggered(bool checked);
	void on_actionSchemeForce_triggered(bool checked);

	/*
	// Window Show / Hide Actions
	*/
	private slots:
	void on_actionAtomlistWindow_triggered(bool checked);
	void on_actionBuildWindow_triggered(bool checked);
	void on_actionDisorderWindow_triggered(bool checked);
	void on_actionForcefieldsWindow_triggered(bool checked);
	void on_actionTransformWindow_triggered(bool checked);
	void on_actionPositionWindow_triggered(bool checked);
	void on_actionGridsWindow_triggered(bool checked);
// 	void on_actionGlyphsWindow_triggered(bool checked);
	void on_actionMinimiserWindow_triggered(bool checked);
	void on_actionCellDefineWindow_triggered(bool checked);
	void on_actionCellTransformWindow_triggered(bool checked);

	/*
	// System Functions
	*/
	private slots:
	void on_actionPreferences_triggered(bool checked);
	void on_actionReloadFilters_triggered(bool checked);

	/*
	// Help Functions
	*/
	private slots:
	void on_actionAboutAten_triggered(bool checked);
	void on_actionAboutQt_triggered(bool checked);

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
	// Text labels for model information and UI messages in status bar
	QLabel *infoLabel1, *infoLabel2, *messageLabel;
	// Statusbar progress indicator widgets
	QProgressBar *progressBar;
	QLabel *progressLabel;
	QPushButton *progressButton;
	QFrame *progressIndicator;
	// String lists to hold file dialog filter definitions
	QString loadModelFilters, saveModelFilters, loadTrajectoryFilters, saveTrajectoryFilters, loadExpressionFilters, saveExpressionFilters, saveBitmapFilters, saveVectorFilters, loadScriptFilters, loadGridFilters, saveGridFilters;
	// Filter set from save model dialog
	Tree *saveModelFilter;
	// Filename set from save model dialog
	Dnchar saveModelFilename;
	// Group for actions that determine the current user action
	QActionGroup *uaGroup;
	// Dummy toolbutton for inclusion in action group
	QAction *dummyToolButton;


	/*
	// Settings
	*/
	private:
	// Settings structure
	QSettings settings_;
	// Load settings
	void loadSettings();
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
