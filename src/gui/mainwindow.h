/*
	*** Main Window
	*** src/gui/mainwindow.h
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

#ifndef ATEN_MAINWINDOW_H
#define ATEN_MAINWINDOW_H

#include "base/dnchar.h"
#include "base/atom.h"
#include "classes/prefs.h"
#include "gui/ui_mainwindow.h"
#include "gui/ui_prefs.h"
#include "gui/useractions.h"
#include "templates/reflist.h"
#include "base/glyph.h"

#define MAXRECENTFILES 10

// Forward Declarations
class QLabel;
class QProgressBar;
class QPushButton;
class QFrame;
class QSettings;
class QActionGroup;
class QButtonGroup;
class QStringListModel;
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
	// Constructor / Destructor
	AtenForm(QMainWindow *parent = 0);
	~AtenForm();
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


	/*
	// Refresh Functions
	*/
	private:
	// Refresh window title
	void updateWindowTitle();

	public:
	// Refresh main window to reflect model change
	void update();
	// Update trajectory control widgets
	void updateTrajectoryMenu();
	// Cancel any current mode and return to select
	void cancelCurrentMode();

	
	/*
	// File Menu / Actions
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
	// Edit Menu / Actions
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


	/*
	// View Menu / Actions
	*/
	public:
	void setCartesianView(double x, double y, double z);
	void setCellView(double x, double y, double z);
	void setActiveSchemeAction(Prefs::ColouringScheme cs);
	private slots:
	void on_actionViewReset_triggered(bool checked);
	void on_actionViewZoomIn_triggered(bool checked);
	void on_actionViewZoomOut_triggered(bool checked);
	void on_actionViewPerspective_triggered(bool checked);
	void on_actionViewOrthographic_triggered(bool checked);
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
	void on_actionSchemeVelocity_triggered(bool checked);
	void on_actionSchemeCustom_triggered(bool checked);


	/*
	// Selection Menu / Actions (doubles as Atom Context Menu)
	*/
	public:
	void activateGlyphActions(int n);
	private:
	void setAtomStyle(Atom::DrawStyle ds);
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
	// Model Menu
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
	// Trajectory Menu
	*/
	private slots:
	void on_actionTrajectoryOpen_triggered(bool checked);
	void on_actionTrajectoryRemove_triggered(bool checked);
	void on_actionTrajectoryInheritParentStyle_triggered(bool checked);
	void on_actionTrajectoryFirstFrame_triggered(bool checked);
	void on_actionTrajectoryLastFrame_triggered(bool checked);
	void on_actionTrajectoryPlayPause_triggered(bool checked);
	void on_actionTrajectoryModel_triggered(bool checked);
	void on_actionTrajectoryFrames_triggered(bool checked);
	void on_actionTrajectorySaveMovie_triggered(bool checked);


	/*
	// Expression Menu
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


	/*
	// Settings Menu / Actions
	*/
	private slots:
	void on_actionPreferences_triggered(bool checked);
	void on_actionReloadFilters_triggered(bool checked);
	void on_actionShowToolBox_triggered(bool checked);
	void on_actionStoreDefaultWindowState_triggered(bool checked);


	/*
	// Help Menu / Actions
	*/
	private slots:
	void on_actionAboutAten_triggered(bool checked);
	void on_actionAboutQt_triggered(bool checked);

	
	/*
	// Main Toolbar (other actions not already account for by menus)
	*/
	private slots:
	void on_actionStyleStick_triggered(bool checked);
	void on_actionStyleTube_triggered(bool checked);
	void on_actionStyleSphere_triggered(bool checked);
	void on_actionStyleScaled_triggered(bool checked);
	void on_actionStyleIndividual_triggered(bool checked);
	void on_actionSelectAtoms_triggered(bool checked);
	void on_actionSelectMolecules_triggered(bool checked);
	void on_actionSelectElement_triggered(bool checked);
	public:
	void setActiveStyleAction(Atom::DrawStyle ds);


	/*
	// Mouse Toolbar Actions
	*/
	private slots:
	void on_actionMouseInteract_triggered(bool checked);
	void on_actionMouseRotate_triggered(bool checked);
	void on_actionMouseTranslate_triggered(bool checked);


	/*
	// Local Widgets and Routines
	*/
	private:
	// List of manually-created QActionGroups
	Reflist<QActionGroup,int> actionGroups_;
	// Action group for main toolbar select actions
	QActionGroup *uaSelectActions_;
	// Dummy button for user-action group
	QToolButton *uaDummyButton_;
	// User-Action button group
	QButtonGroup uaButtons_;
	// Text labels for model information and UI messages in status bar
	QLabel *infoLabel1_, *infoLabel2_, *messageLabel_;

	private slots:
	// Change current user action
	void uaButtonClicked(int id);

	public:
	// Update undo/redo labels
	void updateUndoRedo();
	// Set action/button to reflect supplied user action
	void setActiveUserAction(UserAction::Action ua);
	// Set message label text
	void setMessageLabel(const char *s);
	// String lists to hold file dialog filter definitions
	QString loadModelFilters, saveModelFilters, loadTrajectoryFilters, saveTrajectoryFilters, loadExpressionFilters, saveExpressionFilters, saveBitmapFilters, saveVectorFilters, loadScriptFilters, loadGridFilters, saveGridFilters;
	// Filter set from save model dialog
	Tree *saveModelFilter;
	// Filename set from save model dialog
	Dnchar saveModelFilename;


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
