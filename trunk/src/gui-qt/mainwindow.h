/*
	*** Qt main window
	*** src/gui-qt/mainwindow.h
	Copyright T. Youngs 2007

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

#ifndef H_MAINWINDOW_H
#define H_MAINWINDOW_H

#include "base/master.h"
#include "gui/gui.h"
#include "gui-qt/ui_mainwindow.h"
#include "gui-qt/ui_prefs.h"

// Stack Pages
enum stack_page { SP_ATOMS, SP_EDIT, SP_TRANSFORM, SP_POSITION, SP_CELL, SP_MINIMISER, SP_DISORDER, SP_FORCEFIELD, SP_GRID, SP_ANALYSE, SP_NITEMS };

// Image Formats
enum pixmap_format { PF_BMP, PF_JPG, PF_PNG, PF_PPM, PF_XBM, PF_X11, PF_NITEMS };
const char *filter_from_PF(pixmap_format);
const char *extension_from_PF(pixmap_format);

#define MAXRECENTFILES 5

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
	void finalise_ui();
	// Set controls to reflect program variables
	void set_controls();

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
	void refresh_modeltabs();

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
	void on_actionMouseInteract_triggered(bool checked) { prefs.set_mb_action(MB_LEFT, MA_INTERACT); }
	void on_actionMouseRotate_triggered(bool checked) { prefs.set_mb_action(MB_LEFT, MA_VIEWROTATE); }
	void on_actionMouseTranslate_triggered(bool checked) { prefs.set_mb_action(MB_LEFT, MA_VIEWTRANSLATE); }

	/*
	// Select Toolbar
	*/
	private:
	void set_useraction(bool checked, user_action ua);
	private slots:
	void on_actionSelectAtoms_triggered(bool on) { set_useraction(on, UA_PICKSELECT); }
	void on_actionSelectMolecules_triggered(bool on) { set_useraction(on, UA_PICKFRAG); }
	void on_actionSelectElement_triggered(bool on) { set_useraction(on, UA_PICKELEMENT); }

	/*
	// File Actions
	*/
	public:
	bool run_savemodel_dialog();
	private slots:
	void on_actionFileNew_triggered(bool checked);
	void on_actionFileOpen_triggered(bool checked);
	void on_actionFileAddTrajectory_triggered(bool checked);
	void on_actionFileSave_triggered(bool checked);
	void on_actionFileSaveAs_triggered(bool checked);
	void on_actionFileClose_triggered(bool checked);
	void on_actionFileSaveImage_triggered(bool checked);
	void on_actionFileQuit_triggered(bool checked);
	void on_actionFileLoadForcefield_triggered(bool checked);
	void on_actionFileLoadGridData_triggered(bool checked);

	/*
	// View Actions
	*/
	private slots:
	void on_actionViewReset_triggered(bool checked);
	void on_actionViewZoomIn_triggered(bool checked);
	void on_actionViewZoomOut_triggered(bool checked);
	void on_actionViewPerspective_triggered(bool checked);
	void on_actionViewOrthographic_triggered(bool checked);
	void on_actionViewModel_triggered(bool checked);
	void on_actionViewTrajectory_triggered(bool checked);

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
	QLineEdit *command_edit;
	private slots:
	void execute_command();

	/*
	// Toolbar Actions
	*/
	private slots:
	void on_actionFileToolBarVisibility_triggered(bool v) { ui.FileToolBar->setVisible(v); }
	void on_actionEditToolBarVisibility_triggered(bool v) { ui.EditToolBar->setVisible(v); }
	void on_actionStyleToolBarVisibility_triggered(bool v) { ui.StyleToolBar->setVisible(v); }
	void on_actionTrajectoryToolBarVisibility_triggered(bool v) { ui.TrajectoryToolBar->setVisible(v); }
	void on_actionCommandToolBarVisibility_triggered(bool v) { ui.CommandToolBar->setVisible(v); }
	void on_actionMouseToolBarVisibility_triggered(bool v) { ui.MouseToolBar->setVisible(v); }
	void on_actionSelectToolBarVisibility_triggered(bool v) { ui.SelectToolBar->setVisible(v); }

	/*
	// Widget Stack Functions
	*/
	private:
	void switch_stack(int buttonid, bool checked);
	QPushButton *stackbuttons[SP_NITEMS];
	private slots:
	void on_ShowAtomPageButton_clicked(bool checked) { switch_stack(SP_ATOMS, checked); }
	void on_ShowEditPageButton_clicked(bool checked) { switch_stack(SP_EDIT, checked); }
	void on_ShowTransformPageButton_clicked(bool checked) { switch_stack(SP_TRANSFORM, checked); }
	void on_ShowPositionPageButton_clicked(bool checked) { switch_stack(SP_POSITION, checked); }
	void on_ShowCellPageButton_clicked(bool checked) { switch_stack(SP_CELL, checked); }
	void on_ShowMinimiserPageButton_clicked(bool checked) { switch_stack(SP_MINIMISER, checked); }
	void on_ShowDisorderPageButton_clicked(bool checked) { switch_stack(SP_DISORDER, checked); }
	void on_ShowForcefieldsPageButton_clicked(bool checked) { switch_stack(SP_FORCEFIELD, checked); }
	void on_ShowGridsPageButton_clicked(bool checked) { switch_stack(SP_GRID, checked); }
	void on_ShowAnalysePageButton_clicked(bool checked) { switch_stack(SP_ANALYSE, checked); }

	// Atom Page Functions
	public:
	void refresh_atompage();
	private:
	void peek_scroll_bar();
	void poke_scroll_bar();
	private slots:
	void on_AtomTreeList_itemSelectionChanged();
	void on_ShiftUpButton_clicked(bool checked);
	void on_ShiftDownButton_clicked(bool checked);
	void on_MoveToStartButton_clicked(bool checked);
	void on_MoveToEndButton_clicked(bool checked);

	// Edit Page Functions
	private slots:
	void on_DrawAtomButton_clicked(bool on) { if (on) set_useraction(on, UA_DRAWATOM); }
	void on_DrawChainButton_clicked(bool on) { if (on) set_useraction(on, UA_DRAWCHAIN); }
	void on_DrawDeleteButton_clicked(bool on) { if (on) set_useraction(on, UA_DELATOM); }
	void on_DrawTransmuteButton_clicked(bool on) { if (on) set_useraction(on, UA_TRANSATOM); }
	void on_BondToleranceSpin_valueChanged(double d) { prefs.set_bond_tolerance(d); }
	void on_BondSingleButton_clicked(bool on) { if (on) set_useraction(on, UA_BONDSINGLE); }
	void on_BondDoubleButton_clicked(bool on) { if (on) set_useraction(on, UA_BONDDOUBLE); }
	void on_BondTripleButton_clicked(bool on) { if (on) set_useraction(on, UA_BONDTRIPLE); }
	void on_BondDeleteButton_clicked(bool on) { if (on) set_useraction(on, UA_DELBOND); }
	void on_BondCalcButton_clicked(bool on);
	void on_BondClearButton_clicked(bool on);
	void on_BondCalcSelButton_clicked(bool on);
	void on_BondClearSelButton_clicked(bool on);
	void on_ElementHButton_clicked(bool on) { if (on) master.set_sketchelement(1); }
	void on_ElementCButton_clicked(bool on) { if (on) master.set_sketchelement(6); }
	void on_ElementNButton_clicked(bool on) { if (on) master.set_sketchelement(7); }
	void on_ElementUserButton_clicked(bool on);
	void on_ElementEdit_editingFinished();
	void on_BondAugmentButton_clicked(bool on);
	void on_AddHydrogenButton_clicked(bool on);

	// Analyse page functions
	private slots:
	void on_MeasureDistanceButton_clicked(bool on) { if (on) set_useraction(on, UA_GEOMDIST); }
	void on_MeasureAngleButton_clicked(bool on) { if (on) set_useraction(on, UA_GEOMANGLE); }
	void on_MeasureTorsionButton_clicked(bool on) { if (on) set_useraction(on, UA_GEOMTORSION); }

	// Transformation Page Functions
	private:
	void rotate_selection(double direction);
	void translate_selection(int axis, int dir);
	private slots:
	void on_RotateDefineOriginButton_clicked(bool on);
	void on_RotateDefineAxisButton_clicked(bool on);
	void on_RotateClockwiseButton_clicked(bool on) { rotate_selection(1); }
	void on_RotateAnticlockwiseButton_clicked(bool on) { rotate_selection(-1); }
	void on_TranslatePosXButton_clicked(bool on) { translate_selection(0, 1); }
	void on_TranslatePosYButton_clicked(bool on) { translate_selection(1, 1); }
	void on_TranslatePosZButton_clicked(bool on) { translate_selection(2, 1); }
	void on_TranslateNegXButton_clicked(bool on) { translate_selection(0, -1); }
	void on_TranslateNegYButton_clicked(bool on) { translate_selection(1, -1); }
	void on_TranslateNegZButton_clicked(bool on) { translate_selection(2, -1); }

	// Position Page Functions
	private:
	void flip_selection(int axis);
	private slots:
	void on_FlipXButton_clicked(bool checked) { flip_selection(0); }
	void on_FlipYButton_clicked(bool checked) { flip_selection(1); }
	void on_FlipZButton_clicked(bool checked) { flip_selection(2); }
	void on_DefineCentreButton_clicked(bool checked);
	void on_CentreSelectionButton_clicked(bool checked);

	// Cell Page Functions
	public:
	void refresh_cellpage();
	void cell_changed();
	private slots:
	void on_CellDefinitionGroup_clicked(bool checked);
	void on_CellLengthASpin_valueChanged(double d) { cell_changed(); }
	void on_CellLengthBSpin_valueChanged(double d) { cell_changed(); }
	void on_CellLengthCSpin_valueChanged(double d) { cell_changed(); }
	void on_CellAngleASpin_valueChanged(double d) { cell_changed(); }
	void on_CellAngleBSpin_valueChanged(double d) { cell_changed(); }
	void on_CellAngleCSpin_valueChanged(double d) { cell_changed(); }
	void on_CellReplicateButton_clicked(bool checked);
	void on_CellScaleButton_clicked(bool checked);

	// Minimiser Page Functions
	private slots:
	void on_MinimiserMethodCombo_currentIndexChanged(int index) { ui.MethodOptionsStack->setCurrentIndex(index); }
	void on_MinimiseButton_clicked(bool checked);

	// Forcefield Page Functions
	public:
	void refresh_forcefieldpage();
	private slots:
	void on_RemoveForcefieldButton_clicked(bool checked);
	void on_EditForcefieldButton_clicked(bool checked);
	void on_AssignFFToCurrentButton_clicked(bool checked);
	void on_AssignFFToAllButton_clicked(bool checked);
	void on_AssignFFToPatternButton_clicked(bool clicked);
	void on_TypeModelButton_clicked(bool checked);
	void on_UntypeModelButton_clicked(bool checked);
	void on_ForcefieldList_currentRowChanged(int row);

	// Surface Page Functions
	public:
	void refresh_gridspage();
	private:
	void refresh_gridinfo();
	void grid_origin_changed(int component, double value);
	void grid_axis_changed(int row, int component, double value);
	private slots:
	void on_SaveGridButton_clicked(bool checked);
	void on_GridList_currentRowChanged(int row);
	void on_SurfaceStyleCombo_currentIndexChanged(int index);
	void on_SurfaceCutoffSpin_valueChanged(double d);
	void on_GridOriginXSpin_valueChanged(double d) { grid_origin_changed(0, d); }
	void on_GridOriginYSpin_valueChanged(double d) { grid_origin_changed(1, d); }
	void on_GridOriginZSpin_valueChanged(double d) { grid_origin_changed(2, d); }
	void on_GridAxesAXSpin_valueChanged(double d) { grid_axis_changed(0,0, d); }
	void on_GridAxesAYSpin_valueChanged(double d) { grid_axis_changed(0,1, d); }
	void on_GridAxesAZSpin_valueChanged(double d) { grid_axis_changed(0,2, d); }
	void on_GridAxesBXSpin_valueChanged(double d) { grid_axis_changed(1,0, d); }
	void on_GridAxesBYSpin_valueChanged(double d) { grid_axis_changed(1,1, d); }
	void on_GridAxesBZSpin_valueChanged(double d) { grid_axis_changed(1,2, d); }
	void on_GridAxesCXSpin_valueChanged(double d) { grid_axis_changed(2,0, d); }
	void on_GridAxesCYSpin_valueChanged(double d) { grid_axis_changed(2,1, d); }
	void on_GridAxesCZSpin_valueChanged(double d) { grid_axis_changed(2,2, d); }
	void on_SurfaceColourButton_clicked(bool checked);
	void on_SurfaceTransparencySpin_valueChanged(double d);

	// Disorder Page Functions
	public:
	void refresh_disorderpage();
	private:
	void refresh_components();
	void refresh_component_data();
	void set_component_coords(int centsize, int element, double value);
	private slots:
	void on_ComponentList_itemSelectionChanged();
	void on_AddComponentButton_clicked(bool checked);
	void on_DeleteComponentButton_clicked(bool checked);
	void on_PopulationSpin_valueChanged(int value);
	void on_ComponentTranslateCheck_clicked(bool checked);
	void on_ComponentRotateCheck_clicked(bool checked);
	void on_ComponentRegionCombo_currentIndexChanged(int index);
	void on_ShowRegionsCheck_clicked(bool checked);
	void on_DisorderStartButton_clicked(bool checked);
	void on_VDWScaleSpin_valueChanged(double d);
	void on_ComponentCentreXSpin_valueChanged(double d) { set_component_coords(0,0,d); }
	void on_ComponentCentreYSpin_valueChanged(double d) { set_component_coords(0,1,d); }
	void on_ComponentCentreZSpin_valueChanged(double d) { set_component_coords(0,2,d); }
	void on_ComponentSizeXSpin_valueChanged(double d) { set_component_coords(1,0,d); }
	void on_ComponentSizeYSpin_valueChanged(double d) { set_component_coords(2,1,d); }
	void on_ComponentSizeZSpin_valueChanged(double d) { set_component_coords(3,2,d); }

	/*
	// Atom Popup Functions
	*/
	private:
	void set_atomstyle(draw_style ds);
	void set_atomlabel(atom_label al);
	void set_atomhidden(bool hidden);

	private slots:
	void on_actionAtomStyleStick_triggered(bool checked) { set_atomstyle(DS_STICK); }
	void on_actionAtomStyleTube_triggered(bool checked) { set_atomstyle(DS_TUBE); }
	void on_actionAtomStyleSphere_triggered(bool checked) { set_atomstyle(DS_SPHERE); }
	void on_actionAtomStyleScaled_triggered(bool checked) { set_atomstyle(DS_SCALED); }
	void on_actionAtomLabelID_triggered(bool checked) { set_atomlabel(AL_ID); }
	void on_actionAtomLabelCharge_triggered(bool checked) { set_atomlabel(AL_CHARGE); }
	void on_actionAtomLabelFFType_triggered(bool checked) { set_atomlabel(AL_FFTYPE); }
	void on_actionAtomLabelElement_triggered(bool checked) { set_atomlabel(AL_ELEMENT); }
	void on_actionAtomLabelFFEquiv_triggered(bool checked) { set_atomlabel(AL_FFEQUIV); }
	void on_actionAtomSetVisible_triggered(bool checked) { set_atomhidden(FALSE); }
	void on_actionAtomSetInvisible_triggered(bool checked) { set_atomhidden(TRUE); }

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
	void progress_cancel();

	public:
	// Update undo/redo labels
	void update_undoredo();

	/*
	// Local Widgets
	*/
	public:
	// Main text label for status bar in main window
	QLabel *statuslabel;
	// Statusbar progress indicator widgets
	QProgressBar *progressbar;
	QLabel *progresslabel;
	QPushButton *progressbutton;
	QFrame *progressindicator;
	// File dialogs for filter types
	QFileDialog *dialog[FT_NITEMS];
	// File dialog for save image
	QFileDialog *saveimagedialog;
	// Filter set from save model dialog
	filter *savemodelfilter;
	// Filename set from save model dialog
	dnchar savemodelfilename;
	// QButtonGroup for stackpage buttons
	QButtonGroup *uaGroup;
	// QActionGroup for SelectToolBar actions
	QActionGroup *selectGroup;
	// Dummy button for uaGroup (so we can have none selected)
	QPushButton *dummybutton;

	/*
	// Settings
	*/
	private:
	// Settings structure
	QSettings *settings;
	// Save settings
	void save_settings();

	/*
	// Recent files
	*/
	private slots:
	// Load recent file
	void load_recent();

	private:
	// Pointers to recent file actions
	QAction *actionRecentFile[MAXRECENTFILES];

	public:
	// Add file to top of recent list
	void add_recent(const char*);
};

#endif
