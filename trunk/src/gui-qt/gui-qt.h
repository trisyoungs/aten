/*
	*** Qt user interface stub
	*** src/gui-qt/gui-qt.h
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

#ifndef H_GUIQT_H
#define H_GUIQT_H

#include "gui/gui.h"
#include "gui-qt/canvas-qt.h"

enum widget_combo { W_COM_BONDONLOAD, W_COM_PACKONLOAD, W_COM_ZMAPTYPE, W_COM_NITEMS };
enum widget_scale { W_SCL_LABELSCALE, W_SCL_ATOMQUAL, W_SCL_BONDQUAL, W_SCL_SHINY, W_SCL_NITEMS };
enum widget_spin { W_SPN_EWALDALPHA, W_SPN_EWALDKMAX, W_SPN_EWALDPRE, W_SPN_VDWCUT, W_SPN_ELECCUT,  W_SPN_TIMERSIZE, W_SPN_TIMERPERIOD, W_SPN_TUBESIZE, W_SPN_SELSCALE, W_SPN_NITEMS };
enum widget_check { W_CHK_VDWACTIVE, W_CHK_INTRAACTIVE, W_CHK_ELECACTIVE, W_CHK_SHOWLABELS, W_CHK_SHOWGLOBE, W_CHK_SHOWCELL, W_CHK_SHOWAXES, W_CHK_SHOWGEOM, W_CHK_TIMERACTIVE, W_CHK_USEFOG, W_CHK_USELINEALIASING, W_CHK_USEPOLYALIASING, W_CHK_LOADALLFRAMES, W_CHK_FOLDONLOAD, W_CHK_CENTREONLOAD, W_CHK_PACKONLOAD, W_CHK_NITEMS };
enum widget_label { W_LBL_TRAJ, W_LBL_CELL, W_LBL_MASS, W_LBL_NATOMS, W_LBL_NITEMS };

enum col_modellist { ML_COL_CELLFLAG, ML_COL_MODFLAG, ML_COL_NAME, ML_COL_MODEL, ML_NCOLS };
enum col_complist { CL_COL_NAME, CL_COL_COMP, CL_NCOLS };
enum col_fflist { FL_COL_AUTOLOAD, FL_COL_NAME, FL_COL_FF, FL_NCOLS };
enum col_fflistnb { FLNB_COL_N, FLNB_COL_I, FLNB_COL_Q, FLNB_COL_FORM, FLNB_COL_DATA1, FLNB_COL_DATA2, FLNB_COL_DATA3, FLNB_NCOLS };
enum col_fflistb { FLB_COL_N, FLB_COL_I, FLB_COL_J, FLB_COL_FORM, FLB_COL_DATA1, FLB_COL_DATA2, FLB_NCOLS };
enum col_fflista { FLA_COL_N, FLA_COL_I, FLA_COL_J, FLA_COL_K, FLA_COL_FORM, FLA_COL_DATA1, FLA_COL_DATA2, FLA_NCOLS };
enum col_fflistt { FLT_COL_N, FLT_COL_I, FLT_COL_J, FLT_COL_K, FLT_COL_L, FLT_COL_FORM, FLT_COL_DATA1, FLT_COL_DATA2, FLT_COL_DATA3, FLT_COL_DATA4, FLT_COL_ESCALE, FLT_COL_VSCALE, FLT_NCOLS };
enum col_collist { COL_COL_NAME, COL_COL_ID, COL_NCOLS };
enum col_atomlist {AL_COL_ID,AL_COL_ELEMENT,AL_COL_POSX,AL_COL_POSY,AL_COL_POSZ,AL_COL_FF,AL_COL_DS,AL_COL_HIDE,AL_COL_NBONDS,AL_COL_Q,AL_COL_ATOM,AL_NCOLS};
enum col_patternlist { PL_COL_NAME, PL_COL_PATTERN, PL_NCOLS };

// Forward declarations
class AtenForm;
class AtenPrefs;
class atom;
class model;
class forcefield;
class QApplication;
class QProgressDialog;

// QT4 GUI
class gui_qt : public gui_master
{
	public:
	// Constructor / Destructor
	gui_qt();
	~gui_qt();

	/*
	// Basic Window Functions
	*/
	public:
	// Early doors functions
	virtual void prepare();
	// Initialises all aspects of the GUI and hands over control
	virtual void run(int, char**);
	// Show specified window
	void show(gui_window);
	// Set values of widgets from current prefs data
	void set_controls();
	// Question user dialog
	int user_question(const char*, const char*);

	/*
	// Specific Window Functions
	*/
	public:
	// Add a message to the main window's message output box
	void print_message(const char*);
	// Refresh main canvas
	void refresh();
	// Change the font used in the message box
	void change_msg_font(const char*);
	// Update trajectory control widgets
	void update_trajcontrols();
	// Update main window labels
	void update_labels();
	// Process events from GUI
	void process_events();
	// Terminate GUI
	void close_application();

	/*
	// Object management
	*/
	public:
	// Add model (adds new model to list)
	void add_model(model*);
	// Remove model from list
	void remove_model(model*);
	// Select model (show in main/sub windows)
	void select_model(model*);
	// Add forcefield (adds ff to list)
	void add_ff(forcefield*);
	// Remove ff from list
	void remove_ff(forcefield*);
	// Select forcefield in list
	void select_ff(forcefield*);

	/*
	//
	// Non-virtual functions follow, for local use within gui-qt
	//
	*/

	key_code convert_to_KC(int);

	/*
	// Windows / Dialogs
	*/
	private:
	// Main Qt widget for the interface
	AtenForm *mainwindow;
	// Main application structure
	QApplication *app;

	public:
	// Preferences Dialog
	AtenPrefs *prefsdialog;
	// Call the atompopup menu
	void call_atompopup(atom*, int, int);
	// Main view
	widgetcanvas mainview;

	/*
	// Progress Dialog
	*/
	private:
	// Progress dialog widget
	QProgressDialog *progress;

	public:
	// Instantiate a progress dialog
	virtual void progress_create(const char *jobtitle, int stepstodo);
	// Update the progress dialog
	virtual bool progress_update(int currentstep);
	// Terminate the progress dialog
	virtual void progress_terminate();
};

extern gui_qt gui;

#endif
