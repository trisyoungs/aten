/*
	*** GTK stub
	*** src/gui-gtk/gui-gtk.h
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

#ifndef H_GUIGTK_H
#define H_GUIGTK_H

#include <gtk/gtk.h>
#include <gdk/gdkpixbuf.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/canvas-gtk.h"
#include "gui-gtk/treeview.h"
#include "gui/gui.h"
#include "parse/filter.h"

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

enum xpm_icon { XPM_ADD, XPM_CAESIOUS, XPM_CELL, XPM_DS_STICK, XPM_DS_TUBE, XPM_DS_SPHERE, XPM_DS_SCALED, XPM_DS_INDIVIDUAL, XPM_EXCLAIM, XPM_PAT_CROSS, XPM_PAT_TICK, XPM_REMOVE, XPM_TOGGLELIST, XPM_TRAJ_END, XPM_TRAJ_FF, XPM_TRAJ_PLAYPAUSE, XPM_TRAJ_REWIND, XPM_TRAJ_START, XPM_TRAJ_STOP, XPM_TRANSLATE_DOWN, XPM_TRANSLATE_IN, XPM_TRANSLATE_LEFT, XPM_TRANSLATE_OUT, XPM_TRANSLATE_RIGHT, XPM_TRANSLATE_UP, XPM_BOND_AUGMENT, XPM_BOND_AUGMENTSEL, XPM_BOND_CALCULATE, XPM_BOND_CALCULATESEL, XPM_BOND_CLEAR, XPM_BOND_CLEARSEL, XPM_BOND_DELETE, XPM_BOND_MAKEDOUBLE, XPM_BOND_MAKESINGLE, XPM_BOND_MAKETRIPLE, XPM_DRAW_HADD, XPM_DRAW_ATOMCHAIN, XPM_DRAW_ATOMS, XPM_DRAW_DELATOM, XPM_DRAW_FRAGMENT, XPM_DRAW_SINGLEATOM, XPM_DRAW_TRANSMUTE, XPM_EPSR_RESTRAINT, XPM_EPSR_ROTATION, XPM_GEOMETRY_ANGLE, XPM_GEOMETRY_DISTANCE, XPM_GEOMETRY_TORSION, XPM_SELECT_ELEMENT, XPM_SELECT_EXPAND, XPM_SELECT_FRAGMENT, XPM_SELECT_INVERT, XPM_SELECT_PICK, XPM_SELECT_SPHERE, XPM_NITEMS };

// Forward declarations
class atom;
class model;
class forcefield;

// Gnome GTK-+2.0 GUI
class gui_gtk : public gui_master
{
	public:
	// Constructor / Destructor
	gui_gtk();
	~gui_gtk();

	/*
	// Basic Window Functions
	*/
	public:
	// Early doors functions
	void prepare();
	// Initialises all aspects of the GUI and hands over control
	void run(int, char**);
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
	// GUI file filters
	*/
	public:
	// Initialise GUI file filters array
	void init_filters();

	/*
	//
	// Non-virtual functions follow, for local use within gui_gtk
	//
	*/

	// Treeview Column Enums
	public:
	// Treeview Masters
	model_master modelmaster;
	pattern_master patternmaster;
	ff_master ffmaster;

	/*
	// Window Pointers
	*/
	public:
	GtkWindow *windows[GW_NITEMS];
	
	/*
	// Creation Functions
	*/
	public:
	// Create all GUI windows
	void create_interface();

	private:
	void mainwin_create();				//
	GtkMenuBar *mainmenu_create(GtkAccelGroup*);	// Main menu
	void atomselectwin_create();			//
	void positionwin_create();			//
	void positionwin_create_translate(GtkNotebook*);//
	void positionwin_create_rotate(GtkNotebook*);	//
	void positionwin_create_orient(GtkNotebook*);	//
	void energywin_create();			//
	void labelwin_create();				//
	void atombuildwin_create();			//
	void prefswin_create();				//
	void prefswin_create_general(GtkNotebook*);	//
	void prefswin_create_render(GtkNotebook*);	//
	void prefswin_create_control(GtkNotebook*);	//
	void prefswin_create_view(GtkNotebook*);	//
	void geomwin_create();				//
	void disbuildwin_create();			//
	void disbuildwin_create_components(GtkNotebook*); //
	void disbuildwin_create_method(GtkNotebook*);	//
	void ffwin_create();				//
	void patwin_create();				//
	void colourwin_create();			//
	void savemodelwin_create();			//
	void loadmodelwin_create();			//
	void loadtrajwin_create();			//
	void loadffwin_create();			//
	void saveimagewin_create();			//
	void savefieldwin_create(); 			//
	void atomwin_create();				//
	void celleditwin_create();			//
	void celleditwin_create_matrix(GtkNotebook*);	//
	void celleditwin_create_params(GtkNotebook*);	//
	void cellrepwin_create();			//
	void cellviewwin_create();			//
	void cellexpandwin_create();			//
	void avgmolwin_create();			//
	void volwin_create();				//
	void sdoptwin_create();				//
	void mcoptwin_create();				//
	void minwin_create();				//
	void ptwin_create();				//

	/*
	// File Filters
	*/
	private:
	// File dialog filters
	GtkFileFilter **filters[FT_NITEMS];
	public:
	// Initialise import filters array
	void init_modelinfilters(int, filter*);
	// Initialise export filters array
	void init_modeloutfilters(int, filter*);
	// Initialise trajectory filters array
	void init_trajectoryfilters(int, filter*);
	// Initialise field filters array
	void init_fieldoutfilters(int, filter*);

	/*
	// GUI Widgets
	*/
	public:
	GtkWidget *modelview;				// Drawing area widget
	GtkTextBuffer *statusbuf;			// The text buffer of the main window
	GtkTextTag *msgtag;				// TextTag for the textview buffer
	widgetcanvas mainview;				// Main view

	/*
	// Prefs widgets
	*/
	public:
	GtkLabel *label[W_LBL_NITEMS];			// Labels
	GtkSpinButton *spin[W_SPN_NITEMS];		// Misc spin buttons
	GtkCheckButton *check[W_CHK_NITEMS];		// Misc check buttons
	GtkToolItem *trajcontrols[5];			// Trajectory playback buttons
	GtkScale *scale[W_SCL_NITEMS];			// Misc scale adjustments
	GtkComboBox *combo[W_COM_NITEMS];		// Misc combo boxes
	GtkComboBox *MBcombos[MB_NITEMS];		// Mouse button/wheel pref combo boxes
	GtkComboBox *MKcombos[MK_NITEMS];		// Key modifier pref combo boxes
	GtkToolItem *UAtoolitem[UA_NITEMS];		// All toolbar action widgets are owned here
	GtkSpinButton *DSspins[DS_NITEMS];		// Spins for drawing style scale factors
	GtkRadioButton *EMradios[EM_NITEMS];		// Radio buttons for electrostatics model
	GtkToolItem *DSbuttons[DS_NITEMS];		// Drawing style radio toolbar buttons
	GtkMenuItem *DSmenuitems[DS_NITEMS];		// Drawing style menu radio items


	/*
	// Main window / menu
	*/
	private:
	GtkBox *mainwin_modellist_box;
	public:
	void mainwin_stylechange(GtkRadioToolButton*);
	void mainwin_check_before_close();
	void mainwin_textview_append(char *message);
	void mainwin_modellist_set_visible(bool visible)
		{ visible ? cs_show(GTK_WIDGET(mainwin_modellist_box)) : cs_hide(GTK_WIDGET(mainwin_modellist_box)); }
	void mainwin_modellist_toggle_visible()
		{ mainwin_modellist_set_visible(!GTK_WIDGET_VISIBLE((GTK_WIDGET(mainwin_modellist_box)))); }
	void mainmenu_setstylemenu(int);
	treeview_list *mainwin_modellist;

	//void create_trajectory_thread(trajectory*);

	/*
	// FF Manager
	*/
	public:
	treeview_list *ffwin_fflist;

	/*
	// Atom Window
	*/
	public:
	treeview_list atomwin_atomlist;
	void atomwin_list_update();
	void atomwin_list_repopulate();
	void atomwin_list_reset();
	void atomwin_list_refresh();
	void atomwin_list_updateselected(col_atomlist);
	void atomwin_list_update_selection();
	void atomwin_list_delete_selection();

	/*
	// Cell Edit Window
	*/
	public:
	GtkComboBox *celleditwin_typecombo;
	GtkSpinButton *celleditwin_lengths[3], *celleditwin_angles[3], *celleditwin_matrix[9];
	static bool celleditwin_UPDATING;
	void celleditwin_changed(bool);
	void celleditwin_update();

	/*
	// File Dialogs
	*/
	public:
	void call_loadmodel_dialog();
	void call_savemodel_dialog();
	void call_loadff_dialog();
	void call_savefield_dialog();
	void call_saveimage_dialog();
	void call_loadtraj_dialog();

	// Atomic popup menu
	void call_atompopup(GtkWidget*,GdkEventButton*,atom*);
	
	void mcfillwin_update();
	
	void patwin_update();
	
	void ffwin_datalists_update(forcefield*);


	key_code convert_to_KC(int);

	/*
	// Icons
	*/
	private:
	GdkPixbuf *xpms[XPM_NITEMS];
	void icons_create();
	public:
	void icons_destroy();
	GdkPixbuf *get_xpm(xpm_icon xi)	{ return xpms[xi]; }
};

extern gui_gtk gui;

#endif
