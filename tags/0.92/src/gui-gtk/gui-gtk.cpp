/*
	*** GTK stub functions
	*** src/gui-gtk/gui-gtk.cpp
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

#include <gdk/gdkkeysyms.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"
#include "gui-gtk/gui-gtk.h"
#include "gui-gtk/16x16.xpms"
#include "gui-gtk/atompos.xpms"
#include "gui-gtk/build_bond.xpms"
#include "gui-gtk/build_sketch.xpms"
#include "gui-gtk/epsr.xpms"
#include "gui-gtk/geometry.xpms"
#include "gui-gtk/select.xpms"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

// External Declarations
gui_gtk gui;

// Static Variables
bool gui_gtk::celleditwin_UPDATING;

// Constructor
gui_gtk::gui_gtk()
{
	#ifdef MEMDEBUG
		printf("Constructor : gui_gtk\n");
	#endif
	for (int i=0; i<GW_NITEMS; i++) windows[i] = NULL;
	celleditwin_UPDATING = FALSE;
	// Filters
	for (int n=0; n < FT_NITEMS; n ++) filters[n] = NULL;
}

// Destructor
gui_gtk::~gui_gtk()
{
	#ifdef MEMDEBUG
		printf("Destructor : gui_gtk\n");
	#endif
}

// Early initial functions
void gui_gtk::prepare()
{
	g_type_init();
}

// Initialise and create GUI
void gui_gtk::run(int argc, char **argv)
{
	dbg_begin(DM_CALLS,"gui_gtk::run");
	// Setup GTK and create GUI
	g_thread_init(NULL);
	gdk_threads_init();
	gtk_init(&argc, &argv);
	create_interface();
	does_exist = TRUE;

	// Setup GL and create globs
	gtk_gl_init(&argc, &argv);
	glutInit(&argc, argv);

	// Set GUI controls linked to preferences values
	set_controls();

	// Refresh GUI lists
	modelmaster.refresh();		// Populate the model list with any loaded models
	ffmaster.refresh();		// Add pre-loaded forcefields into the forcefield list

	// If no or one model loaded, hide the model panel since we don't need it right now
	mainwin_modellist_set_visible( master.get_nmodels() < 2 ? FALSE : TRUE);

	// Do projection of main canvas and (re)set currentmodel to start of model list
	mainview.do_projection();
	mainwin_modellist->select(ML_COL_MODEL,master.get_models());
	master.set_currentmodel(master.get_models());
	master.get_currentmodel()->calculate_viewmatrix();
	master.get_currentmodel()->project_all();

	// Start timer
	if (master.use_timer) master.start_timer_events();

	// Hand over to GTK
	gdk_threads_enter();
	gtk_main();
	gdk_threads_leave();

	// Free icon list
	icons_destroy();
	dbg_end(DM_CALLS,"gui_gtk::run");
}

// Create all GUI elements
void gui_gtk::create_interface()
{
	dbg_begin(DM_CALLS,"gui_gtk::create_interface");
	// Prepare master treeviews
	GtkCellRenderer *rend;
	modelmaster.initialise("modelmaster","xxsp");
	rend = cs_column(&modelmaster,"C","pixbuf",ML_COL_CELLFLAG);
	rend = cs_column(&modelmaster,"M","pixbuf",ML_COL_MODFLAG);
	rend = cs_column(&modelmaster,"Model","text",ML_COL_NAME);
	modelmaster.set_pointer_column(ML_COL_MODEL);
	patternmaster.initialise("patternmaster","sp");
	rend = cs_column(&patternmaster,"Pattern","pixbuf",PL_COL_NAME);
	patternmaster.set_pointer_column(PL_COL_PATTERN);
	ffmaster.initialise("ffmaster","xsp");
	rend = cs_column(&ffmaster,"A","pixbuf",FL_COL_AUTOLOAD);
	rend = cs_column(&ffmaster,"Name","text",FL_COL_NAME);
	ffmaster.set_pointer_column(FL_COL_FF);

	// Create icons
	icons_create();

	// Main windows
	mainwin_create();
	colourwin_create();
	atomwin_create();
	ptwin_create();
	minwin_create();
	mcoptwin_create();
	sdoptwin_create();
	atomselectwin_create();
	positionwin_create();
	celleditwin_create();
	cellrepwin_create();
	cellviewwin_create();
	cellexpandwin_create();

	// File dialogs
	loadmodelwin_create();
	savemodelwin_create();
	loadtrajwin_create();
	loadffwin_create();
	savefieldwin_create();
	//saveimagewin_create();

	// Preferences window
	prefswin_create();

	// Build window
	atombuildwin_create();
	geomwin_create();
	labelwin_create();
	ffwin_create();
	disbuildwin_create();
	energywin_create();

	dbg_end(DM_CALLS,"gui_gtk::create_interface");
}

// Set GUI to reflect prefs
void gui_gtk::set_controls()
{
	dbg_begin(DM_CALLS,"gui_master::set_controls");
	int n;
	// In Prefs.Control Page
	// -- Mouse Binding Combobox Widgets
	for (n=0; n<MB_NITEMS; n++) gtk_combo_box_set_active(MBcombos[n],prefs.get_mb_action((mouse_button) n));
	// In Prefs.Rendering Page
	gtk_range_set_value(GTK_RANGE(scale[W_SCL_ATOMQUAL]),prefs.get_atom_detail());
	gtk_range_set_value(GTK_RANGE(scale[W_SCL_BONDQUAL]),prefs.get_bond_detail());
	gtk_range_set_value(GTK_RANGE(scale[W_SCL_SHINY]),prefs.get_shininess());
	// In Prefs.View Page
	for (n=0; n<DS_INDIVIDUAL; n++)
		gtk_spin_button_set_value(DSspins[n],prefs.get_atom_size((draw_style) n));
	
	gtk_spin_button_set_value(spin[W_SPN_TUBESIZE],prefs.get_tube_size());
	gtk_spin_button_set_value(spin[W_SPN_SELSCALE],prefs.get_selection_scale());
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check[W_CHK_USEFOG]),prefs.get_gl_option(GO_FOG));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check[W_CHK_USELINEALIASING]),prefs.get_gl_option(GO_LINEALIASING));
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check[W_CHK_USEPOLYALIASING]),prefs.get_gl_option(GO_POLYALIASING));
	// In Measurements window
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check[W_CHK_SHOWGEOM]),prefs.should_render(VO_MEASUREMENTS));
	// In Labelling window
	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(check[W_CHK_SHOWGEOM]),prefs.should_render(VO_LABELS));
	// In Energy window
	gtk_spin_button_set_range(spin[W_SPN_VDWCUT],0.0,prefs.get_vdw_cutoff());
	gtk_spin_button_set_range(spin[W_SPN_ELECCUT],0.0,prefs.get_elec_cutoff());
	// Set the widget values to those of the model's
	gtk_spin_button_set_value(spin[W_SPN_VDWCUT],prefs.get_vdw_cutoff());
	gtk_spin_button_set_value(spin[W_SPN_ELECCUT],prefs.get_elec_cutoff());
	// Set the Ewald sum values
	// TODO kvec for each direction!
	gtk_spin_button_set_value(spin[W_SPN_EWALDKMAX],prefs.get_ewald_kvec().x);
	gtk_spin_button_set_value(spin[W_SPN_EWALDALPHA],prefs.get_ewald_alpha());
	gtk_spin_button_set_value(spin[W_SPN_EWALDPRE],prefs.get_ewald_precision());

	dbg_end(DM_CALLS,"gui_master::set_controls");
}

// Show window
void gui_gtk::show(gui_window gw)
{
	cs_show(windows[gw]);
}

// Update trajectory controls
void gui_gtk::update_trajcontrols()
{
	// (De)Sensitize trajectory control widgets
	model *m = master.get_currentmodel();
	if (m->get_totalframes() != 0)
	{
		// Desensitise all but play/pause if the trajectory is currently playing...
		//if (m->get_playing()) for (int n=0; n<4; n++) gtk_widget_set_sensitive(GTK_WIDGET(trajcontrols[n]),FALSE);
		//else for (int n=0; n<4; n++) gtk_widget_set_sensitive(GTK_WIDGET(trajcontrols[n]),TRUE);
		// Play/pause is always active...
		//gtk_widget_set_sensitive(GTK_WIDGET(trajcontrols[4]),TRUE);
	}
	else for (int n=0; n<5; n++) gtk_widget_set_sensitive(GTK_WIDGET(trajcontrols[n]),FALSE);
}

// Change msgbox font
void gui_gtk::change_msg_font(const char *font)
{
	// Remove the old font tag from the text buffer's tag table
	GtkTextTagTable* table = gtk_text_buffer_get_tag_table(statusbuf);
	gtk_text_tag_table_remove(table,msgtag);
	// Now create a new tag based on the info from the font button
	msgtag = gtk_text_buffer_create_tag(statusbuf,"font","font",font,NULL);
}

// Update labels
void gui_gtk::update_labels()
{
	// Update the information labels in the button bar
	if (!does_exist) return;
	dbg_begin(DM_CALLS,"gui_gtk::update_labels");
	string s;
	model *m = master.get_currentmodel();
	// Trajectory information label
	s = "(";
	s += itoa(m->get_frameposition());
	s += " / ";
	s += itoa(m->get_totalframes());
	s += ")";
	gtk_label_set_text(label[W_LBL_TRAJ],s.c_str());
	// Model information
	s = itoa(m->get_natoms());
	if (m->get_nselected() != 0)
	{
		s += " (<b>";
		s += itoa(m->get_nselected());
		s += "</b>)";
	}
	s += " Atoms, ";
	cs_set_label_pango(s.c_str(),label[W_LBL_NATOMS]);
	s = ftoa(m->get_mass());
	s += " g ";
	gtk_label_set_text(label[W_LBL_MASS],s.c_str());
	cell_type ct = m->cell.get_type();
	if (ct != CT_NONE)
	{
		s = "(";
		s += text_from_CT(ct);
		s += ", ";
		s += ftoa(m->get_density());
		switch (prefs.get_density_units())
		{
			case (DU_GPERCM):	s += " g cm<sup>-3</sup>)"; break;
			case (DU_ATOMSPERANG):	s += " atoms &#8491;<sup>-3</sup>)"; break;
		}
		cs_set_label_pango(s.c_str(),label[W_LBL_CELL]);
	}
	else gtk_label_set_text(label[W_LBL_CELL],"");
//	m->ff == NULL ? gtk_label_set_text(GTK_LABEL(lbl_ff),"None")
//			  : gtk_label_set_text(GTK_LABEL(lbl_ff),m->ff->name.c_str());
	dbg_end(DM_CALLS,"gui_gtk::update_labels");
}

/*
// General GUI Routines
*/

void gui_gtk::process_events()
{
	// During intensive computations, this should be called periodically so that the interface remains responsive, and the process can be interrupted, modelview can be manipulated etc.
	do
	{
		if (gtk_events_pending()) gtk_main_iteration();
	}
	while (mainview.is_drawing());
}

// Add model to list
void gui_gtk::add_model(model *m)
{
	if (!does_exist) return;
	modelmaster.add_item(m);
}

// Remove model from list
void gui_gtk::remove_model(model *m)
{
	if (!does_exist) return;
	// Find iter that matches the old model in the model master
	GtkTreeIter iter;
	if (modelmaster.find_iter(&iter,ML_COL_MODEL,m))
	{
		//printf("Removing old model from list...\n");
		modelmaster.remove(&iter);
		//printf("Done.\n");
	}
}

// Select model in list and show in main/sub windows
void gui_gtk::select_model(model *m)
{
	// Select the new model in the mainview model list
	if (!does_exist) return;
	// Model list
	modelmaster.update();  // TODO Replace with simple iter select?
	// Master pattern list
	patternmaster.refresh();
	// Labels in statusbar in main window
	update_labels();
	// MC Fill window reflects current model data
	// TODO2 gui.disbuildwin_update();
	// Atomwin reflects current model data, so refresh
	atomwin_list_refresh();
	// Show unit cell data in cell window
	celleditwin_update();
	// (De)Activate trajectory controls depending on whether one is associated
	update_trajcontrols();
	//mainview.render();
	mainwin_modellist->select(ML_COL_MODEL,m);
}

// Add ff to list
void gui_gtk::add_ff(forcefield *ff)
{
	if (!does_exist) return;
	ffmaster.add_item(ff);
}

// Remove ff from list
void gui_gtk::remove_ff(forcefield *ff)
{
	// Find iter that matches the old model in the model master
	if (!does_exist) return;
	GtkTreeIter iter;
	if (ffmaster.find_iter(&iter,FL_COL_FF,ff))
	{
		//printf("Removing old ff from list...\n");
		ffmaster.remove(&iter);
		//printf("Done.\n");
	}
}

// Select ff in list
void gui_gtk::select_ff(forcefield *ff)
{
	if (!does_exist) return;
	ffwin_fflist->select(FL_COL_FF,ff);
}

// Create gui file filters (globs) from filter list
void gui_gtk::init_filters()
{
	int count, n;
	filter *f; 
	for (n = 0; n < FT_NITEMS; n++)
	{
		count = 0;
		filters[n] = new GtkFileFilter*[master.filters[n].size()];
		for (f = master.filters[n].first(); f != NULL; f = f->next)
		{
			filters[n][count] = cs_filefilter(f->get_glob(),f->get_description());
			g_object_ref(filters[n][count]);
			count ++;
		}
	}
}

// Convert GTK keysym to key_code
key_code gui_gtk::convert_to_KC(int sym)
{
	key_code result = KC_OTHER;
	switch (sym)
	{
		case (GDK_Left):
			result = KC_LEFT;
			break;
		case (GDK_Right):
			result = KC_RIGHT;
			break;
		case (GDK_Up):
			result = KC_UP;
			break;
		case (GDK_Down):
			result = KC_DOWN;
			break;
		case (GDK_Shift_L):
			result = KC_SHIFT_L;
			break;
		case (GDK_Shift_R):
			result = KC_SHIFT_R;
			break;
		case (GDK_Control_L):
			result = KC_CONTROL_L;
			break;
		case (GDK_Control_R):
			result = KC_CONTROL_R;
			break;
		case (GDK_Alt_L):
			result = KC_ALT_L;
			break;
		case (GDK_Alt_R):
			result = KC_ALT_R;
			break;
	}
	return result;
}

// Redraw main window canvas
void gui_gtk::refresh()
{
	if (does_exist) mainview.postredisplay();
}

void gui_gtk::icons_create()
{
	xpms[XPM_ADD] = gdk_pixbuf_new_from_xpm_data(icon_add);
	xpms[XPM_CAESIOUS] = gdk_pixbuf_new_from_xpm_data(icon_caesious);
	xpms[XPM_CELL] = gdk_pixbuf_new_from_xpm_data(icon_cell);
	xpms[XPM_DS_INDIVIDUAL] = gdk_pixbuf_new_from_xpm_data(icon_ds_individual);
	xpms[XPM_DS_SCALED] = gdk_pixbuf_new_from_xpm_data(icon_ds_scaled);
	xpms[XPM_DS_SPHERE] = gdk_pixbuf_new_from_xpm_data(icon_ds_sphere);
	xpms[XPM_DS_STICK] = gdk_pixbuf_new_from_xpm_data(icon_ds_stick);
	xpms[XPM_DS_TUBE] = gdk_pixbuf_new_from_xpm_data(icon_ds_tube);
	xpms[XPM_EXCLAIM] = gdk_pixbuf_new_from_xpm_data(icon_exclaim);
	xpms[XPM_PAT_CROSS] = gdk_pixbuf_new_from_xpm_data(icon_pat_cross);
	xpms[XPM_PAT_TICK] = gdk_pixbuf_new_from_xpm_data(icon_pat_tick);
	xpms[XPM_REMOVE] = gdk_pixbuf_new_from_xpm_data(icon_remove);
	xpms[XPM_TOGGLELIST] = gdk_pixbuf_new_from_xpm_data(icon_togglelist);
	xpms[XPM_TRAJ_END] = gdk_pixbuf_new_from_xpm_data(icon_traj_end);
	xpms[XPM_TRAJ_FF] = gdk_pixbuf_new_from_xpm_data(icon_traj_ff);
	xpms[XPM_TRAJ_PLAYPAUSE] = gdk_pixbuf_new_from_xpm_data(icon_traj_playpause);
	xpms[XPM_TRAJ_REWIND] = gdk_pixbuf_new_from_xpm_data(icon_traj_rewind);
	xpms[XPM_TRAJ_START] = gdk_pixbuf_new_from_xpm_data(icon_traj_start);
	xpms[XPM_TRAJ_STOP] = gdk_pixbuf_new_from_xpm_data(icon_traj_stop);
	xpms[XPM_TRANSLATE_DOWN] = gdk_pixbuf_new_from_xpm_data(icon_translate_down);
	xpms[XPM_TRANSLATE_IN] = gdk_pixbuf_new_from_xpm_data(icon_translate_in);
	xpms[XPM_TRANSLATE_LEFT] = gdk_pixbuf_new_from_xpm_data(icon_translate_left);
	xpms[XPM_TRANSLATE_OUT] = gdk_pixbuf_new_from_xpm_data(icon_translate_out);
	xpms[XPM_TRANSLATE_RIGHT] = gdk_pixbuf_new_from_xpm_data(icon_translate_right);
	xpms[XPM_TRANSLATE_UP] = gdk_pixbuf_new_from_xpm_data(icon_translate_up);
	xpms[XPM_BOND_AUGMENT] = gdk_pixbuf_new_from_xpm_data(icon_bond_augment);
	xpms[XPM_BOND_AUGMENTSEL] = gdk_pixbuf_new_from_xpm_data(icon_bond_augmentsel);
	xpms[XPM_BOND_CALCULATE] = gdk_pixbuf_new_from_xpm_data(icon_bond_calculate);
	xpms[XPM_BOND_CALCULATESEL] = gdk_pixbuf_new_from_xpm_data(icon_bond_calculatesel);
	xpms[XPM_BOND_CLEAR] = gdk_pixbuf_new_from_xpm_data(icon_bond_clear);
	xpms[XPM_BOND_CLEARSEL] = gdk_pixbuf_new_from_xpm_data(icon_bond_clearsel);
	xpms[XPM_BOND_DELETE] = gdk_pixbuf_new_from_xpm_data(icon_bond_delete);
	xpms[XPM_BOND_MAKEDOUBLE] = gdk_pixbuf_new_from_xpm_data(icon_bond_makedouble);
	xpms[XPM_BOND_MAKESINGLE] = gdk_pixbuf_new_from_xpm_data(icon_bond_makesingle);
	xpms[XPM_BOND_MAKETRIPLE] = gdk_pixbuf_new_from_xpm_data(icon_bond_maketriple);
	xpms[XPM_DRAW_HADD] = gdk_pixbuf_new_from_xpm_data(icon_draw_hadd);
	xpms[XPM_DRAW_ATOMCHAIN] = gdk_pixbuf_new_from_xpm_data(icon_draw_atomchain);
	xpms[XPM_DRAW_ATOMS] = gdk_pixbuf_new_from_xpm_data(icon_draw_atoms);
	xpms[XPM_DRAW_DELATOM] = gdk_pixbuf_new_from_xpm_data(icon_draw_delatom);
	xpms[XPM_DRAW_FRAGMENT] = gdk_pixbuf_new_from_xpm_data(icon_draw_fragment);
	xpms[XPM_DRAW_SINGLEATOM] = gdk_pixbuf_new_from_xpm_data(icon_draw_singleatom);
	xpms[XPM_DRAW_TRANSMUTE] = gdk_pixbuf_new_from_xpm_data(icon_draw_transmute);
	xpms[XPM_EPSR_RESTRAINT] = gdk_pixbuf_new_from_xpm_data(icon_epsr_restraint);
	xpms[XPM_EPSR_ROTATION] = gdk_pixbuf_new_from_xpm_data(icon_epsr_rotation);
	xpms[XPM_GEOMETRY_ANGLE] = gdk_pixbuf_new_from_xpm_data(icon_geometry_angle);
	xpms[XPM_GEOMETRY_DISTANCE] = gdk_pixbuf_new_from_xpm_data(icon_geometry_distance);
	xpms[XPM_GEOMETRY_TORSION] = gdk_pixbuf_new_from_xpm_data(icon_geometry_torsion);
	xpms[XPM_SELECT_ELEMENT] = gdk_pixbuf_new_from_xpm_data(icon_select_element);
	xpms[XPM_SELECT_EXPAND] = gdk_pixbuf_new_from_xpm_data(icon_select_expand);
	xpms[XPM_SELECT_FRAGMENT] = gdk_pixbuf_new_from_xpm_data(icon_select_fragment);
	xpms[XPM_SELECT_INVERT] = gdk_pixbuf_new_from_xpm_data(icon_select_invert);
	xpms[XPM_SELECT_PICK] = gdk_pixbuf_new_from_xpm_data(icon_select_pick);
	xpms[XPM_SELECT_SPHERE] = gdk_pixbuf_new_from_xpm_data(icon_select_sphere);
}

void gui_gtk::icons_destroy()
{
	// Free the pixbuf memory
	for (int n=0; n<XPM_NITEMS; n++) g_object_unref(xpms[n]);
}
