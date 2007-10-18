/*
	*** GTK main menu
	*** src/gui-gtk/menu_main.cpp

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

#include "gui-gtk/funcs.h"
#include "gui-gtk/treeview.h"
#include "gui-gtk/gui-gtk.h"
#include <gtk/gtk.h>
#include <gtk/gtkgl.h>
#include <gdk/gdkkeysyms.h>
#include <GL/gl.h>
#include "model/model.h"
#include "classes/atom.h"
#include "classes/bond.h"
#include "base/master.h"
#include "base/prefs.h"
#include "base/debug.h"
#include "file/filter.h"

// Variables
GtkMenuItem *menu_colscheme[AC_NITEMS];

/*
// Menu Actions - 'File' subshell
*/

void mainmenu_load(GtkMenuItem *widget, gpointer data)
{
	gui.call_loadmodel_dialog();
}

void mainmenu_save(GtkMenuItem *widget, gpointer data)
{
	// Saves the model in the format and filename stored in the model structure
	// Can't save if we don't know what the format is, so call the save file selector instead
	filter *mf = master.get_currentmodel()->get_filter();
	// Check for no associated filter...
	if (mf == NULL) gui.call_savemodel_dialog();
	else
	{
		if (mf->get_type() == FT_MODEL_EXPORT) mf->export_model(master.get_currentmodel());
		else gui.call_savemodel_dialog();
	}
}

void mainmenu_saveas(GtkMenuItem *widget, gpointer data)
{
	gui.call_savemodel_dialog();
}

void mainmenu_quit(GtkMenuItem *widget, gpointer data)
{
	gui.close_application();
}

void mainmenu_loadtraj(GtkMenuItem *widget, gpointer data)
{
	gui.call_loadtraj_dialog();
}

void mainmenu_savefield(GtkMenuItem *widget, gpointer data)
{
	gui.call_savefield_dialog();
}

void mainmenu_saveimage(GtkMenuItem *widget, gpointer data)
{
	// Save a snapshot of the current view
	dbg_begin(DM_CALLS,"mainmenu_saveimage");
	// Temporarily unset the current model from the mainview canvas
	//master.mainview.unset_datamodel();
	pixmapcanvas test;
	printf("111\n");
	if (test.set_pixmap(1600,1200,24))
	{
		test.do_projection();
		printf("444\n");
		test.render_scene(master.get_currentmodel());
		printf("555\n");
		test.save("snapshot.bmp");
	}
	printf("666\n");
	//test.unset_datamodel();
	//master.mainview.set_datamodel(master.get_currentmodel());
	dbg_end(DM_CALLS,"mainmenu_saveimage");
}

/*
// Menu Actions - 'Edit' subshell
*/

void mainmenu_edit_cut(GtkMenuItem *widget, gpointer data)
{
	// Cut the selected atoms from the model, copying to the paste buffer
	gui.atomwin_list_delete_selection();
	master.userclip.cut_selection(master.get_currentmodel());
	gui.update_labels();
	gui.atomwin_list_refresh();
	gui.refresh();
}

void mainmenu_edit_copy(GtkMenuItem *widget, gpointer data)
{
	// Copy the selected atoms in the model into the paste buffer
	master.userclip.copy_selection(master.get_currentmodel());
}

void mainmenu_edit_paste(GtkMenuItem *widget, gpointer data)
{
	// Paste the buffered atoms into the model
	master.userclip.paste_to_model(master.get_currentmodel());
	gui.update_labels();
	gui.atomwin_list_refresh();
	gui.refresh();
}

void mainmenu_edit_delete(GtkMenuItem *widget, gpointer data)
{
	// Delete the selected atoms in the model
	dbg_begin(DM_CALLS,"mainmenu_edit_delete");
	gui.atomwin_list_delete_selection();
	master.get_currentmodel()->selection_delete();
	// Clear the main canvas' selection array to be on the safe side, since we might have deleted an atom in it!
	gui.mainview.clear_subsel();
	gui.update_labels();
	gui.atomwin_list_refresh();
	gui.refresh();
	dbg_end(DM_CALLS,"mainmenu_edit_delete");
}

void mainmenu_invert_selection(GtkMenuItem *widget, gpointer data)
{
	// Invert the selection of the current model
	dbg_begin(DM_CALLS,"mainmenu_invert_selection");
	master.get_currentmodel()->selection_invert();
	gui.update_labels();
	gui.atomwin_list_update_selection();
	gui.refresh();
	dbg_end(DM_CALLS,"mainmenu_invert_selection");
}

void mainmenu_select_all(GtkMenuItem *widget, gpointer data)
{
	// Select all atoms in the current model
	dbg_begin(DM_CALLS,"mainmenu_select_all");
	master.get_currentmodel()->select_all();
	gui.update_labels();
	gui.atomwin_list_update_selection();
	gui.refresh();
	dbg_end(DM_CALLS,"mainmenu_select_all");
}

void mainmenu_deselect_all(GtkMenuItem *widget, gpointer data)
{
	// Deselect all atoms in the current model
	dbg_begin(DM_CALLS,"mainmenu_deselect_all");
	master.get_currentmodel()->select_none();
	gui.update_labels();
	gui.atomwin_list_update_selection();
	gui.refresh();
	dbg_end(DM_CALLS,"mainmenu_deselect_all");
}

/*
// Menu Actions - 'View' subshell
*/

void mainmenu_change_view(GtkMenuItem *widget, gpointer data)
{
	dnchar action;
	action.set((char*)data);
	model *m = master.get_currentmodel();
	if (action == "reset") m->reset_view();
	else if (action == "zoomin") m->adjust_camera(0.0,0.0,5.0,0.0);
	else if (action == "zoomout") m->adjust_camera(0.0,0.0,-5.0,0.0);
	else if (action == "perspective")
	{
		prefs.set_perspective(TRUE);
		gui.mainview.do_projection();
		m->reset_view();
	}
	else if (action == "orthographic")
	{
		prefs.set_perspective(FALSE);
		gui.mainview.do_projection();
		m->reset_view();
	}
	gui.refresh();
}

void mainmenu_stylechange(GtkMenuItem *widget, gpointer data)
{
	// The drawing style of the current model has been changed from the menu
	dbg_begin(DM_CALLS,"mainmenu_stylechange");
	int n;
	for (n=0; n<DS_NITEMS; n++) if (widget == gui.DSmenuitems[n]) break;
	// Call the mainwin routine to set the style
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(gui.DSbuttons[n]),TRUE);
	dbg_end(DM_CALLS,"mainmenu_stylechange");
}

void mainmenu_colourchange(GtkMenuItem *widget, gpointer data)
{
	// The atom colouring style of the current model has been changed from the menu
	dbg_begin(DM_CALLS,"mainmenu_colourchange");
	int n;
	for (n=0; n<AC_NITEMS; n++)
		if (widget == menu_colscheme[n]) master.set_colour_scheme( (atom_colour) n);
	// Call routine to colour atoms here...
	master.get_currentmodel()->set_atom_colours(NULL);
	gui.refresh();
	dbg_end(DM_CALLS,"mainmenu_colourchange");
}

void mainmenu_forcearrowchange(GtkMenuItem *widget, gpointer data)
{
	printf("gui::toggle_forcearrows_visible() not done.\n");
	//prefs.toggle_forcearrows_visible();
	master.get_currentmodel()->log_change(LOG_VISUAL);
}

void test_function(GtkMenuItem *widget, gpointer data)
{
	// Test functions go in here!
	int i = gui.user_question("Does this work?","'No, not at all' 'Yes, sort of'");
}

void mainmenu_showlabelwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_LABELS);
}

void mainmenu_showcellviewwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_CELLVIEW);
}

/*
// Menu Actions - 'Model' Subshell
*/

void mainmenu_click_model(GtkMenuItem *widget, gpointer data)
{
	dnchar action;
	action.set((char*) data);
	model *newm, *m = master.get_currentmodel();
	if (action == "togglewin") gui.mainwin_modellist_toggle_visible();
	else if (action == "next")
	{
		if (m->next != NULL) gui.mainwin_modellist->select(ML_COL_MODEL,m->next);
		else msg(DM_NONE,"Already at last model.\n");
	}
	else if (action == "prev")
	{
		if (m->prev != NULL) gui.mainwin_modellist->select(ML_COL_MODEL,m->prev);
		else msg(DM_NONE,"Already at first model.\n");
	}
	gui.refresh();
}

/*
// Menu Actions - 'Forcefield' Subshell
*/

void mainmenu_click_ff(GtkMenuItem *widget, gpointer data)
{
	dnchar action;
	action.set((char*) data);
	model *m = master.get_currentmodel();
	if (action == "type") m->type_all();
	else if (action == "untype") m->remove_typing();
	else if (action == "showffwin") gui.show(GW_FORCEFIELD);
	gui.refresh();
}

void mainmenu_showatselwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_ATOMSELECT);
}

/*
// 'Cell' menu
*/

void mainmenu_foldmolecules(GtkMenuItem *widget, gpointer data)
{
	master.get_currentmodel()->fold_all_molecules();
}

void mainmenu_foldatoms(GtkMenuItem *widget, gpointer data)
{
	master.get_currentmodel()->fold_all_atoms();
}

void mainmenu_showcelleditwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_CELLEDIT);
}

void mainmenu_showcellrepwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_CELLREPEAT);
}

void mainmenu_expandshrink(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_CELLEXPAND);
}

/*
// Menu Actions - 'Build' subshell
*/

void mainmenu_showatombuildwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_ATOMBUILDER);
}

void mainmenu_showmcinsertwinshow(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_MCINSERT);
	prefs.set_visible(VO_REGIONS, TRUE);
}

void mainmenu_showatomposwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_POSITION);
}

void mainmenu_showatomlistwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_ATOMLIST);
	gui.atomwin_list_refresh();
}

/*
// Menu Actions - 'Analyse' Subshell
*/

void mainmenu_showavgmolwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_AVERAGEMOL);
}

void mainmenu_showvolwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_FREEVOLUME);
}

/*
// Menu Actions - 'Options' Subshell
*/

void mainmenu_showprefswin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_PREFS);
}

void mainmenu_showcolourwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_COLOUR);
}

void mainmenu_debugchange(GtkCheckMenuItem *widget, gpointer data)
{
	gtk_check_menu_item_get_active(widget) ? add_debuglevel(DM_CALLS) : remove_debuglevel(DM_CALLS);
}

/*
// Menu Actions - 'Tools' Subshell
*/

void mainmenu_atominfo(GtkMenuItem *widget, gpointer data)
{
	gui.mainview.set_selectedmode(UA_PROBEATOM);
}

void mainmenu_showenergywin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_ENERGYSETUP);
}

void mainmenu_showminimiserwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_MINIMISER);
}

void mainmenu_showgeometrywin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_MEASURE);
}

void mainmenu_showptwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_PERIODICTABLE);
}

void mainmenu_showepsrwin(GtkMenuItem *widget, gpointer data)
{
	gui.show(GW_EPSRSETUP);
}

/*
// GUI Creation
*/

GtkMenuBar *gui_gtk::mainmenu_create(GtkAccelGroup *accel)
{
	// Creates the menubar for the modelview window
	dbg_begin(DM_CALLS,"gui_gtk::create_menu");
	GtkMenuItem *m, *m2;
	GtkMenu *shell, *subshell;
	GtkMenuBar *menubar;
	int i;

	menubar = GTK_MENU_BAR(gtk_menu_bar_new());

	// "File" menu shell
	shell = cs_menushell("_File",menubar);
	m = cs_menuitem("_Open",shell,accel,GDK_O,GDK_CONTROL_MASK,mainmenu_load,NULL);
	if (master.filters[FT_MODEL_IMPORT].size() == 0) gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	m = cs_menuitem("_Save",shell,accel,GDK_S,GDK_CONTROL_MASK,mainmenu_save,NULL);
	if (master.filters[FT_MODEL_EXPORT].size() == 0) gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	m = cs_menuitem("Save _As",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_saveas,NULL);
	if (master.filters[FT_MODEL_EXPORT].size() == 0) gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	cs_menuseparator(shell);
	m = cs_menuitem("Open _Trajectory",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_loadtraj,NULL);
	if (master.filters[FT_TRAJECTORY_IMPORT].size() == 0) gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	cs_menuseparator(shell);
	m = cs_menuitem("_Export",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	m = cs_menuitem("Forcefield Spec",subshell,NULL,0,GDK_CONTROL_MASK,mainmenu_savefield,NULL);
	if (master.filters[FT_FIELD_EXPORT].size() == 0) gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	cs_menuseparator(shell);
	m = cs_menuitem("Save _Image",shell,accel,GDK_Print,GDK_CONTROL_MASK,mainmenu_saveimage,NULL);
	cs_menuseparator(shell);
	gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	m = cs_menuitem("_Quit",shell,accel,GDK_Q,GDK_CONTROL_MASK,mainmenu_quit,NULL);

	// "Edit" menu shell
	shell = cs_menushell("_Edit",menubar);
	m = cs_menuitem("_Cut",shell,accel,GDK_X,GDK_CONTROL_MASK,mainmenu_edit_cut,NULL);
	m = cs_menuitem("C_opy",shell,accel,GDK_C,GDK_CONTROL_MASK,mainmenu_edit_copy,NULL);
	m = cs_menuitem("_Paste",shell,accel,GDK_V,GDK_CONTROL_MASK,mainmenu_edit_paste,NULL);
	m = cs_menuitem("_Delete",shell,accel,GDK_Delete,GDK_CONTROL_MASK,mainmenu_edit_delete,NULL);
	cs_menuseparator(shell);
	m = cs_menuitem("_Invert Selection",shell,accel,GDK_I,GDK_CONTROL_MASK,mainmenu_invert_selection,NULL);
	cs_menuseparator(shell);
	m = cs_menuitem("Select _All",shell,accel,GDK_A,GDK_CONTROL_MASK,mainmenu_select_all,NULL);
	m = cs_menuitem("Dese_lect All",shell,accel,GDK_D,GDK_CONTROL_MASK,mainmenu_deselect_all,NULL);

	// "View" menu shell
	shell = cs_menushell("_View",menubar);
	m = cs_menuitem("_Reset",shell,accel,GDK_R,GDK_CONTROL_MASK,mainmenu_change_view,"reset");
	m = cs_menuitem("Zoom _In",shell,accel,GDK_plus,GDK_CONTROL_MASK,mainmenu_change_view,"zoomin");
	m = cs_menuitem("Zoom _Out",shell,accel,GDK_minus,GDK_CONTROL_MASK,mainmenu_change_view,"zoomout");
	cs_menuseparator(shell);
	// -- Model Rendering Style subshell
	m = cs_menuitem("_Model Style",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	gui.DSmenuitems[DS_STICK] = NULL;
	for (i=0; i<DS_NITEMS; i++) gui.DSmenuitems[i] = cs_radiomenuitem(text_from_DS((draw_style) i),subshell,gui.DSmenuitems[DS_STICK],mainmenu_stylechange,NULL);
	// -- View Style subshell
	m = cs_menuitem("_View _Style",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	m = cs_radiomenuitem("_Perspective",subshell,NULL,mainmenu_change_view,"perspective");
	m2 = cs_radiomenuitem("_Orthographic",subshell,m,mainmenu_change_view,"orthographic");
	// -- Atom colouring scheme subshell
	m = cs_menuitem("C_olour Scheme",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	menu_colscheme[0] = cs_radiomenuitem("_Element",subshell,NULL,mainmenu_colourchange,NULL);
	menu_colscheme[1] = cs_radiomenuitem("_Charge",subshell,menu_colscheme[0],mainmenu_colourchange,NULL);
	menu_colscheme[2] = cs_radiomenuitem("_Velocity",subshell,menu_colscheme[0],mainmenu_colourchange,NULL);
	menu_colscheme[3] = cs_radiomenuitem("_Force",subshell,menu_colscheme[0],mainmenu_colourchange,NULL);
	cs_menuseparator(shell);
	m = cs_menuitem("_Labels",shell,accel,GDK_L,GDK_CONTROL_MASK,mainmenu_showlabelwin,NULL);
	m = cs_menuitem("_Force Arrows",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_forcearrowchange,NULL);
	m = cs_menuitem("_Cell",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showcellviewwin,NULL);

	// 'Model' menu shell
	shell = cs_menushell("_Model",menubar);
	m = cs_menuitem("_Toggle List",shell,accel,GDK_M,GDK_CONTROL_MASK,mainmenu_click_model,"togglewin");
	m = cs_menuitem("Next",shell,accel,GDK_Page_Down,GDK_CONTROL_MASK,mainmenu_click_model,"next");
	m = cs_menuitem("Previous",shell,accel,GDK_Page_Up,GDK_CONTROL_MASK,mainmenu_click_model,"prev");

	// 'Forcefield' menu shell
	shell = cs_menushell("_Forcefields",menubar);
	m = cs_menuitem("Forcefield _Manager",shell,accel,GDK_F,GDK_CONTROL_MASK,mainmenu_click_ff,"showffwin");
	m = cs_menuitem("Perform _Typing",shell,accel,GDK_T,GDK_CONTROL_MASK,mainmenu_click_ff,"type");
	m = cs_menuitem("_Remove Typing",shell,accel,GDK_Y,GDK_CONTROL_MASK,mainmenu_click_ff,"untype");
	m = cs_menuitem("_Atomtype Selector",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showatselwin,NULL);


	// 'Cell' menu shell
	shell = cs_menushell("_Cell",menubar);
	m = cs_menuitem("_Define",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showcelleditwin,NULL);
	m = cs_menuitem("_Replicate",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showcellrepwin,NULL);
	m = cs_menuitem("Fold _Atoms",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_foldatoms,NULL);
	m = cs_menuitem("Fold _Molecules",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_foldmolecules,NULL);
	m = cs_menuitem("Expand / Shrink",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_expandshrink,NULL);

	// 'Build' menu shell
	shell = cs_menushell("_Build",menubar);
	m = cs_menuitem("_Atomic Builder",shell,accel,GDK_B,GDK_CONTROL_MASK,mainmenu_showatombuildwin,NULL);
	m = cs_menuitem("_Disordered Builder",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showmcinsertwinshow,NULL);
	cs_menuseparator(shell);
	m = cs_menuitem("_Position Atoms",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showatomposwin,NULL);
	m = cs_menuitem("Atom _List",shell,accel,GDK_K,GDK_CONTROL_MASK,mainmenu_showatomlistwin,NULL);

	// "Tools" menu shell
	shell = cs_menushell("_Tools",menubar);
	m = cs_menuitem("_Energy Setup",shell,accel,GDK_E,GDK_CONTROL_MASK,mainmenu_showenergywin,NULL);
	m = cs_menuitem("_Minimiser",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showminimiserwin,NULL);
	m = cs_menuitem("Me_asure",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showgeometrywin,NULL);
	m = cs_menuitem("_Periodic Table",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showptwin,NULL);
	m = cs_menuitem("Probe _Atom",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_atominfo,NULL);
	m = cs_menuitem("Test1",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showepsrwin,NULL);
	m = cs_menuitem("Test2",shell,NULL,0,GDK_CONTROL_MASK,test_function,NULL);

	// "Analyse" menu shell
	shell = cs_menushell("_Analyse",menubar);
	m = cs_menuitem("_Average Molecule",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showavgmolwin,NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	m = cs_menuitem("_Free Volume",shell,NULL,0,GDK_CONTROL_MASK,mainmenu_showvolwin,NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);

	// "Options" menu shell
	shell = cs_menushell("_Options",menubar);
	m = cs_menuitem("_Preferences",shell,accel,GDK_P,GDK_CONTROL_MASK,mainmenu_showprefswin,NULL);
	m = cs_menuitem("_Colours",shell,accel,GDK_C,GDK_CONTROL_MASK,mainmenu_showcolourwin,NULL);
	m = cs_checkmenuitem("_Debug",shell,mainmenu_debugchange,NULL);

	dbg_end(DM_CALLS,"gui_gtk::create_menu");
	return menubar;
}
