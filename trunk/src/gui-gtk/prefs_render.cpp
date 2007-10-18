/*
	*** GTK render prefs window
	*** src/gui-gtk/prefs_render.cpp

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

#include <gtk/gtk.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"
#include "base/prefs.h"
#include "render/globs.h"

void prefswin_change_quality(GtkRange *sender, GtkScrollType scroll, gdouble value, gpointer data)
{
	// Either the atom or bond qualities have been changed. Store the new values and post a redraw.
	dbg_begin(DM_CALLS,"prefswin_change_quality");
	if (sender == GTK_RANGE(gui.scale[W_SCL_ATOMQUAL]))
		prefs.set_atom_detail((int)gtk_range_get_value(GTK_RANGE(gui.scale[W_SCL_ATOMQUAL])));
	else
		prefs.set_bond_detail((int)gtk_range_get_value(GTK_RANGE(gui.scale[W_SCL_BONDQUAL])));
	canvas_master::globs.recreate_all();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
	dbg_end(DM_CALLS,"prefswin_change_quality");
}

void prefswin_change_shininess(GtkRange *sender, GtkScrollType scroll, gdouble value, gpointer data)
{
	dbg_begin(DM_CALLS,"prefswin_change_shininess");
	prefs.set_shininess((int)gtk_range_get_value(sender));
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
	dbg_end(DM_CALLS,"prefswin_change_shininess");
}

void prefswin_click_gloption(GtkCheckButton *widget, gpointer data)
{
	dnchar action;
	action.set((char*)data);
	gl_option glo;
	if (action == "fog") glo = GO_FOG;
	else if (action == "cull") glo = GO_BACKCULLING;
	else if (action == "line") glo = GO_LINEALIASING;
	else if (action == "poly") glo = GO_POLYALIASING;
	bool active = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
	active ? prefs.add_gl_option(glo) : prefs.remove_gl_option(glo);
	gui.mainview.init_gl();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void prefswin_change_fognear(GtkRange *sender, GtkScrollType scroll, gdouble value, gpointer data)
{
	prefs.set_fog_near(gtk_range_get_value(GTK_RANGE(sender)));
	gui.mainview.init_gl();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void prefswin_change_fogfar(GtkRange *sender, GtkScrollType scroll, gdouble value, gpointer data)
{
	prefs.set_fog_far(gtk_range_get_value(GTK_RANGE(sender)));
	gui.mainview.configure();
	gui.mainview.init_gl();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void gui_gtk::prefswin_create_render(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::prefswin_create_render");
	GtkBox *page, *vbox, *qualboxh, *qualboxvl, *qualboxvr;
	GtkTable *table;
	GtkFrame *frame;
	GtkLabel *l;
	GtkObject *adj;
	GtkScale *s;
	// Create the page
	page = cs_notebookpage("Rendering",notebook);
	// Create a vbox to put all the frames into...
	vbox = cs_box(CS_VERTICAL,FALSE,0,page);

	// First frame - Quality
	frame = cs_frame("Quality",vbox,CS_START,FALSE,TRUE,0);
	// -- Boxes for quality sliders
	qualboxh = cs_box(CS_HORIZONTAL,FALSE,0,frame);
	qualboxvl = cs_box(CS_VERTICAL,FALSE,0,qualboxh,CS_START,FALSE,TRUE,0);
	qualboxvr = cs_box(CS_VERTICAL,FALSE,0,qualboxh,CS_END,TRUE,TRUE,0);
	// -- Scale widgets for quality
	l = cs_label("Atom Detail",qualboxvl,CS_START,TRUE,TRUE,0);
	adj = cs_adjustment(1.0*prefs.get_atom_detail(),3.0,50.0,1.0,1.0);
	scale[W_SCL_ATOMQUAL] = cs_scale(adj,qualboxvr,CS_START,TRUE,TRUE,0,prefswin_change_quality,NULL);
	l = cs_label("Bond Detail",qualboxvl,CS_START,TRUE,TRUE,0);
	adj = cs_adjustment(1.0*prefs.get_bond_detail(),3.0,20.0,1.0,1.0);
	scale[W_SCL_BONDQUAL] = cs_scale(adj,qualboxvr,CS_START,TRUE,TRUE,0,prefswin_change_quality,NULL);
	// -- Shininess
	l = cs_label("Shininess",qualboxvl,CS_START,TRUE,TRUE,0);
	adj = cs_adjustment(1.0*prefs.get_shininess(),1.0,128.0,1.0,1.0);
	scale[W_SCL_SHINY] = cs_scale(adj,qualboxvr,CS_START,TRUE,TRUE,0,prefswin_change_shininess,NULL);

	// Second frame - view effects
	frame = cs_frame("Effects",vbox,CS_START,FALSE,TRUE,0);
	table = cs_table(4,2,FALSE,frame);
	check[W_CHK_USEFOG] = cs_check("Depth cueing",prefs.get_gl_option(GO_FOG),table,0,2,0,1,prefswin_click_gloption,"fog");
	cs_label("Near",table,0,1,1,2);
	adj = cs_adjustment(prefs.get_fog_near(),1.0,100.0,1.0,10.0);
	s = cs_scale(adj,table,1,2,1,2,prefswin_change_fognear,NULL);
	cs_label("Far",table,0,1,2,3);
	adj = cs_adjustment(prefs.get_fog_far(),0.0,100.0,1.0,10.0);
	s = cs_scale(adj,table,1,2,2,3,prefswin_change_fogfar,NULL);
	check[W_CHK_USELINEALIASING] = cs_check("Line aliasing", prefs.get_gl_option(GO_LINEALIASING), table, 0, 2, 3, 4, prefswin_click_gloption, "line");
	check[W_CHK_USEPOLYALIASING] = cs_check("Polygon aliasing", prefs.get_gl_option(GO_POLYALIASING), table, 0, 2, 4, 5, prefswin_click_gloption, "poly");
	dbg_end(DM_CALLS,"gui_gtk::prefswin_create_render");
}
