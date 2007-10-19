/*
	*** GTK view prefs window
	*** src/gui-gtk/prefs_view.cpp
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

#include <gtk/gtk.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "render/globs.h"
#include "base/master.h"
#include "base/prefs.h"

void prefswin_change_size(GtkSpinButton *sender, gpointer data)
{
	dnchar action;
	action.set((char*) data);
	// If we don't recognise the action, assume its a DS
	if (action == "bondtube") prefs.set_tube_size(gtk_spin_button_get_value(sender));
	else if (action == "selscale") prefs.set_selection_scale(gtk_spin_button_get_value(sender));
	else
	{
		draw_style ds = DS_from_text(action.get());
		prefs.set_atom_size(ds,gtk_spin_button_get_value(sender));
	}
	canvas_master::globs.recreate_all();
	master.get_currentmodel()->project_all();
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void prefswin_click_globe(GtkCheckButton *widget, gpointer data)
{
	prefs.set_visible(VO_GLOBE, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	gui.refresh();
}

void prefswin_click_cell(GtkCheckButton *widget, gpointer data)
{
	prefs.set_visible(VO_CELL, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void prefswin_click_axes(GtkCheckButton *widget, gpointer data)
{
	prefs.set_visible(VO_CELLAXES, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void gui_gtk::prefswin_create_view(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::prefswin_create_view");
	GtkBox *page, *vbox;
	GtkTable *table;
	GtkFrame *frame;
	GtkScale *s;
	GtkObject *adj;
	// Create the page
	page = cs_notebookpage("View",notebook);
	// Create a vbox to put all the frames into...
	vbox = cs_box(CS_VERTICAL,FALSE,0,page);

	// First frame - Atom Sizes
	frame = cs_frame("Atom Sizes",vbox,CS_START,FALSE,TRUE,0);
	table = cs_table(7,2,FALSE,frame);
	for (int n=0; n<DS_INDIVIDUAL; n++)
	{
		cs_label(text_from_DS((draw_style) n),table,0,1,n,n+1);
		adj = cs_adjustment(prefs.get_atom_size((draw_style) n),0.01,2.0,0.01,0);
		DSspins[n] = cs_spin(adj, 0.01, 2, table, 1, 2, n, n+1, prefswin_change_size, text_from_DS((draw_style) n));
	}
	cs_label("Bond Tube Size",table,0,1,4,5);
	adj = cs_adjustment(prefs.get_tube_size(),0.01,1.0,0.01,0);
	spin[W_SPN_TUBESIZE] = cs_spin(adj, 0.01, 2, table, 1, 2, 4, 5, prefswin_change_size, "bondtube");
	cs_label("Selection Scale",table,0,1,6,7);
	adj = cs_adjustment(prefs.get_selection_scale(),1.0,3.0,0.1,0);
	spin[W_SPN_SELSCALE] = cs_spin(adj, 0.01, 2, table, 1, 2, 6, 7, prefswin_change_size, "selscale");

	// Second frame - view objects
	frame = cs_frame("Objects",vbox,CS_END,FALSE,TRUE,0);
	table = cs_table(2,2,FALSE,frame);
	check[W_CHK_SHOWGLOBE] = cs_check("Rotation globe",prefs.should_render(VO_GLOBE), table,0,1,0,1,prefswin_click_globe,NULL);
	check[W_CHK_SHOWCELL] = cs_check("Unit cell",prefs.should_render(VO_CELL),table,0,1,1,2,prefswin_click_cell,NULL);
	check[W_CHK_SHOWAXES] = cs_check("Unit cell axes",prefs.should_render(VO_CELLAXES),table,1,2,0,1,prefswin_click_axes,NULL);

	dbg_end(DM_CALLS,"gui_gtk::prefswin_create_view");
}
