/*
	*** GTK control prefs window
	*** src/gui-gtk/prefs_control.cpp
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
#include "base/master.h"
#include "base/prefs.h"

void prefswin_change_mousebind(GtkComboBox *widget, gpointer data)
{
	// Update the mouse bindings
	dbg_begin(DM_CALLS,"prefswin_change_mousebind");
	mouse_action temp = (mouse_action)gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
	for (int n=0; n<MB_NITEMS; n++) if (GTK_COMBO_BOX(gui.MBcombos[n]) == widget) prefs.set_mb_action((mouse_button) n,temp);
	dbg_end(DM_CALLS,"prefswin_change_mousebind");
}

void prefswin_change_keymodbind(GtkComboBox *widget, gpointer data)
{
	// Update the key modifier bindings
	dbg_begin(DM_CALLS,"prefswin_change_keymodbind");
	key_action ka = (key_action)gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
	for (int n=0; n<MK_NITEMS; n++) if (GTK_COMBO_BOX(gui.MKcombos[n]) == widget) prefs.set_keymod_action((modifier_key) n,ka);
	dbg_end(DM_CALLS,"prefswin_change_keymodbind");
}

void gui_gtk::prefswin_create_control(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::prefswin_create_control");
	int n;
	GtkBox *page, *vbox;
	GtkTable *table;
	GtkFrame *frame;
	// Create the page
	page = cs_notebookpage("Control",notebook);
	// Create a vbox to put all the frames into...
	vbox = cs_box(CS_VERTICAL,FALSE,0,page);
	// First frame - Mouse bindings
	frame = cs_frame("Mouse Bindings",vbox,CS_START,FALSE,TRUE,0);
	table = cs_table(4,2,TRUE,frame);
	cs_label("Left Button",table,0,1,0,1);
	cs_label("Middle Button",table,0,1,1,2);
	cs_label("Right Button",table,0,1,2,3);
	cs_label("Wheel",table,0,1,3,4);
	for (n=0; n<MB_NITEMS; n++)
		gui.MBcombos[n] = cs_combo(get_MA_strings(), MA_NITEMS, 0, table, 1, 2, n, n+1, prefswin_change_mousebind);
	// Second frame - Key Modifier bindings
	frame = cs_frame("Modifier Bindings",vbox,CS_START,FALSE,TRUE,0);
	table = cs_table(4,2,TRUE,frame);
	cs_label("Shift",table,0,1,0,1);
	cs_label("Ctrl",table,0,1,1,2);
	cs_label("Alt",table,0,1,2,3);
	for (n=0; n<MK_NITEMS; n++)
		gui.MKcombos[n] = cs_combo(get_KA_strings(), KA_NITEMS, 0, table, 1, 2, n, n+1, prefswin_change_keymodbind);

	dbg_end(DM_CALLS,"gui_gtk::prefswin_create_control");
}
