/*
	*** GTK cell expand window
	*** src/gui-gtk/cell_expand.cpp
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

// Variables
GtkCheckButton *cell_expandaxis_checks[3];
GtkSpinButton *cell_expandscale_spin;

gint cellexpandwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_CELLEXPAND]);
	gui.refresh();
	return TRUE;
}

void cellexpandwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(gui.windows[GW_CELLEXPAND]);
	gui.refresh();
}

void cellexpandwin_click_expandshrink(GtkButton *widget, gpointer data)
{
	vec3<double> scale;
	scale.set(1.0,1.0,1.0);
	double fac = gtk_spin_button_get_value(cell_expandscale_spin);
	for (int i=0; i<3; i++)
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(cell_expandaxis_checks[i]))) scale.set(i,fac);
	master.get_currentmodel()->scale_cell(scale);
	gui.update_labels();
	gui.refresh();
}

void gui_gtk::cellexpandwin_create()
{
	// Create the cell view window
	dbg_begin(DM_CALLS,"gui_gtk::cellexpandwin_create");
	GtkBox *mainbox, *box, *hbox;
	GtkButton *b;
	GtkObject *adj;
	GtkTable *table;
	GtkLabel *l;
	GtkFrame *frame;
	// Create the window and register some callbacks.
	windows[GW_CELLEXPAND] = cs_subwindow("Expand / Shrink",FALSE,cellexpandwin_close);

	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_CELLEXPAND]);

	// Frame to hold 'Ratio'
	frame = cs_frame("Ratio",mainbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,TRUE,0,frame);
	hbox = cs_box(CS_HORIZONTAL,TRUE,0,box,CS_START,TRUE,TRUE,0);
	l = cs_label("Ratio",hbox,CS_START,FALSE,TRUE,0);
	adj = cs_adjustment(1.0,0.0,2.0,0.01,0.01);
	cell_expandscale_spin = cs_spin(adj,1.0,3,hbox,CS_END,TRUE,TRUE,0,NULL,NULL);
	hbox = cs_box(CS_HORIZONTAL,TRUE,0,box,CS_START,TRUE,TRUE,0);
	b = cs_button("Scale", hbox, CS_START, TRUE, TRUE, 0, cellexpandwin_click_expandshrink, NULL);

	// Frame to hold axis lock checkboxes
	frame = cs_frame("Axes",mainbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,TRUE,0,frame);
	cell_expandaxis_checks[0] = cs_check("X",TRUE,box,CS_START,TRUE,TRUE,0,NULL,NULL);
	cell_expandaxis_checks[1] = cs_check("Y",TRUE,box,CS_START,TRUE,TRUE,0,NULL,NULL);
	cell_expandaxis_checks[2] = cs_check("Z",TRUE,box,CS_END,TRUE,TRUE,0,NULL,NULL);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,cellexpandwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::cellexpandwin_create");
}
