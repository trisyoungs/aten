/*
	*** GTK cell repeat window
	*** src/gui-gtk/cell_rep.cpp

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
GtkSpinButton *cell_repvec_edits[3];

gint cellrepwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_CELLREPEAT]);
	gui.refresh();
	return TRUE;
}

void cellrepwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(gui.windows[GW_CELLREPEAT]);
	gui.refresh();
}

void cellrepwin_repvec_changed(GtkSpinButton *widget, gpointer data)
{
}

void cellrepwin_click_replicate(GtkButton *widget, gpointer data)
{
	vec3<double> pos;
	for (int i=0; i<3; i++) pos.set(i,gtk_spin_button_get_value(cell_repvec_edits[i]));
	master.get_currentmodel()->replicate_cell(pos,pos);
}

void gui_gtk::cellrepwin_create()
{
	// Create the cell view window
	dbg_begin(DM_CALLS,"gui_gtk::cellrepwin_create");
	GtkBox *mainbox, *box;
	GtkButton *b;
	GtkObject *adj;
	GtkTable *table;
	GtkFrame *frame;
	// Create the window and register some callbacks.
	windows[GW_CELLREPEAT] = cs_subwindow("Replicate",FALSE,cellrepwin_close);

	// VBox will contain a tabbed notebook
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_CELLREPEAT]);

	table = cs_table(5,2,FALSE,mainbox);
	// Add in repeat cell spinedits
	for (int n=0; n<3; n++)
	{
		adj = cs_adjustment(0,0,1.0,0.01,0.01);
		cell_repvec_edits[n] = cs_spin(adj,1,0,table,1,2,n+1,n+2,cellrepwin_repvec_changed,NULL);
	}
	cs_label("X",table,0,1,1,2);
	cs_label("Y",table,0,1,2,3);
	cs_label("Z",table,0,1,3,4);
	b = cs_button("Replicate",table,1,3,4,5,cellrepwin_click_replicate,NULL);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,cellrepwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::cellrepwin_create");
}
