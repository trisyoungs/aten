/*
	*** GTK cell view window
	*** src/gui-gtk/cell_view.cpp
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
GtkSpinButton *cell_viewpos_edits[3], *cell_viewneg_edits[3];

gint cellviewwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_CELLVIEW]);
	gui.refresh();
	return TRUE;
}

void cellviewwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cellviewwin_close(NULL,NULL,NULL);
}

void cellviewwin_viewpos_changed(GtkSpinButton *widget, gpointer data)
{
	int i;
	for (i=0; i<3; i++) if (widget == cell_viewpos_edits[i]) break;
	prefs.set_repcellpos(i,gtk_spin_button_get_value_as_int(widget));
	gui.refresh();
}

void cellviewwin_viewneg_changed(GtkSpinButton *widget, gpointer data)
{
	int i;
	for (i=0; i<3; i++) if (widget == cell_viewneg_edits[i]) break;
	prefs.set_repcellneg(i,gtk_spin_button_get_value_as_int(widget));
	gui.refresh();
}

void create_cell_repeat_page(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"create_cell_repeat_page");
	GtkBox *page, *mainbox;
	GtkTable *table;
	GtkObject *adj;
	int n,row,col;
	// Create the page
	page = cs_notebookpage("Repeat",notebook);
	table = cs_table(4,4,FALSE,page);
	// Add in repeat cell spinedits
	for (n=0; n<3; n++)
	{
		adj = cs_adjustment(0,0,10,1,1);
		cell_viewpos_edits[n] = cs_spin(adj,1,0,table,2,3,n+1,n+2,cellviewwin_viewpos_changed,NULL);
		adj = cs_adjustment(0,0,10,1,1);
		cell_viewneg_edits[n] = cs_spin(adj,1,0,table,0,1,n+1,n+2,cellviewwin_viewneg_changed,NULL);
	}
	cs_label("X",table,1,2,1,2);
	cs_label("Y",table,1,2,2,3);
	cs_label("Z",table,1,2,3,4);
	dbg_end(DM_CALLS,"create_cell_repeat_page");
}

void gui_gtk::cellviewwin_create()
{
	// Create the cell view window
	dbg_begin(DM_CALLS,"gui_gtk::cellviewwin_create");
	GtkBox *mainbox, *box;
	GtkNotebook *notebook;
	GtkButton *b;
	GtkFrame *frame;
	// Create the window and register some callbacks.
	windows[GW_CELLVIEW]= cs_subwindow("Cell View",FALSE,cellviewwin_close);

	// VBox will contain a tabbed notebook
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_CELLVIEW]);

	// Create the notebook
	notebook = cs_notebook(mainbox,CS_START,FALSE,TRUE,0);
	create_cell_repeat_page(notebook);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,cellviewwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::cellviewwin_create");
}
