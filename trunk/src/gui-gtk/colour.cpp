/*
	*** GTK colour selector window
	*** src/gui-gtk/colour.cpp
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
#include "model/model.h"
#include "base/master.h"
#include "base/prefs.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"

// Variables
GtkWidget *colsel;
treeview_list col_list;
GdkColor editcolour;
int CURRENTCOL;

gint colourwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_COLOUR]);
	gui.refresh();
	return TRUE;
}

void colourwin_click_closebtn(GtkButton *widget, gpointer data)
{
	colourwin_close(NULL,NULL,NULL);
}

void colourwin_list_changed(GtkTreeSelection *selection, gpointer data)
{
	// Called when a different item is selected in the colour list
	dbg_begin(DM_CALLS,"colourwin_list_changed");
	// First, save the changed editcolour into the currently (soon to be last) selection colour item
	GLint *cc;
	GtkTreeModel *active_model;
	GtkTreeIter selected_row;
	if (col_list.get_selected(&selected_row))
		col_list.get(&selected_row, COL_COL_ID, &CURRENTCOL);
	cc = prefs.get_colour((colour)CURRENTCOL);
	editcolour.red = int( (double(cc[0]) / INT_MAX) * 65535.0 );
	editcolour.green = int( (double(cc[1]) / INT_MAX) * 65535.0 );
	editcolour.blue = int( (double(cc[2]) / INT_MAX) * 65535.0 );
	gtk_color_selection_set_current_color(GTK_COLOR_SELECTION(colsel),&editcolour);
	dbg_end(DM_CALLS,"colourwin_list_changed");
}

void colourwin_change_colour(GtkWidget *sender, gpointer data)
{
	// Colour has been modified - dynamically update the model space to show the change
	dbg_begin(DM_CALLS,"colourwin_change_colour");
	gtk_color_selection_get_current_color(GTK_COLOR_SELECTION(colsel),&editcolour);
	GLint cc[3];
	cc[0] =(GLint) ((double(editcolour.red) / 65535.0) * INT_MAX);
	cc[1] =(GLint) ((double(editcolour.green) / 65535.0) * INT_MAX);
	cc[2] =(GLint) ((double(editcolour.blue) / 65535.0) * INT_MAX);
	prefs.set_colour((colour)CURRENTCOL, cc[0], cc[1], cc[2], INT_MAX);
	// Must do certain actions depending on whichi colour was modified
	switch (CURRENTCOL)
	{
		case (COL_PEN) : break;
		case (COL_BG) : gui.mainview.init_gl(); break;
		case (COL_ACSCHEMELO) : master.get_currentmodel()->set_atom_colours(NULL); break;
		case (COL_ACSCHEMEMID) : master.get_currentmodel()->set_atom_colours(NULL); break;
		case (COL_ACSCHEMEHI) : master.get_currentmodel()->set_atom_colours(NULL); break;
		case (COL_SPECREFLECT) : break;
		default : printf("colourwin_change : Didn't recognise CURRENTCOL.\n");
	}
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
	dbg_end(DM_CALLS,"colourwin_change_colour");
}

void gui_gtk::colourwin_create()
{
	// Create the colour selection / editing window
	dbg_begin(DM_CALLS,"gui_gtk::colourwin_create");
	GtkTreeIter iter;
	GtkCellRenderer *rend;
	GtkBox *box, *mainbox;
	GtkScrolledWindow *scroll;
	GtkButton *b;
	windows[GW_COLOUR] = cs_subwindow("Colours",FALSE,colourwin_close);

	// A single vbox will hold the controls in the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(windows[GW_COLOUR]));

	// The colour selector widget
	colsel = gtk_color_selection_new();
	gtk_color_selection_set_has_opacity_control(GTK_COLOR_SELECTION(colsel),FALSE);
	gtk_box_pack_start(mainbox,colsel,FALSE,TRUE,2);
	GLint *cc;
	cc = prefs.get_colour(COL_PEN);
	editcolour.red = int( (double(cc[0]) / INT_MAX) * 65535.0 );
	editcolour.green = int( (double(cc[1]) / INT_MAX) * 65535.0 );
	editcolour.blue = int( (double(cc[2]) / INT_MAX) * 65535.0 );
	gtk_color_selection_set_current_color(GTK_COLOR_SELECTION(colsel),&editcolour);
	g_signal_connect(G_OBJECT(colsel),"color_changed",G_CALLBACK(colourwin_change_colour),NULL);

	// List widget with available editable colours
	box = cs_box(CS_VERTICAL,FALSE,2,mainbox,CS_START,FALSE,TRUE,2);
	col_list.initialise("colourlist","si");
	scroll = cs_treeview(&col_list,box,CS_START,FALSE,TRUE,0,colourwin_list_changed);
	// -- Add the columns
	rend = cs_column(&col_list,"Colour","text",COL_COL_NAME);
	col_list.set_size(200,200);

	// -- Populate the list
	for (int n=COL_PEN; n<COL_NITEMS; n++)
		col_list.append_and_set(&iter,COL_COL_NAME,text_from_COL(colour(n)),COL_COL_ID,n,-1);
	CURRENTCOL = 0;
	gtk_tree_model_get_iter_first(col_list.get_treemodel(),&iter);
        col_list.select(&iter);

	// Separator
	GtkWidget *sep = gtk_vseparator_new();
	gtk_box_pack_start(mainbox,sep,FALSE,TRUE,0);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,colourwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::colourwin_create");
}

