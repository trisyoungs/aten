/*
	*** GTK cell window
	*** src/gui-gtk/cell.cpp

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

cell_type win_cell_celltype = CT_NONE;

gint celleditwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_CELLEDIT]);
	gui.refresh();
	return TRUE;
}

void celleditwin_click_closebtn(GtkButton *widget, gpointer data)
{
	celleditwin_close(NULL,NULL,NULL);
}

void gui_gtk::celleditwin_update()
{
	// Set the controls of the window to reflect the model
	if (!gui.exists()) return;
	celleditwin_UPDATING = TRUE;
	model *m = master.get_currentmodel();
	vec3<double> v1,v2;
	mat3<double> cellmat;
	int n, i;
	cell_type ct = m->cell.get_type();
	// Set all controls off to start with...
	for (n=0; n<3; n++)
	{
		gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_lengths[n]),FALSE);
		gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_angles[n]),FALSE);
		for (i=0; i<3; i++) gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_matrix[n*3+i]),FALSE);
	}
	// Set cell controls
	switch (ct)
	{
		case (CT_NONE):		// No cell - all controls remain off
			break;
		case (CT_CUBIC):	// All off except M[0] and L[A]
			gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_matrix[0]),TRUE);
			gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_lengths[0]),TRUE);
			break;
		case (CT_ORTHORHOMBIC):	// Cell lengths and diagonal terms only
			for (n=0; n<3; n++)
			{
				gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_matrix[n*3+n]),TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_lengths[n]),TRUE);
			}
			break;
		case (CT_PARALLELEPIPED): // All on
			for (n=0; n<9; n++) gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_matrix[n]),TRUE);
			for (n=0; n<3; n++)
			{
				gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_lengths[n]),TRUE);
				gtk_widget_set_sensitive(GTK_WIDGET(celleditwin_angles[n]),TRUE);
			}
	}
	// Put data from model into controls
	if (ct != CT_NONE)
	{
		// Add in values from the model
		// Cell parameters
		v1 = m->cell.get_lengths();
		v2 = m->cell.get_angles();
		for (n=0; n<3; n++)
		{
			gtk_spin_button_set_value(celleditwin_lengths[n],v1.get(n));
			gtk_spin_button_set_value(celleditwin_angles[n],v2.get(n));
		}
		// Cell matrix
		cellmat = m->cell.get_axes();
		for (n=0; n<9; n++) gtk_spin_button_set_value(celleditwin_matrix[n],cellmat.get_element(n));
				
	}
	// Lastly, set cell combo box. This also 'nulls' values in the other controls that might otherwise 'bugger about'
	gtk_combo_box_set_active(celleditwin_typecombo,ct);
	celleditwin_UPDATING = FALSE;
}

void celleditwin_matrix_changed(GtkSpinButton *widget, gpointer data)
{
	gui.celleditwin_changed(TRUE);
}

void celleditwin_params_changed(GtkSpinButton *widget, gpointer data)
{
	gui.celleditwin_changed(FALSE);
}

void gui_gtk::celleditwin_changed(bool wasmatrix)
{
	// Change the cell data in the model, unless we are just updating the controls
	if (celleditwin_UPDATING) return;
	double d;
	vec3<double> lengths, angles;
	mat3<double> matrix;
	// For cubic cell type, set lengths to all be the same
	if (master.get_currentmodel()->cell.get_type() == CT_CUBIC)
	{
		celleditwin_UPDATING = TRUE;
		// Get value to set other controls to
		if (wasmatrix) d = gtk_spin_button_get_value(gui.celleditwin_matrix[0]);
		else d = gtk_spin_button_get_value(gui.celleditwin_lengths[0]);
		// Set other widgets
		gtk_spin_button_set_value(celleditwin_lengths[0],d);
		gtk_spin_button_set_value(celleditwin_lengths[1],d);
		gtk_spin_button_set_value(celleditwin_lengths[2],d);
		gtk_spin_button_set_value(celleditwin_matrix[0],d);
		gtk_spin_button_set_value(celleditwin_matrix[4],d);
		gtk_spin_button_set_value(celleditwin_matrix[8],d);
		gui.celleditwin_UPDATING = FALSE;
	}
	// Grab values from controls and set new cell values
	if (wasmatrix)
	{
		for (int n=0; n<9; n++) matrix.set(n,gtk_spin_button_get_value(celleditwin_matrix[n]));
		master.get_currentmodel()->cell.set(matrix);
	}
	else
	{
		for (int n=0; n<3; n++)
		{
			lengths.set(n,gtk_spin_button_get_value(celleditwin_lengths[n]));
			angles.set(n,gtk_spin_button_get_value(celleditwin_angles[n]));
		}
		master.get_currentmodel()->cell.set(lengths,angles);
	}
	celleditwin_update();
	gui.refresh();
}

void celleditwin_celltype_changed(GtkComboBox *widget, gpointer data)
{
	// If we are just updating, set some values in the other controls to reflect the cell type
	int n;
	cell_type ct = (cell_type) gtk_combo_box_get_active(widget);
	switch (ct)
	{
		case (CT_CUBIC): 	// Cell angles must be 90...
		case (CT_ORTHORHOMBIC):
			for (n=0; n<3; n++) gtk_spin_button_set_value(gui.celleditwin_angles[n],90.0);
			break;
	}
	win_cell_celltype = ct;
	// Update the model
	// Set data in model by calling celleditwin_params_changed...
	celleditwin_params_changed(NULL,NULL);
}

void gui_gtk::celleditwin_create_matrix(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::celleditwin_create_matrix");
	GtkBox *page, *mainbox;
	GtkTable *table;
	GtkObject *adj;
	int n,row,col;
	// Create the page
	page = cs_notebookpage("Matrix",notebook);
	table = cs_table(3,4,FALSE,GTK_CONTAINER(page));
	// Add in matrix spinedits
	for (n=0; n<9; n++)
	{
		row = n/3;
		col = n%3 + 1;
		if (n == 0 || n == 4 || n == 8) adj = cs_adjustment(1.0,1.0,1000.0,0.1,0.1);
		else adj = cs_adjustment(0.0,0.0,1000.0,0.1,0.1);
		gui.celleditwin_matrix[n] = cs_spin( adj, 0.1, 5, table, col, col+1, row, row+1, celleditwin_matrix_changed,NULL);
	}
	cs_label("X",table,0,1,0,1);
	cs_label("Y",table,0,1,1,2);
	cs_label("Z",table,0,1,2,3);
	dbg_end(DM_CALLS,"gui_gtk::celleditwin_create_matrix");
}

void gui_gtk::celleditwin_create_params(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::celleditwin_create_params");
	GtkBox *page, *mainbox;
	GtkTable *table;
	GtkObject *adj;
	int n,row,col;
	// Create the page
	page = cs_notebookpage("Parameters",notebook);
	table = cs_table(3,4,FALSE,page);
	// Add in length / angle spinedits
	for (n=0; n<3; n++)
	{
		adj = cs_adjustment(1.0,1.0,1000.0,0.1,0.1);
		celleditwin_lengths[n] = cs_spin(adj,0.1,5,table,n+1,n+2,1,2,celleditwin_params_changed,NULL);
		adj = cs_adjustment(0.0,0.0,180.0,0.1,0.1);
		celleditwin_angles[n] = cs_spin(adj,0.1,5,table,n+1,n+2,2,3,celleditwin_params_changed,NULL);
	}
	cs_label("A",table,1,2,0,1);
	cs_label("B",table,2,3,0,1);
	cs_label("C",table,3,4,0,1);
	cs_label("Lengths",table,0,1,1,2);
	cs_label("Angles",table,0,1,2,3);
	dbg_end(DM_CALLS,"gui_gtk::celleditwin_create_params");
}

void gui_gtk::celleditwin_create()
{
	// Create the cell edit window
	dbg_begin(DM_CALLS,"gui_gtk::celleditwin_create");
	GtkBox *mainbox, *box;
	GtkNotebook *notebook;
	GtkButton *b;
	GtkFrame *frame;
	// Create the window and register some callbacks.
	windows[GW_CELLEDIT] = cs_subwindow("Cell Edit",TRUE,celleditwin_close);

	// VBox will contain a tabbed notebook (for switching between matrix and parameter views) and a frame (for other tools)
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_CELLEDIT]);

	// Cell type combobox
	celleditwin_typecombo = cs_combo(get_CT_strings(),CT_NITEMS,0,mainbox,CS_START,FALSE,FALSE,0,celleditwin_celltype_changed);

	// Create the notebook
	notebook = cs_notebook(mainbox,CS_START,FALSE,TRUE,0);
	celleditwin_create_matrix(notebook);
	celleditwin_create_params(notebook);

	// Subframe - Cell tools
//	frame = cs_frame("Actions",mainbox,CS_START,FALSE,TRUE,0);
//	box = cs_box(CS_VERTICAL,FALSE,frame);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,celleditwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::celleditwin_create");
}
