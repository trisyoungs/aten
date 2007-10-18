/*
	*** GTK labeling window
	*** src/gui-gtk/label.cpp

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
GtkButton *atombtn[AL_NITEMS], *clearbtn[AL_NITEMS];

gint labelwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_LABELS]);
	gui.refresh();
	return TRUE;
}

void labelwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(gui.windows[GW_LABELS]);
	gui.refresh();
}

void labelwin_click_clear_all(GtkButton *widget, gpointer data)
{
	// Clear all the annotations for the current model
	dbg_begin(DM_CALLS,"labelwin_click_clear_all");
	master.get_currentmodel()->clear_atom_labels();
	gui.refresh();
	dbg_end(DM_CALLS,"labelwin_click_clear_all");
}

void labelwin_click_add_annotation(GtkButton *widget, gpointer data)
{
	// Add annotation to the currently selected atoms
	dbg_begin(DM_CALLS,"labelwin_click_add_annotation");
	atom_label al;
	if (GTK_BUTTON(atombtn[0]) == widget) al = AL_ID;
	else if (GTK_BUTTON(atombtn[1]) == widget) al = AL_ELEMENT;
	else if (GTK_BUTTON(atombtn[2]) == widget) al = AL_FFTYPE;
	else if (GTK_BUTTON(atombtn[3]) == widget) al = AL_FFEQUIV;
	else if (GTK_BUTTON(atombtn[4]) == widget) al = AL_CHARGE;
	master.get_currentmodel()->selection_set_atom_labels(al);
	gui.refresh();
	dbg_end(DM_CALLS,"labelwin_click_add_annotation");
}

void labelwin_click_clear_annotation(GtkButton *widget, gpointer data)
{
	// Clear the selected annotation for all selected atoms
	dbg_begin(DM_CALLS,"labelwin_click_clear_annotation");
	atom_label al;
	if (GTK_BUTTON(clearbtn[0]) == widget) al = AL_ID;
	else if (GTK_BUTTON(clearbtn[1]) == widget) al = AL_ELEMENT;
	else if (GTK_BUTTON(clearbtn[2]) == widget) al = AL_FFTYPE;
	else if (GTK_BUTTON(clearbtn[3]) == widget) al = AL_FFEQUIV;
	else if (GTK_BUTTON(clearbtn[4]) == widget) al = AL_CHARGE;
	master.get_currentmodel()->selection_clear_atom_labels(al);
	gui.refresh();
	dbg_end(DM_CALLS,"labelwin_click_clear_annotation");
}

void labelwin_toggle_annotation(GtkCheckButton *widget, gpointer data)
{
	prefs.set_visible(VO_LABELS, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)) ? TRUE : FALSE);
	gui.refresh();
}

void labelwin_change_labelscale(GtkRange *sender, GtkScrollType scroll, gdouble value, gpointer data)
{
	prefs.set_label_scale(gtk_range_get_value(GTK_RANGE(sender)));
	gui.refresh();
}

//
// Widget creation
//

void gui_gtk::labelwin_create()
{
	// Create the window of annotation options
	dbg_begin(DM_CALLS,"gui_gtk::labelwin_create");
	GtkWidget *w;
	GtkBox *mainbox, *box;
	GtkFrame *frame;
	GtkTable *table;
	GtkObject *adj;
	GtkButton *b;
	int n;
	windows[GW_LABELS] = cs_subwindow("Labels",FALSE,labelwin_close);

	// A single vbox will hold the frames for the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(windows[GW_LABELS]));

	// First frame - Atom labelling
	frame = cs_frame("Atom Labelling",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(5,4,TRUE,frame);
	// checkbox for turning on/off atom annotation and button to clear all labelling
	check[W_CHK_SHOWLABELS] = cs_check("Show labels",TRUE,table,0,2,0,1,labelwin_toggle_annotation,NULL);
	b = cs_button("Clear All",table,2,4,0,1,labelwin_click_clear_all,NULL);
	// -- First column
	atombtn[0] = cs_button("ID",table,0,1,1,2,labelwin_click_add_annotation,NULL);
	atombtn[1] = cs_button("Element",table,0,1,2,3,labelwin_click_add_annotation,NULL);
	atombtn[2] = cs_button("FF Type",table,0,1,3,4,labelwin_click_add_annotation,NULL);
	// -- Second column
	atombtn[3] = cs_button("FF Equiv",table,2,3,1,2,labelwin_click_add_annotation,NULL);
	atombtn[4] = cs_button("Charge",table,2,3,2,3,labelwin_click_add_annotation,NULL);
	// -- 'Clear' buttons
	for (n=0; n<AL_NITEMS; n++)
		if (n < 3) clearbtn[n] = cs_button("Clr",table,1,2,n+1,n+2,labelwin_click_clear_annotation,NULL);
		else clearbtn[n] = cs_button("Clr",table,3,4,n-2,n-1,labelwin_click_clear_annotation,NULL);
	// ---- Adjustment for label scaling
	cs_label("Size",table,0,1,4,5);
	adj = cs_adjustment(prefs.get_label_scale(),1.0,10.0,0.1,0.1);
	scale[W_SCL_LABELSCALE] = cs_scale(adj,table,1,4,4,5,labelwin_change_labelscale,NULL);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,labelwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::labelwin_create");
}
