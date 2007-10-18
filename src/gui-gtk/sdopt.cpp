/*
	*** GTK steepest-descent window
	*** src/gui-gtk/sdopt.cpp

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
#include "gui-gtk/gui-gtk.h"
#include <gtk/gtk.h>
#include "base/master.h"
#include "methods/sd.h"

// Variables
GtkSpinButton *sd_maxiter_spin, *sd_maxstep_spin, *sd_maxline_spin;

gint sdoptwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_SDOPT]);
	gui.refresh();
	return TRUE;
}

void sdoptwin_click_closebtn(GtkButton *widget, gpointer data)
{
	sdoptwin_close(NULL,NULL,NULL);
}

void sdoptwin_spin_changed(GtkSpinButton *widget, gpointer data)
{
	// One of the spin edits has been changed
	if (widget == sd_maxstep_spin)
		sd.set_maxstep(gtk_spin_button_get_value(sd_maxstep_spin));
	if (widget == sd_maxiter_spin)
		sd.set_maxiterations(gtk_spin_button_get_value_as_int(sd_maxiter_spin));
	if (widget == sd_maxline_spin)
		sd.set_maxlinetrials(gtk_spin_button_get_value_as_int(sd_maxline_spin));
}

void gui_gtk::sdoptwin_create()
{
	// Create the optline window
	dbg_begin(DM_CALLS,"gui_gtk::sdoptwin_create");
	GObject *tobj;
	GtkTable *table;
	GtkFrame *frame;
	GtkBox *box, *mainbox;
	GtkButton *b;
	GtkObject *adj;

	int n;
	// Create the window and register some callbacks.
	windows[GW_SDOPT] = cs_subwindow("Steepest Descent",TRUE,sdoptwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_SDOPT]);

	// Move Types, Probabilities, Step sizes and frequencies
	frame = cs_frame("Options",mainbox,CS_START,FALSE,FALSE,2);
	table = cs_table(4,2,FALSE,frame);
	cs_label("Max Step (A)",table,0,1,0,1);
	cs_label("Max Iterations",table,0,1,1,2);
	cs_label("Max Line Trials",table,0,1,2,3);
	adj = cs_adjustment(sd.get_maxstep(),0.1,1.0,0.1,0.1);
	sd_maxstep_spin = cs_spin(adj,10,2,table,1,2,0,1,sdoptwin_spin_changed,NULL);
	adj = cs_adjustment(sd.get_maxiterations(),1,10000,100,1000);
	sd_maxiter_spin = cs_spin(adj,100,0,table,1,2,1,2,sdoptwin_spin_changed,NULL);
	adj = cs_adjustment(sd.get_maxlinetrials(),1,100,5,10);
	sd_maxline_spin = cs_spin(adj,5,0,table,1,2,2,3,sdoptwin_spin_changed,NULL);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,sdoptwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::sdoptwin_create");
}
