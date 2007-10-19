/*
	*** GTK Monte Carlo options window
	*** src/gui-gtk/mcopt.cpp
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

#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include <gtk/gtk.h>
#include "base/master.h"
#include "methods/mc.h"

// Variables
GtkCheckButton *move_check[MT_NITEMS];
GtkSpinButton *prob_spin[MT_NITEMS], *step_spin[MT_NITEMS], *trials_spin[MT_NITEMS];

gint mcoptwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_MCOPT]);
	gui.refresh();
	return TRUE;
}

void mcoptwin_click_closebtn(GtkButton *widget, gpointer data)
{
	mcoptwin_close(NULL,NULL,NULL);
}

void mcoptwin_use_changed(GtkCheckButton *widget, gpointer data)
{
	// Check buttons indicating allowable moves have changed
	if (!gui.exists()) return;
	for (int n=0; n<MT_NITEMS; n++) mc.set_allowed((mc_move) n, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(move_check[n])));
}

void mcoptwin_step_changed(GtkSpinButton *widget, gpointer data)
{
	// Maximum step size spin buttons have changed
	for (int n=0; n<MT_NITEMS; n++) mc.set_maxstep((mc_move) n, gtk_spin_button_get_value(step_spin[n]));
}

void mcoptwin_trials_changed(GtkSpinButton *widget, gpointer data)
{
	// NTrials spin buttons have changed
	for (int n=0; n<MT_NITEMS; n++) mc.set_ntrials((mc_move) n, gtk_spin_button_get_value_as_int(trials_spin[n]));
}

void gui_gtk::mcoptwin_create()
{
	// Create the optmc window
	dbg_begin(DM_CALLS,"gui_gtk::mcoptwin_create");
	GObject *tobj;
	GtkButton *b;
	GtkTable *table;
	GtkFrame *frame;
	GtkBox *box, *mainbox;
	GtkObject *adj;
	char *text;
	int n;
	// Create the window and register some callbacks.
	windows[GW_MCOPT] = cs_subwindow("Monte Carlo Options",TRUE,mcoptwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_MCOPT]);

	// Move Types, Probabilities, Step sizes and frequencies
	frame = cs_frame("Move Types",mainbox,CS_START,TRUE,TRUE,2);
	table = cs_table(MT_NITEMS+1,6,FALSE,frame);
	cs_label("Probability",table,1,2,0,1);
	cs_label("Step Size",table,2,3,0,1);
	cs_label("Trials",table,4,5,0,1);
	for (n=0; n<MT_NITEMS; n++)
	{
		move_check[n] = cs_check(text_from_MT((mc_move) n), mc.get_allowed((mc_move) n), table, 0, 1, 1+n, 2+n, mcoptwin_use_changed,NULL);
		//adj = cs_adjustment(mc.move_probs[n],0.0,1.0,0.1,0.1);
		//prob_spin[n] = cs_spin(adj,0.1,2,table,1,2,1+n,2+n,mcoptwin_prob_changed,NULL);
		adj = cs_adjustment(mc.get_maxstep((mc_move) n),0.0,10.0,0.1,0.1);
		step_spin[n] = cs_spin(adj,0.1,2,table,2,3,1+n,2+n,mcoptwin_step_changed,NULL);
		if (n == MT_ROTATE) text = "Degrees";
		else if (n >= MT_INSERT) text = "";
		else text = "Angstroms";
		cs_label(text,table,3,4,1+n,2+n);
		adj = cs_adjustment(mc.get_ntrials((mc_move) n),1,1000,10,100);
		trials_spin[n] = cs_spin(adj,10,0,table,4,5,1+n,2+n,mcoptwin_trials_changed,NULL);
	}
	
	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,mcoptwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::mcoptwin_create");
}
