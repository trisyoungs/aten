/*
	*** GTK minimiser window
	*** src/gui-gtk/minimise.cpp
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
#include "model/model.h"
#include "base/master.h"
#include "methods/mc.h"
#include "methods/sd.h"

// Minimisation algorithms
enum min_method { MM_STEEPEST, MM_NEWTON, MM_MONTECARLO, MM_SIMPLEX, MM_NITEMS };

// Variables
GtkRadioButton *method_radio[MM_NITEMS];
GtkSpinButton *deltae_spin, *deltaf_spin;
GtkCheckButton *deltae_check, *deltaf_check;
min_method selectedmethod;

gint minwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_MINIMISER]);
	gui.refresh();
	return TRUE;
}

void minwin_click_closebtn(GtkButton *widget, gpointer data)
{
	minwin_close(NULL,NULL,NULL);
}

void minwin_method_changed(GtkRadioButton *widget, gpointer data)
{
	// The method has been changed.
	dbg_begin(DM_CALLS,"minwin_method_changed");
	for (int i=0; i<MM_NITEMS; i++) if (widget == method_radio[i]) selectedmethod = (min_method) i;
	dbg_end(DM_CALLS,"minwin_method_changed");
}

void minwin_click_moptions(GtkButton *widget, gpointer data)
{
	// Show the option window for the selected method
	switch (selectedmethod)
	{
		case (MM_MONTECARLO):
			cs_show(gui.windows[GW_MCOPT]);
			break;
		case (MM_SIMPLEX): 
			break;
		case (MM_STEEPEST):
			cs_show(gui.windows[GW_SDOPT]);
			break;
		case (MM_NEWTON):
			break;
	}
}
void minwin_deltacheck_clicked(GtkCheckButton* widget, gpointer data)
{
	// Set the sensitivity of the spin widget depending on the state of the checkbox
	if (!gui.exists()) return;
	widget == deltae_check
		? gtk_widget_set_sensitive(GTK_WIDGET(deltae_spin),gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
		: gtk_widget_set_sensitive(GTK_WIDGET(deltaf_spin),gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void minwin_click_minimise(GtkButton *widget, gpointer data)
{
	// Set up the model pattern, energy expression and forcefield
	dbg_begin(DM_CALLS,"minwin_click_minimise");
	double econverge, fconverge;
	// Get the convergence values from the window controls
	!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(deltae_check)) ? econverge = 0.0
		: econverge = pow(10.0,gtk_spin_button_get_value(GTK_SPIN_BUTTON(deltae_spin)));
	!gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(deltaf_check)) ? fconverge = 0.0
		: fconverge = pow(10.0,gtk_spin_button_get_value(GTK_SPIN_BUTTON(deltaf_spin)));
	// Perform the minimisation
	switch (selectedmethod)
	{
		case (MM_MONTECARLO):
			mc.minimise(master.get_currentmodel(),econverge,fconverge);
			break;
		case (MM_SIMPLEX):
			msg(DM_NONE,"Simplex minimiser not yet written!\n");
			break;
		case (MM_STEEPEST):
			sd.minimise(master.get_currentmodel(),econverge,fconverge);
			break;
		case (MM_NEWTON):
			msg(DM_NONE,"Newton-Raphson minimiser not yet written!\n");
			break;
	}
	dbg_end(DM_CALLS,"minwin_click_minimise");
}

void gui_gtk::minwin_create()
{
	// Create the minimise window
	dbg_begin(DM_CALLS,"gui_gtk::minwin_create");
	GtkObject *adj;
	GtkWidget *w;
	GtkFrame *frame;
	GtkBox *box, *hbox, *mainbox;
	GtkButton *b;
	GtkTable *table;
	// Create the window and register some callbacks.
	windows[GW_MINIMISER] = cs_subwindow("Minimise",FALSE,minwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(windows[GW_MINIMISER]));

	// Minimisation method frame
	frame = cs_frame("Method",mainbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_VERTICAL,FALSE,2,frame);
	method_radio[MM_MONTECARLO] = cs_radio(NULL,"Monte Carlo",box,CS_START,FALSE,FALSE,0,minwin_method_changed);
	method_radio[MM_SIMPLEX] = cs_radio(method_radio[MM_MONTECARLO],"Simplex",box,CS_START,FALSE,FALSE,0,minwin_method_changed);
	gtk_widget_set_sensitive(GTK_WIDGET(method_radio[MM_SIMPLEX]),FALSE);
	method_radio[MM_STEEPEST] = cs_radio(method_radio[MM_MONTECARLO],"Steepest Descent",box,CS_START,FALSE,FALSE,0,minwin_method_changed);
	method_radio[MM_NEWTON] = cs_radio(method_radio[MM_MONTECARLO],"Newton-Raphson",box,CS_START,FALSE,FALSE,0,minwin_method_changed);
	selectedmethod = MM_MONTECARLO;
	gtk_widget_set_sensitive(GTK_WIDGET(method_radio[MM_NEWTON]),FALSE);

	// -- Hbox containing options button
	hbox = cs_box(CS_HORIZONTAL,FALSE,0,mainbox,CS_START,FALSE,TRUE,0);
	b = cs_button("Minimise",hbox,CS_START,TRUE,TRUE,0,minwin_click_minimise,NULL);
	b = cs_button("Options",hbox,CS_END,TRUE,TRUE,0,minwin_click_moptions,NULL);

	// Convergence criteria frame
	frame = cs_frame("Convergence",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(2,4,FALSE,frame);
	deltae_check = cs_check("Delta E",TRUE,table,0,1,0,1,minwin_deltacheck_clicked,NULL);
	deltaf_check = cs_check("Delta RMS Force",FALSE,table,0,1,1,2,minwin_deltacheck_clicked,NULL);
	cs_label("10E",table,1,2,0,1);
	cs_label("10E",table,1,2,1,2);
	adj = cs_adjustment(-4.0,-10.0,-1.0,1.0,1.0);
	deltae_spin = cs_spin(adj,1.0,0,table,2,3,0,1,NULL,NULL);
	adj = cs_adjustment(-4.0,-10.0,-1.0,1.0,1.0);
	deltaf_spin = cs_spin(adj,1.0,0,table,2,3,1,2,NULL,NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(deltaf_spin),FALSE);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,minwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::minwin_create");
}
