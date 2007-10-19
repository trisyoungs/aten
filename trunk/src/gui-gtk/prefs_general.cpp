/*
	*** GTK general prefs window
	*** src/gui-gtk/prefs_general.cpp
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

void prefswin_mupdate_changed(GtkSpinButton *widget, gpointer data)
{
	prefs.set_mupdate(gtk_spin_button_get_value_as_int(widget));
}

void prefswin_eupdate_changed(GtkSpinButton *widget, gpointer data)
{
	prefs.set_eupdate(gtk_spin_button_get_value_as_int(widget));
}

void prefswin_changed_font(GtkFontButton *widget, gpointer data)
{
	prefs.set_msg_font(gtk_font_button_get_font_name(widget));
	gui.change_msg_font(prefs.get_msg_font());
}

void prefswin_timerevents_changed(GtkSpinButton *widget, gpointer data)
{
	master.set_timer_period(gtk_spin_button_get_value_as_int(widget));
}

void prefswin_timerscale_changed(GtkSpinButton *widget, gpointer data)
{
	master.set_timer_eventsize(gtk_spin_button_get_value_as_int(widget));
}

void prefswin_changed_usetimer(GtkCheckButton *widget, gpointer data)
{
	master.use_timer = (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)) ? TRUE : FALSE);
	master.use_timer ? master.start_timer_events() : master.stop_timer_events();
}

void gui_gtk::prefswin_create_general(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::prefswin_create_general");
	GtkBox *page, *mainbox, *box;
	GtkFrame *frame;
	GtkTable *table;
	GtkObject *adj;
	GtkSpinButton *s;
	GtkWidget *w;
	// Create the page
	page = cs_notebookpage("General",notebook);
	// Create a vbox to put all the frames into...
	mainbox = cs_box(CS_VERTICAL,FALSE,0,page);

	// Feedback settings
	frame = cs_frame("Feedback",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(2,2,FALSE,frame);
	cs_label("Model update frequency",table,0,1,0,1);
	adj = cs_adjustment(prefs.get_mupdate(),0,100,1,10);
	s = cs_spin(adj,1,0,table,1,2,0,1,prefswin_mupdate_changed,NULL);
	cs_label("Energy output frequency",table,0,1,1,2);
	adj = cs_adjustment(prefs.get_eupdate(),1,100,1,10);
	s = cs_spin(adj,1,0,table,1,2,1,2,prefswin_eupdate_changed,NULL);

	// Font selectors
	frame = cs_frame("Fonts",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(2,2,FALSE,frame);
	cs_label("Messages",table,0,1,0,1);
	w = gtk_font_button_new();
	g_signal_connect(w,"font-set",G_CALLBACK(prefswin_changed_font),NULL);
	gtk_table_attach_defaults(GTK_TABLE(table),w,1,2,0,1);

	// Internal timer
	frame = cs_frame("Timer",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(3,2,FALSE,frame);
	check[W_CHK_TIMERACTIVE] = cs_check("Use timer events", master.use_timer, table,0,1,0,1,prefswin_changed_usetimer,NULL);
	cs_label("Events per second",table,0,1,1,2);
	adj = cs_adjustment(master.get_timer_period(),1,60,1,5);
	spin[W_SPN_TIMERPERIOD] = cs_spin(adj,1,0,table,1,2,1,2,prefswin_timerevents_changed,NULL);
	cs_label("Length scale",table,0,1,2,3);
	adj = cs_adjustment(master.get_timer_eventsize(),5,100,1,10);
	spin[W_SPN_TIMERSIZE] = cs_spin(adj,1,0,table,1,2,2,3,prefswin_timerscale_changed,NULL);
	
	dbg_end(DM_CALLS,"gui_gtk::prefswin_create_general");
}
