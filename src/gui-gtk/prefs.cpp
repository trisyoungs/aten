/*
	*** GTK prefs window
	*** src/gui-gtk/prefs.cpp
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

gint prefswin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_PREFS]);
	gui.refresh();
	return TRUE;
}

void prefswin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(gui.windows[GW_PREFS]);
	gui.refresh();
}

void gui_gtk::prefswin_create()
{
	// Create the preferences window
	dbg_begin(DM_CALLS,"gui_gtk::presfwin_create");
	GtkButton *b;
	GtkBox *mainbox, *box;
	GtkNotebook *notebook;
	// Create the window and register some callbacks.
	windows[GW_PREFS] = cs_subwindow("Preferences",FALSE,prefswin_close);

	// Vbox for the tab control and buttons
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_PREFS]);	// All same width, 2 pixel spacing
	// Create the notebook
	notebook = cs_notebook(mainbox,CS_START,TRUE,TRUE,0);
	// ** General page
	prefswin_create_general(notebook);

	// ** Render page
	prefswin_create_render(notebook);

	// ** Control page
	prefswin_create_control(notebook);

	// ** View pags
	prefswin_create_view(notebook);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,prefswin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::presfwin_create");
}
