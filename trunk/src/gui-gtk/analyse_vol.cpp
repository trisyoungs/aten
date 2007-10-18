/*
	*** GTK volume analyse window
	*** src/gui-gtk/analyse_vol.cpp

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

#include "gui/gui.h"
#include "gui-gtk/gui-gtk.h"
#include "gui-gtk/funcs.h"
#include "base/master.h"
#include <gtk/gtk.h>

// Variables
GtkWindow *win_freevol;

void volwin_show(GtkMenuItem *widget, gpointer data)
{
	cs_show(win_freevol);
}

gint volwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(win_freevol);
	gui.refresh();
	return TRUE;
}

void volwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(win_freevol);
	gui.refresh();
}

void gui_gtk::volwin_create()
{
	// Create the Free Volume calculation window
	dbg_begin(DM_CALLS,"gui_gtk::volwin_create");
	GObject *tobj;
	GtkCellRenderer *rend;
	GtkBox *box, *mainbox;
	GtkFrame *frame;
	GtkButton *button;
	// Create the window and register some callbacks.
	win_freevol = cs_subwindow("New Window",FALSE,volwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,win_freevol);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	button = cs_button("Close",box,CS_END,FALSE,TRUE,0,volwin_click_closebtn,NULL);

	cs_showall(win_freevol);
	cs_hide(win_freevol);
	dbg_end(DM_CALLS,"gui_gtk::volwin_create");
}
