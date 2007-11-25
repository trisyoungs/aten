/*
	*** GTK average molecule window
	*** src/gui-gtk/analyse_avgmol.cpp
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
#include "base/master.h"
#include <gtk/gtk.h>
#include "gui/gui.h"

// Variables
GtkWindow *win_avgmol;

void avgmolwin_show(GtkMenuItem *widget, gpointer data)
{
	cs_show(win_avgmol);
}

gint avgmolwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(win_avgmol);
	gui.refresh();
	return TRUE;
}

void avgmolwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(win_avgmol);
	gui.refresh();
}

void avgmolwin_create()
{
	// Create the Average Molecule calculator window
	dbg_begin(DM_CALLS,"avgmolwin_create");
	GObject *tobj;
	GtkCellRenderer *rend;
	GtkButton *button;
	GtkFrame *frame;
	GtkBox *box, *mainbox;
	// Create the window and register some callbacks.
	win_avgmol = cs_subwindow("New Window",FALSE,avgmolwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,win_avgmol);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	button = cs_button("Close",box,CS_END,FALSE,TRUE,0,avgmolwin_click_closebtn,NULL);

	cs_showall(win_avgmol);
	cs_hide(win_avgmol);
	dbg_end(DM_CALLS,"avgmolwin_create");
}
