/*
	*** GTK periodic table window
	*** src/gui-gtk/ptable.cpp

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
#include "base/sysfunc.h"
#include <gtk/gtk.h>
#include "base/elements.h"
#include "base/master.h"

// Variables
GtkButton **el_btn;

gint ptwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_PERIODICTABLE]);
	gui.refresh();
	return TRUE;
}

void ptwin_click_closebtn(GtkButton *widget, gpointer data)
{
	ptwin_close(NULL,NULL,NULL);
}

void ptwin_click_el_btn(GtkButton *widget, gpointer data)
{
	// Get the element that was selected, and return it to the stored routine.
}

void gui_gtk::ptwin_create()
{
	// Create the Periodic Table window
	dbg_begin(DM_CALLS,"gui_gtk::ptwin_create");
	GObject *tobj;
	GtkCellRenderer *rend;
	GtkBox *box, *mainbox;
	GtkButton *b;
	GtkTable *table;
	int n, m, z;
	el_btn = new GtkButton*[NELEMENTS+1];
	// Create the window and register some callbacks.
	windows[GW_PERIODICTABLE] = cs_subwindow("Periodic Table",FALSE,ptwin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_PERIODICTABLE]);

	// A table contains all the buttons for the elements
	table = cs_table(11,19,TRUE,mainbox,CS_START,TRUE,TRUE,0);

	// First row - Group Number
	for (n=1; n<19; n++) cs_label(itoa(n),table,n,n+1,0,1);
		
	// First column - Period number
	for (n=1; n<8; n++) cs_label(itoa(n),table,0,1,n,n+1);
		
	cs_label("LN",table,0,1,9,10);
	cs_label("AN",table,0,1,10,11);

	// H, He
	el_btn[1] = cs_button("H",table,1,2,1,2,ptwin_click_el_btn,NULL);
	el_btn[2] = cs_button("He",table,18,19,1,2,ptwin_click_el_btn,NULL);

	// Groups 1-2 (periods 1-6) [s]
	z = 3;
	for (n=0; n<6; n++)
	{
		el_btn[z] = cs_button(elements.symbol(z),table,1,2,n+2,n+3,ptwin_click_el_btn,NULL);
		el_btn[z+1] = cs_button(elements.symbol(z+1),table,2,3,n+2,n+3,ptwin_click_el_btn,NULL);
		z += 8;
		if (n > 1) z += 10;
		if (n > 3) z += 14;
	}

	// Groups 13-18 (periods 1-6) [p] 
	z = 5;
	for (n=0; n<6; n++)
	{
		for (m=0; m<6; m++)
			el_btn[z+m] = cs_button(elements.symbol(z+m),table,13+m,14+m,n+2,n+3,ptwin_click_el_btn,NULL);
		z += 8;
		if (n > 0) z += 10; 
		if (n > 2) z += 14;
	}

	// Groups 3-8 (periods 3-6) [p] 
	z = 21;
	for (n=0; n<4; n++)
	{
		for (m=0; m<10; m++)
			el_btn[z+m] = cs_button(elements.symbol(z+m),table,3+m,4+m,n+4,n+5,ptwin_click_el_btn,NULL);
		if (n == 1) z += 14;
		z += 18; 
		if (n > 1) z += 14;
	}

	// Lanthanoids and Actinoids
	z = 57;
	for (n=0; n<14; n++)
	{
		el_btn[z+n] = cs_button(elements.symbol(z+n),table,3+n,4+n,9,10,ptwin_click_el_btn,NULL);
		el_btn[z+n+32] = cs_button(elements.symbol(z+n+32),table,3+n,4+n,10,11,ptwin_click_el_btn,NULL);
	}

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,ptwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::ptwin_create");
}
