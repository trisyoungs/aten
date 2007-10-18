/*
	*** GTK atomtype selector window
	*** src/gui-gtk/atselect.cpp

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

#include "gui-gtk/gui-gtk.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/treeview.h"
#include <gtk/gtk.h>
#include "model/model.h"
#include "classes/pattern.h"
#include "classes/atomtype.h"
#include "base/sysfunc.h"
#include "base/master.h"
#include "base/elements.h"

// Variables
GtkEntry *ats_entry, *el_entry;
GtkLabel *score_lbl, *nmatched_lbl;
bool additive_select = FALSE;

gint atswin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_ATOMSELECT]);
	return TRUE;
}

void atswin_click_closebtn(GtkButton *widget, gpointer data)
{
	atswin_close(NULL,NULL,NULL);
}

void atswin_apply_clicked(GtkButton *widget, gpointer data)
{
	// Test out the type description supplied
	dbg_begin(DM_CALLS,"atswin_apply_clicked");
	int count, el, matchscore, atomscore, n;
	// Deselect all atoms if not doing a cumulative selection
	if (!additive_select) master.get_currentmodel()->select_none();
	// Get the target element first.
	el = elements.find(gtk_entry_get_text(GTK_ENTRY(el_entry)),ZM_ALPHA);
	if (el != 0) 
	{
		// Create the atomtype
		atomtype *testat = new atomtype();
		testat->el = el;
		testat->expand(gtk_entry_get_text(GTK_ENTRY(ats_entry)),NULL);
		// Apply it to the atoms in the model, selecting atoms that match
		model *m = master.get_currentmodel();
		count = 0;
		matchscore = 0;
		if (m->autocreate_patterns())
		{
			// Prepare for typing
			m->describe_atoms();
			// Loop over patterns and select atoms
			pattern *p = m->get_patterns();
			while (p != NULL)
			{
				atom *i = p->get_firstatom();
				for (n=0; n<p->get_natoms(); n++)
				{
					p->reset_tempi(0);
					i->tempi = 1;
					if (i->get_element() == testat->el)
					{
						atomscore = testat->match_atom(i,p->get_ringlist(),m);
						if (atomscore != 0)
						{
							m->select_atom(i);
							count ++;
							matchscore = atomscore;
						}
					}
					i = i->next;
				}
				p = p->next;
			}
			// Change labels in window
			gtk_label_set_text(GTK_LABEL(score_lbl),itoa(matchscore));
			gtk_label_set_text(GTK_LABEL(nmatched_lbl),itoa(count));
			// Update model and delete temporary atomtype
			m->log_change(LOG_SELECTION);
			delete testat;
		}
		else msg(DM_NONE,"Can't test atomtype description without a valid pattern definition!\n");
	}
	gui.refresh();
	dbg_end(DM_CALLS,"atswin_apply_clicked");
}

void atswin_atentry_changed(GtkEntry *widget, gpointer data)
{
}

void atswin_elentry_changed(GtkEntry *widget, gpointer data)
{
}

void atswin_clicked_addselect(GtkCheckButton *widget, gpointer data)
{
	additive_select = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}

void gui_gtk::atomselectwin_create()
{
	// Create the Atomtype selector window
	dbg_begin(DM_CALLS,"gui_gtk::atomselectwin_create");
	GObject *tobj;
	GtkCellRenderer *rend;
	GtkTable *table;
	GtkBox *box, *mainbox;
	GtkFrame *frame;
	GtkButton *b;
	GtkLabel *l;
	GtkCheckButton *c;
	// Create the window and register some callbacks.
	windows[GW_ATOMSELECT] = cs_subwindow("Atomtype Selector",FALSE,atswin_close);

	// Main box for whole window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_ATOMSELECT]);

	// Atomtype description entry frame
	frame = cs_frame("Type Definition",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(2,3,FALSE,frame);
	// -- Entries for atomtype definition string
	l = cs_label("Element",table,0,1,0,1);
	l = cs_label("Atomtype",table,0,1,1,2);
	el_entry = cs_entry(table,1,2,0,1,atswin_elentry_changed);
	ats_entry = cs_entry(table,1,3,1,2,atswin_atentry_changed);
	b = cs_button("Apply",table,2,3,0,1,atswin_apply_clicked,NULL);

	// Score frame
	frame = cs_frame("Match",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(2,2,TRUE,frame);
	// -- Data labels
	l = cs_label("Type Score :",table,0,1,0,1);
	l = cs_label("Atoms Matched : ",table,0,1,1,2);
	score_lbl = cs_label("0",table,1,2,0,1);
	nmatched_lbl = cs_label("0",table,1,2,1,2);

	// Checkbox to allow for cumulative selection
	c = cs_check("Additive selections", FALSE, mainbox, CS_START, FALSE, FALSE, 0,atswin_clicked_addselect,NULL);

	// Buttons at foot of window (Close etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,atswin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::atomselectwin_create");
}
