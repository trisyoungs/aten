/*
	*** GTK atom list window
	*** src/gui-gtk/atomlist.cpp
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
#include "model/model.h"
#include "classes/forcefield.h"
#include "base/master.h"
#include "base/sysfunc.h"
#include "base/elements.h"
#include "gui-gtk/gui-gtk.h"
#include "gui-gtk/funcs.h"

// Variables
GtkWidget *atomwin_hidden;
GtkSpinButton *atomwin_charge;
GtkComboBox *atomwin_style;

// Local variables which hold the atom properties to modify
draw_style new_ds;
int new_element;
double new_charge;
bool new_hidden, UPDATING = FALSE;
int atomwin_logs[LOG_NITEMS];

/*
// List Operations
*/
void gui_gtk::atomwin_list_update()
{
	dbg_begin(DM_CALLS,"gui::atomwin_list_update");
	forcefield *ff = master.get_currentmodel()->get_ff();
	UPDATING = TRUE;
	atom *i;
	GtkTreeIter iter;
	if (atomwin_atomlist.get_first(&iter))
	{
		do
		{
			// Grab the pointer to the atom from the current iter and put it into 'i'
			atomwin_atomlist.get(&iter, AL_COL_ATOM, gpointer(&i));
			// Set the atom selection flag to mirror the list selection
			atomwin_atomlist.set(&iter,AL_COL_ID,i->get_id(),AL_COL_ELEMENT,elements.symbol(i),
			AL_COL_POSX,i->r.x,
			AL_COL_POSY,i->r.y,
			AL_COL_POSZ,i->r.z,
			AL_COL_FF,(i->fftype_is(NULL) ? "None" : i->get_fftype()->get_equiv()),
			AL_COL_DS,xpms[XPM_DS_STICK+i->get_style()],
			AL_COL_NBONDS,i->get_nbonds(),
			AL_COL_Q,i->get_charge(),
			AL_COL_ATOM,i,-1);
			if (i->is_hidden()) atomwin_atomlist.set(&iter,AL_COL_HIDE,"Y",-1);
			i->is_selected() ? atomwin_atomlist.select(&iter) : atomwin_atomlist.unselect(&iter);
		} while (atomwin_atomlist.get_next(&iter));
	}
	UPDATING = FALSE;
	dbg_end(DM_CALLS,"gui::atomwin_list_update");
}

void gui_gtk::atomwin_list_update_selection()
{
	dbg_begin(DM_CALLS,"gui::atomwin_list_update_selection");
	UPDATING = TRUE;
	atom *i;
	GtkTreeIter iter;
	if (atomwin_atomlist.get_first(&iter))
	{
		do
		{
			// Grab the pointer to the atom from the current iter and put it into 'i'
			atomwin_atomlist.get(&iter, AL_COL_ATOM, gpointer(&i));
			i->is_selected() ? atomwin_atomlist.select(&iter) : atomwin_atomlist.unselect(&iter);
		} while (atomwin_atomlist.get_next(&iter));
	}
	UPDATING = FALSE;
	dbg_end(DM_CALLS,"gui::atomwin_list_update_selection");
}

void gui_gtk::atomwin_list_delete_selection()
{
	dbg_begin(DM_CALLS,"gui::atomwin_list_update_selection");
	UPDATING = TRUE;
	atom *i;
	GtkTreeIter iter;
	if (atomwin_atomlist.get_first(&iter))
	{
		do
		{
			// Grab the pointer to the atom from the current iter and put it into 'i'
			atomwin_atomlist.get(&iter, AL_COL_ATOM, gpointer(&i));
			if (i->is_selected()) atomwin_atomlist.remove(&iter);
		} while (atomwin_atomlist.get_next(&iter));
	}
	UPDATING = FALSE;
	dbg_end(DM_CALLS,"gui::atomwin_list_update_selection");
}

void gui_gtk::atomwin_list_repopulate()
{
	// Clear and repopulate the atom list from scratch
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"gui::atomwin_list_repopulate");
	GtkTreeIter iter;
	atomwin_atomlist.clear();
	forcefield *ff = master.get_currentmodel()->get_ff();
	atom *i = master.get_currentmodel()->get_atoms();
	while (i != NULL)
	{
		atomwin_atomlist.append_and_set(&iter,AL_COL_ID,i->get_id(),AL_COL_ELEMENT,elements.symbol(i),
			AL_COL_POSX,i->r.x,AL_COL_POSY,i->r.y,AL_COL_POSZ,i->r.z,
			AL_COL_FF,(i->fftype_is(NULL) ? "None" : i->get_fftype()->get_equiv()),
			AL_COL_DS,xpms[XPM_DS_STICK+i->get_style()],AL_COL_NBONDS,i->get_nbonds(),AL_COL_Q,i->get_charge(),
			AL_COL_ATOM,i,-1);
		if (i->is_hidden()) atomwin_atomlist.set(&iter,AL_COL_HIDE,"Y",-1);
		i->is_selected() ? atomwin_atomlist.select(&iter) : atomwin_atomlist.unselect(&iter);
		i = i->next;
	}
	dbg_end(DM_CALLS,"gui::atomwin_list_repopulate");
}

void gui_gtk::atomwin_list_updateselected(col_atomlist column)
{
	// Change the data of selected rows in the specified column the atom list to reflect the (recently-changed) atom data
	dbg_begin(DM_CALLS,"gui::atomwin_list_updateselected");
	atom *i;
	GtkTreeIter iter;
	if (atomwin_atomlist.get_first(&iter))
	{
		do
		{
			// If the row is selected, modify its info...
			if (atomwin_atomlist.is_selected(&iter))
			{
				atomwin_atomlist.get(&iter, AL_COL_ATOM, gpointer(&i));
				switch (column)
				{
					case (AL_COL_DS) : 	// Draw Style
						i->set_style(new_ds);
						atomwin_atomlist.set(&iter,AL_COL_DS,xpms[XPM_DS_STICK+new_ds],-1);
						break;
					case (AL_COL_HIDE) : 	// Hidden flag
						i->set_hidden(new_hidden);
						i->is_hidden() ? atomwin_atomlist.set(&iter,AL_COL_HIDE,"Y",-1)
							  : atomwin_atomlist.set(&iter,AL_COL_HIDE,"",-1);
						break;
					case (AL_COL_Q) :	// Atomic charge
						i->set_charge(new_charge);
						atomwin_atomlist.set(&iter,AL_COL_Q,new_charge,-1);
						break;
				}
			}
		} while (atomwin_atomlist.get_next(&iter));
	}
	gui.refresh();
	dbg_begin(DM_CALLS,"gui:atomwin_list_updateselected");
}

void gui_gtk::atomwin_list_refresh()
{
	// Refresh the information in the list.
	// Only update if the window is currently visible. Otherwise it should be updated after the call to show()
	if ((GTK_WIDGET_FLAGS(windows[GW_ATOMLIST])&GTK_VISIBLE) == 0) return;
	dbg_begin(DM_CALLS,"gui::atomwin_list_refresh");
	// Only change the data in the list if it is out of data / no longer relevant.
	model *m = master.get_currentmodel();
	// Major change - different model, atom deletion etc.
	if (m->get_log(LOG_STRUCTURE) != atomwin_logs[LOG_STRUCTURE])
	{
		atomwin_list_repopulate();
		atomwin_logs[LOG_STRUCTURE] = m->get_log(LOG_STRUCTURE);
	}
	else
	{
		// Minor changes - update data already in the list.
		// Actual atom data - coordinates etc.
		if (m->get_log(LOG_VISUAL) != atomwin_logs[LOG_VISUAL])
		{
			atomwin_list_update();
			atomwin_logs[LOG_VISUAL] = m->get_log(LOG_VISUAL);
		}
		// Selection data
		if (m->get_log(LOG_SELECTION) != atomwin_logs[LOG_SELECTION])
		{
			atomwin_list_update();
			atomwin_logs[LOG_SELECTION] = m->get_log(LOG_SELECTION);
		}
	}
}

void gui_gtk::atomwin_list_reset()
{
	// Resets the list variables / pointers on change of model
	for (int n=0; n<LOG_NITEMS; n++) atomwin_logs[n] = -10;
	atomwin_list_refresh();
}

/*
// GUI Functions
*/

gint atomwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_ATOMLIST]);
	gui.refresh();
	return TRUE;
}

void atomwin_click_closebtn(GtkButton *widget, gpointer data)
{
	cs_hide(gui.windows[GW_ATOMLIST]);
	gui.refresh();
}

void atomwin_displaytype_clicked(GtkRadioButton *widget, gpointer data)
{
}

void atomwin_list_changed(GtkTreeSelection *selection, gpointer data)
{
	// Called when a different selection is made in the list
	if (UPDATING) return;
	dbg_begin(DM_CALLS,"atomwin_list_changed");
	atom *i, *firstsel;
	GtkTreeIter iter;
	firstsel = NULL;
	model *m = master.get_currentmodel();
	if (gui.atomwin_atomlist.get_first(&iter))
	{
		do
		{
			gui.atomwin_atomlist.get(&iter, AL_COL_ATOM, gpointer(&i));
			// Set the atom selection flag to mirror the list selection
			gui.atomwin_atomlist.is_selected(&iter) ? m->select_atom(i) : m->deselect_atom(i);
			if ((i->is_selected()) && (firstsel == NULL)) firstsel = i;
		} while (gui.atomwin_atomlist.get_next(&iter));
	}
	gui.refresh();
	dbg_end(DM_CALLS,"atomwin_list_changed");
}

void atomwin_drawstyle_changed(GtkComboBox *widget, gpointer data)
{
	new_ds = (draw_style)gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
	gui.atomwin_list_updateselected(AL_COL_DS);
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void atomwin_charge_changed(GtkSpinButton *widget, gpointer data)
{
	new_charge = gtk_spin_button_get_value(GTK_SPIN_BUTTON(widget));
	gui.atomwin_list_updateselected(AL_COL_Q);
}

void atomwin_click_togglevisible(GtkButton *widget, gpointer data)
{
	string action = (const char*) data;
	new_hidden = (action == "hide" ? TRUE : FALSE);
	gui.atomwin_list_updateselected(AL_COL_HIDE);
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void atomwin_element_clicked(GtkButton *widget, gpointer data)
{
}

void atomwin_style_clicked(GtkButton *widget, gpointer data)
{
	new_ds = (draw_style)gtk_combo_box_get_active(GTK_COMBO_BOX(atomwin_style));
	gui.atomwin_list_updateselected(AL_COL_DS);
	master.get_currentmodel()->log_change(LOG_VISUAL);
	gui.refresh();
}

void atomwin_charge_clicked(GtkButton *widget, gpointer data)
{
	new_charge = gtk_spin_button_get_value(GTK_SPIN_BUTTON(atomwin_charge));
	gui.atomwin_list_updateselected(AL_COL_Q);
}

void atomwin_pos_edited(GtkCellRendererText *cell,gchar *path_string,gchar *new_text,gpointer user_data)
{
	// Use the supplied path to get the atom pointer
	atom *i;
	GtkTreeIter iter;
	char *data = (char*) user_data;
	int dir = data[0] - 120;		// Gives 0,1,2 for x,y,z respectively
	if (gtk_tree_model_get_iter(gui.atomwin_atomlist.get_treemodel(),&iter,gtk_tree_path_new_from_string (path_string))) 
	{
		gui.atomwin_atomlist.get(&iter,AL_COL_ATOM,gpointer(&i));
		vec3<double> oldr = i->r;
		oldr.set(dir,atof(new_text));
		i->r = oldr;
		gui.atomwin_atomlist.set(&iter,AL_COL_POSX,oldr.get(dir),-1);
		master.get_currentmodel()->log_change(LOG_VISUAL);
		master.get_currentmodel()->project_selection();
		gui.refresh();
	}
	else printf("Arse\n");
}

void atomwin_click_move(GtkButton* widget, gpointer data)
{
	string action = (const char*) data;
	if (action == "up") master.get_currentmodel()->shift_selection_up();
	else if (action == "down") master.get_currentmodel()->shift_selection_down();
	master.get_currentmodel()->log_change(LOG_STRUCTURE);
	gui.atomwin_list_refresh();
}

void gui_gtk::atomwin_create()
{
	// Create the atom properties window
	dbg_begin(DM_CALLS,"gui_gtk::atomwin_create");
	GtkTreeIter iter;
	GtkObject *adj;
	GtkScrolledWindow *scroll;
	GtkCellRenderer *rend;
	GtkFrame *frame, *frame2;
	GtkBox *box, *mainbox, *page;
	GtkTable *table;
	GtkRadioButton *radio, *radio2;
	GtkNotebook *notebook;
	GtkButton *b;
	windows[GW_ATOMLIST] = cs_subwindow("Atom Properties",FALSE,atomwin_close);

	// A single vbox will hold the controls in the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_ATOMLIST]);

	// Horizontal box for type of display
	box = cs_box(CS_HORIZONTAL,TRUE,0,mainbox,CS_START,FALSE,TRUE,0);
	radio = cs_radio(NULL,"By Atom",box,CS_START,FALSE,TRUE,0,atomwin_displaytype_clicked);
	radio2 = cs_radio(radio,"By Pattern",box,CS_START,FALSE,TRUE,0,atomwin_displaytype_clicked);

	// Atom List
	atomwin_atomlist.initialise("atomwin_atomlist","isdddsxsidp");
	scroll = cs_treeview(&atomwin_atomlist,mainbox,CS_START,TRUE,TRUE,0,atomwin_list_changed);
	atomwin_atomlist.set_multiple();
	// -- Add the columns
	rend = cs_column(&atomwin_atomlist,"ID","text",AL_COL_ID);
	rend = cs_column(&atomwin_atomlist,"El","text",AL_COL_ELEMENT);
	rend = cs_column(&atomwin_atomlist,"X","text",AL_COL_POSX);
	g_object_set(rend,"editable",TRUE,NULL);
	g_signal_connect(rend,"edited",(GCallback)atomwin_pos_edited,(gpointer)"x");
	rend = cs_column(&atomwin_atomlist,"Y","text",AL_COL_POSY);
	g_object_set(rend,"editable",TRUE,NULL);
	g_signal_connect(rend,"edited",(GCallback)atomwin_pos_edited,(gpointer)"y");
	rend = cs_column(&atomwin_atomlist,"Z","text",AL_COL_POSZ);
	g_object_set(rend,"editable",TRUE,NULL);
	g_signal_connect(rend,"edited",(GCallback)atomwin_pos_edited,(gpointer)"z");
	rend = cs_column(&atomwin_atomlist,"FF","text",AL_COL_FF);
	rend = cs_column(&atomwin_atomlist,"DS","pixbuf",AL_COL_DS);
	rend = cs_column(&atomwin_atomlist,"Hide","text",AL_COL_HIDE);
	rend = cs_column(&atomwin_atomlist,"NBonds","text",AL_COL_NBONDS);
	rend = cs_column(&atomwin_atomlist,"Charge","text",AL_COL_Q);
	atomwin_atomlist.set_size(400,300);

	// Separator
	cs_separator(CS_HORIZONTAL,mainbox,CS_START,FALSE,TRUE,0);

	// Notebook with edit / move controls
	notebook = cs_notebook(mainbox,CS_START,FALSE,TRUE,0);

	// Edit Components
	page = cs_notebookpage("Edit",notebook);
	table = cs_table(2,2,FALSE,page);
	// -- Draw style
	frame2 = cs_frame("Draw Style",table,0,1,0,1);
	box = cs_box(CS_HORIZONTAL,FALSE,0,GTK_CONTAINER(frame2));
	b = cs_button("Apply",box,CS_START,FALSE,TRUE,0,atomwin_style_clicked,NULL);
	atomwin_style = cs_combo(get_DS_strings(),DS_NITEMS,0,box,CS_END,TRUE,TRUE,0,atomwin_drawstyle_changed);
	// -- Charge
	frame2 = cs_frame("Charge",table,0,1,1,2);
	box = cs_box(CS_HORIZONTAL,FALSE,0,GTK_CONTAINER(frame2));
	b = cs_button("Apply",box,CS_START,FALSE,TRUE,0,atomwin_charge_clicked,NULL);
	adj = cs_adjustment(0.0,-5.0,5.0,0.1,1.0);
	atomwin_charge = cs_spin(adj,0.1,5,box,CS_END,TRUE,TRUE,0,atomwin_charge_changed,NULL);
	// -- Visibility
	frame2 = cs_frame("Visibility",table,1,2,0,1);
	box = cs_box(CS_HORIZONTAL,FALSE,0,GTK_CONTAINER(frame2));
	b = cs_button("Hide",box,CS_START,FALSE,TRUE,0,atomwin_click_togglevisible,"hide");
	b = cs_button("Show",box,CS_END,FALSE,TRUE,0,atomwin_click_togglevisible,"show");
	//w = cs_button_table("Element",table,0,1,0,1,atomwin_element_clicked);

	// Order Components
	page = cs_notebookpage("Order",notebook);
	table = cs_table(2,2,FALSE,page);
	b = cs_button("Shift Up",table,0,1,0,1,atomwin_click_move,"up");
	b = cs_button("Shift Down",table,0,1,1,2,atomwin_click_move,"down");
	b = cs_button("Move to start",table,1,2,0,1,atomwin_click_move,"start");
	b = cs_button("Move to end",table,1,2,1,2,atomwin_click_move,"end");

	// Separator
	cs_separator(CS_HORIZONTAL,mainbox,CS_START,FALSE,TRUE,0);
	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,atomwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::atomwin_create");
}

