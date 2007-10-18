/*
	*** GTK disordered builder window
	*** src/gui-gtk/disbuild.cpp

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
#include "gui-gtk/treeview.h"
#include <gtk/gtk.h>
#include "base/master.h"
#include "model/model.h"
#include "methods/mc.h"
#include "classes/component.h"

// Variables
component *CURRENTCOMP = NULL;
treeview_list *mcfill_mlist, mcfill_clist, mcfill_clist2;
GtkComboBox *mcfill_regioncombo;
GtkCheckButton *mcfill_usecellcentre, *mcfill_translate, *mcfill_rotate, *mcfill_zmatrix;
GtkSpinButton *mcfill_centre[3], *mcfill_size[3], *mcfill_attempts[4], *mcfill_nmols, *mcfill_ncycles;

gint disbuildwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_MCINSERT]);
	prefs.set_visible(VO_REGIONS, FALSE);
	gui.refresh();
	return TRUE;
}

void disbuildwin_click_okbtn()
{
	disbuildwin_close(NULL,NULL,NULL);
}

void disbuildwin_list_delete_current()
{
	// Remove the current selected component from the clist	
	dbg_begin(DM_CALLS,"disbuildwin_list_delete_current");
	GtkTreeIter selected_row;
	component *selcomp;
	if (mcfill_clist.get_selected(&selected_row))
	{
		mcfill_clist.get(&selected_row, CL_COL_COMP, &selcomp);
		mcfill_clist.remove(&selected_row);
		mc.remove_component(selcomp);
	}
	dbg_end(DM_CALLS,"disbuildwin_list_delete_current");
}

void disbuildwin_list_add(component *xcomp)
{
	// Add the data for a component to the component listview
	// Return the created row to store in component->clist_iter
	dbg_begin(DM_CALLS,"disbuildwin_list_add");
	GtkTreeIter iter;
	mcfill_clist.append_and_set(&iter,CL_COL_NAME,xcomp->get_model()->get_name(),
		CL_COL_COMP,xcomp,-1);
	dbg_end(DM_CALLS,"disbuildwin_list_add");
}

void disbuildwin_list_update()
{
	// Update the component data in the mcfill_clist.
	dbg_begin(DM_CALLS,"gui::disbuildwin_list_update");
	component *c;
	GtkTreeIter iter;
	// Must empty the list and build it again.
	mcfill_clist.clear();
	c = mc.get_components();
	while (c != NULL)
	{
		disbuildwin_list_add(c);
		c = c->next;
	}
	dbg_end(DM_CALLS,"gui::disbuildwin_list_update");
}

void disbuildwin_controls_update()
{
	// Update the controls reflecting the currently selected component
	static vec3<double> centre, size;
	if (CURRENTCOMP != NULL)
	{
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_nmols),CURRENTCOMP->get_nrequested());
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mcfill_translate),CURRENTCOMP->get_allowed(MT_TRANSLATE));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mcfill_rotate),CURRENTCOMP->get_allowed(MT_ROTATE));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mcfill_zmatrix),CURRENTCOMP->get_allowed(MT_ZMATRIX));
		centre = CURRENTCOMP->area.get_centre();
		size = CURRENTCOMP->area.get_size();
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_size[0]),size.x);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_size[1]),size.y);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_size[2]),size.z);
		gtk_combo_box_set_active(GTK_COMBO_BOX(mcfill_regioncombo),CURRENTCOMP->area.get_shape());
		if (CURRENTCOMP->area.get_usecellcentre())
		{
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mcfill_usecellcentre),TRUE);
			//mat3<double> craptemp;
			vec3<double> tempv;
			tempv.set(0.5,0.5,0.5);
			//craptemp = cell->get_axes();
			tempv *= master.get_currentmodel()->cell.get_axes();
			CURRENTCOMP->area.set_centre(tempv);
			//*v *= craptemp;
		}
		else gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(mcfill_usecellcentre),FALSE);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_centre[0]),centre.x);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_centre[1]),centre.y);
		gtk_spin_button_set_value(GTK_SPIN_BUTTON(mcfill_centre[2]),centre.z);
	}
}

void disbuildwin_update()
{
	// Update controls on model change
	disbuildwin_list_update();
}

void disbuildwin_click_addbtn(GtkButton *widget, gpointer data)
{
	// Add a component to the component list of the master.activemodel. Source is the model selected in the mlist.
	dbg_begin(DM_CALLS,"disbuildwin_click_addbtn");
	GtkTreeIter sel;
	model *xmodel;
	component *comp;
	// Make sure a model is selected in the mlist.
	if (mcfill_mlist->get_selected(&sel))
	{
		// 'sel' now holds the information of the selected row.
		mcfill_mlist->get(&sel, ML_COL_MODEL, &xmodel);
		comp = mc.add_component();
		comp->set_model(xmodel);
		// Add this new element to the clist treeview
		disbuildwin_list_add(comp);
	}
	dbg_end(DM_CALLS,"disbuildwin_click_addbtn");
}

void disbuildwin_click_delbtn(GtkButton *widget, gpointer data)
{
}

void disbuildwin_click_upbtn(GtkButton *widget, gpointer data)
{
}

void disbuildwin_click_downbtn(GtkButton *widget, gpointer data)
{
}

void mcfill_click_nmolschange(GtkSpinButton *widget, gpointer data)
{
	if (CURRENTCOMP != NULL) CURRENTCOMP->set_nrequested(gtk_spin_button_get_value_as_int(widget));
}

void mcfill_click_translate(GtkCheckButton *widget, gpointer data)
{
	if (CURRENTCOMP != NULL) CURRENTCOMP->set_allowed(MT_TRANSLATE,gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void mcfill_click_rotate(GtkCheckButton *widget, gpointer data)
{
	if (CURRENTCOMP != NULL) CURRENTCOMP->set_allowed(MT_ROTATE,gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void mcfill_click_zmatrix(GtkCheckButton *widget, gpointer data)
{
	if (CURRENTCOMP != NULL) CURRENTCOMP->set_allowed(MT_ZMATRIX,gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void mcfill_change_region(GtkComboBox *widget, gpointer data)
{
	if (CURRENTCOMP == NULL) return;
	region_shape temp = (region_shape)gtk_combo_box_get_active(GTK_COMBO_BOX(widget));
	CURRENTCOMP->area.set_shape(temp);
	gui.refresh();
}

void mcfill_change_centre(GtkSpinButton *widget, gpointer data)
{
	if (CURRENTCOMP == NULL) return;
	vec3<double> oldpos = CURRENTCOMP->area.get_centre();
	if (widget == GTK_SPIN_BUTTON(mcfill_centre[0])) oldpos.set(0,gtk_spin_button_get_value(widget));
	else if (widget == GTK_SPIN_BUTTON(mcfill_centre[1])) oldpos.set(1,gtk_spin_button_get_value(widget));
	else if (widget == GTK_SPIN_BUTTON(mcfill_centre[2])) oldpos.set(2,gtk_spin_button_get_value(widget));
	CURRENTCOMP->area.set_centre(oldpos);
	gui.refresh();
}

void mcfill_change_size(GtkSpinButton *widget, gpointer data)
{
	if (CURRENTCOMP == NULL) return;
	vec3<double> oldpos = CURRENTCOMP->area.get_size();
	if (widget == GTK_SPIN_BUTTON(mcfill_size[0])) oldpos.set(0,gtk_spin_button_get_value(widget));
	else if (widget == GTK_SPIN_BUTTON(mcfill_size[1])) oldpos.set(1,gtk_spin_button_get_value(widget));
	else if (widget == GTK_SPIN_BUTTON(mcfill_size[2])) oldpos.set(2,gtk_spin_button_get_value(widget));
	CURRENTCOMP->area.set_size(oldpos);
	gui.refresh();
}

void mcfill_click_usecellcentre(GtkCheckButton *widget, gpointer data)
{
	if (CURRENTCOMP != NULL) CURRENTCOMP->area.set_usecellcentre(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void mcfill_click_mcoptions(GtkButton *widget, gpointer data)
{
	cs_show(gui.windows[GW_MCINSERT]);
}

void mcfill_click_run(GtkButton *widget, gpointer data)
{
	// Grab the 'cycles' value from the widget, since this is a 'shared' control option
	if (master.get_currentmodel()->cell.get_type() == CT_NONE)
		msg(DM_NONE,"Monte Carlo insertion cannot be performed on a model without a unit cell!\n");
	else
	{
		mc.set_ncycles(gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(mcfill_ncycles)));
		// TODO Read energy acceptance criterion (eaccept) from GUI
		mc.disorder(master.get_currentmodel());
	}
}

void disbuildwin_change_clist(GtkTreeSelection *selection, gpointer data)
{
	// Called when a different component is selected in the clist
	dbg_begin(DM_CALLS,"disbuildwin_change_clist");
	GtkTreeModel *active_model;
	GtkTreeIter selected_row;
	if (mcfill_clist2.get_selected(&selected_row))
		mcfill_clist2.get(&selected_row, CL_COL_COMP, &CURRENTCOMP);
	// Set controls in the window
	disbuildwin_controls_update();
	dbg_end(DM_CALLS,"disbuildwin_change_clist");
}

gboolean disbuildwin_mlist_visiblefunc(GtkTreeModel *tmodel, GtkTreeIter *iter, gpointer data)
{
	// Extract the model pointer from the iter
	model *m;
	gtk_tree_model_get(tmodel,iter,ML_COL_MODEL,&m,-1);
	if (m == NULL) return FALSE;
	if (m->cell.get_type() == CT_NONE) return TRUE;
	return FALSE;
}

void gui_gtk::disbuildwin_create_components(GtkNotebook *notebook)
{
	GtkFrame *frame;
	GtkBox *mainbox, *page, *box;
	GtkScrolledWindow *w;
	GtkCellRenderer *rend;
	GtkButton *b;

	// Create the page
	page = cs_notebookpage("Elements",notebook);
	// Create a vbox to put all the frames into...
	mainbox = cs_box(CS_HORIZONTAL,FALSE,0,page);

	// Create the available model list first.
	// This list displays all models suitable for insertion (i.e. those that are non-periodic)
	frame = cs_frame("Available Models",mainbox,CS_START,TRUE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,2,frame);
	// Create the model list
	mcfill_mlist = modelmaster.add_child();	
	w = cs_treeview(mcfill_mlist,box,CS_START,TRUE,TRUE,0,NULL);
	rend = cs_column(mcfill_mlist,"Model","text",ML_COL_NAME);
	// Now, in order to exclude the periodic models in the original model_list we create a new GtkTreeModelFilter using
	// this original model as the child. Then, we replace the reference in commp_mlist.model to point to this. The filtering
	// function will use the model's cell_type as the visibility indicator.
	mcfill_mlist->set_treemodel(gtk_tree_model_filter_new(mainwin_modellist->get_treemodel(),NULL));
	gtk_tree_view_set_model(mcfill_mlist->get_treeview(),mcfill_mlist->get_treemodel());
	gtk_tree_model_filter_set_visible_func(GTK_TREE_MODEL_FILTER(mcfill_mlist->get_treemodel()),disbuildwin_mlist_visiblefunc,NULL,NULL);
	mcfill_mlist->set_size(150,150);
	// Buttons for the model selector
	box = cs_box(CS_VERTICAL,FALSE,2,box,CS_START,FALSE,TRUE,0);
	b = cs_pixbutton(xpms[XPM_ADD],box,CS_START,FALSE,FALSE,0,disbuildwin_click_addbtn,NULL);

	// Second frame - the list of components selected for insertion
	frame = cs_frame("Components",mainbox,CS_END,TRUE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,2,frame);

	// Create the cell component treeview and liststore
	mcfill_clist.initialise("complist","sp");
	cs_treeview(&mcfill_clist,box,CS_START,TRUE,TRUE,0,NULL);
	// Add the columns
	rend = cs_column(&mcfill_clist,"Model","text",CL_COL_NAME);
	mcfill_clist.set_size(150,150);

	// Buttons and their button boxes for both listviews
	box = cs_box(CS_VERTICAL,FALSE,2,box,CS_END,FALSE,TRUE,0);
	b = cs_pixbutton(xpms[XPM_REMOVE],box,CS_START,FALSE,FALSE,0,disbuildwin_click_delbtn,NULL);
	b = cs_pixbutton(xpms[XPM_EXCLAIM],box,CS_START,FALSE,FALSE,0,disbuildwin_click_upbtn,NULL);
	b = cs_pixbutton(xpms[XPM_EXCLAIM],box,CS_START,FALSE,FALSE,0,disbuildwin_click_downbtn,NULL);
}

void gui_gtk::disbuildwin_create_method(GtkNotebook *notebook)
{
	GtkFrame *frame, *frame2;
	GtkBox *mainbox, *page, *box, *subbox;
	GtkTable *table;
	GtkButton *b;
	GtkObject *adj;
	GtkCellRenderer *rend;
	// Create the method page
	page = cs_notebookpage("Method",notebook);
	// Create a vbox to put all the frames into...
	mainbox = cs_box(CS_VERTICAL,FALSE,0,page);

	// Composition editing
	frame = cs_frame("Composition",mainbox,CS_START,TRUE,TRUE,0);
	subbox = cs_box(CS_HORIZONTAL,FALSE,2,frame);

	// The list of components selected for insertion
	box = cs_box(CS_HORIZONTAL,FALSE,2,subbox,CS_START,TRUE,TRUE,0);
	// -- Create the cell component treeview and liststore
	mcfill_clist2.initialise_link(mcfill_clist.get_liststore(),"mcfill_modellist");
	cs_treeview(&mcfill_clist2,box,CS_START,TRUE,TRUE,0,disbuildwin_change_clist);
	// -- Add the columns
	rend = cs_column(&mcfill_clist2,"Model","text",CL_COL_NAME);
	mcfill_clist.set_size(150,150);

	// Table of edit widgets for the component
	table = cs_table(4,4,FALSE,subbox,CS_END,TRUE,FALSE,0);
	frame2 = cs_frame("Molecules",table,0,1,0,1);
	adj = cs_adjustment(1.0,0.0,100000,1,10);
	mcfill_nmols = cs_spin(adj,10.0,0,GTK_CONTAINER(frame2),mcfill_click_nmolschange,NULL);
	frame2 = cs_frame("Allowed Moves",table,0,1,1,2);
	subbox = cs_box(CS_VERTICAL,FALSE,2,frame2);
	mcfill_translate = cs_check("Translate",TRUE,subbox,CS_START,FALSE,TRUE,0,mcfill_click_translate,NULL);
	mcfill_rotate = cs_check("Rotate",TRUE,subbox,CS_START,FALSE,TRUE,0,mcfill_click_rotate,NULL);
	mcfill_zmatrix = cs_check("Z-Matrix",TRUE,subbox,CS_START,FALSE,TRUE,0,mcfill_click_zmatrix,NULL);
	frame2 = cs_frame("Region",table,1,2,0,2);
	table = cs_table(7,2,FALSE,frame2);
	mcfill_regioncombo = cs_combo(get_RS_strings(),RS_NITEMS,0,table,0,3,0,1,mcfill_change_region);
	cs_label("Centre",table,1,2,1,2);
	cs_label("Size",table,2,3,1,2);
	cs_label("X",table,0,1,2,3);
	cs_label("Y",table,0,1,3,4);
	cs_label("Z",table,0,1,4,5);
	mcfill_usecellcentre = cs_check("Use Cell",TRUE,table,1,2,5,6,mcfill_click_usecellcentre,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_centre[0] = cs_spin(adj,10.0,2,table,1,2,2,3,mcfill_change_centre,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_centre[1] = cs_spin(adj,10.0,2,table,1,2,3,4,mcfill_change_centre,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_centre[2] = cs_spin(adj,10.0,2,table,1,2,4,5,mcfill_change_centre,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_size[0] = cs_spin(adj,10.0,2,table,2,3,2,3,mcfill_change_size,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_size[1] = cs_spin(adj,10.0,2,table,2,3,3,4,mcfill_change_size,NULL);
	adj = cs_adjustment(5.0,0.0,10000.0,1,10);
	mcfill_size[2] = cs_spin(adj,10.0,2,table,2,3,4,5,mcfill_change_size,NULL);

	// MC Strategy frame
	frame = cs_frame("Strategy",mainbox,CS_START,TRUE,TRUE,0);
	subbox = cs_box(CS_HORIZONTAL,FALSE,2,frame);
	table = cs_table(3,2,FALSE,subbox,CS_START,TRUE,FALSE,0);
	cs_label("Cycles",table,0,1,0,1);
	adj = cs_adjustment(100,0.0,100000,1,10);
	mcfill_ncycles = cs_spin(adj,10.0,0,table,1,2,0,1,NULL,NULL);
	//cs_label_table("Temperature",table,0,1,1,2);
	//adj = cs_adjustment(300,1.0,100000,1,10);
	//w = cs_spin_table(adj,10.0,0,table,1,2,1,2,NULL);
	b = cs_button("Options",table,2,3,0,1,mcfill_click_mcoptions,NULL);
	b = cs_button("Run",table,2,3,2,3,mcfill_click_run,NULL);
}

void gui_gtk::disbuildwin_create()
{
	// Create the Monte Carlo Insertion window
	dbg_begin(DM_CALLS,"gui_gtk::disbuildwin_create");
	GObject *tobj;
	GtkBox *mainbox;
	GtkNotebook *notebook;
	GtkCellRenderer *rend;
	// Create the window and register some callbacks.
	windows[GW_MCINSERT] = cs_subwindow("Disordered Builder",FALSE,disbuildwin_close);

	// Main box for window
	mainbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(windows[GW_MCINSERT]));

	// A notebook control will split up the window
	notebook = cs_notebook(mainbox,CS_START,TRUE,TRUE,0);
	// ** Component list page
	disbuildwin_create_components(notebook);
	// ** Method
	disbuildwin_create_method(notebook);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::disbuildwin_create");
}

