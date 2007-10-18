/*
	*** GTK forcefield manager window
	*** src/gui-gtk/ffmanager.cpp

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
#include "gui-gtk/treeview.h"
#include "gui-gtk/gui-gtk.h"
#include <gtk/gtk.h>
#include "model/model.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"
#include "base/elements.h"
#include "base/sysfunc.h"
#include "base/master.h"

// Variables
GtkLabel *lbl_ffname, *lbl_ffdesc, *lbl_ntypes, *lbl_path;
GtkLabel *lbl_b_ndata, *lbl_a_ndata, *lbl_t_ndata, *lbl_nb_ndata;
treeview_list ff_blist, ff_alist, ff_tlist, ff_nblist;

gint ffwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	//return FALSE;	   // FALSE = do NOT destroy window. TRUE = DO destroy window.
	cs_hide(gui.windows[GW_FORCEFIELD]);
	gui.refresh();
	return TRUE;
}

void ffwin_click_closebtn(GtkButton *widget, gpointer data)
{
	ffwin_close(NULL,NULL,NULL);
}

/*
// Data Lists
*/

void ffwin_datalists_update(forcefield *xff)
{
	// Update all the information about the current forcefield
	dbg_begin(DM_CALLS,"ffwin_datalists_update");
	GtkTreeIter iter;
	char lbl[64];
	int count;
	ffatom *ffa;
	ffbound *ffb;
	ffparams params;
	// First, clear all listboxes.
	ff_nblist.clear();
	ff_blist.clear();
	ff_alist.clear();
	ff_tlist.clear();
	// Return here if no forcefields are currently loaded
	if (xff == NULL)
	{
		dbg_end(DM_CALLS,"ffwin_datalists_update");
		return;
	}
	// Fill VDW types list
	// Range is from 0..natomtypes inclusive, since zero is the NULL type
	for (ffa = xff->get_atomtypes(); ffa != NULL; ffa = ffa->next)
	{
		params = ffa->get_params();
		if (xff->get_rules() == FFR_NORULES)
			ff_nblist.append_and_set(&iter,FLNB_COL_N,ffa->get_ffid(),
				FLNB_COL_I,ffa->get_name(),
				FLNB_COL_Q,ffa->get_charge(),
				FLNB_COL_FORM,text_from_VF(ffa->get_style()),
				FLNB_COL_DATA1,params.data[0],
				FLNB_COL_DATA2,params.data[1],
				FLNB_COL_DATA3,params.data[2],-1);
		else	// TODO Info for rule-based ffs - different listview?
			ff_nblist.append_and_set(&iter,FLNB_COL_N,ffa->get_ffid(),
				FLNB_COL_I,ffa->get_name(),-1);
	}
	strcpy(lbl,"Definitions : ");
	strcat(lbl,itoa(xff->get_natomtypes()));
	gtk_label_set_text(GTK_LABEL(lbl_nb_ndata), lbl);
	// Fill Bond types list
	count = 0;
	for (ffb = xff->get_bonds(); ffb != NULL; ffb = ffb->next)
	{
		count ++;
		params = ffb->get_params();
		ff_blist.append_and_set(&iter,FLB_COL_N,count,
			FLB_COL_I,ffb->get_type(0),
			FLB_COL_J,ffb->get_type(1),
			FLB_COL_FORM,text_from_BF(ffb->get_funcform().bondfunc),
			FLB_COL_DATA1,params.data[0],
			FLB_COL_DATA2,params.data[1],-1);
	}
	strcpy(lbl,"Definitions : ");
	strcat(lbl,itoa(count));
	gtk_label_set_text(GTK_LABEL(lbl_b_ndata),lbl);
	// Fill Angle types list
	count = 0;
	for (ffb = xff->get_angles(); ffb != NULL; ffb = ffb->next)
	{
		count ++;
		params = ffb->get_params();
		ff_alist.append_and_set(&iter,FLA_COL_N,count,
			FLA_COL_I,ffb->get_type(0),
			FLA_COL_J,ffb->get_type(1),
			FLA_COL_K,ffb->get_type(2),
			FLA_COL_FORM,text_from_AF(ffb->get_funcform().anglefunc),
			FLA_COL_DATA1,params.data[0],
			FLA_COL_DATA2,params.data[1],-1);
	}
	strcpy(lbl,"Definitions : ");
	strcat(lbl,itoa(count));
	gtk_label_set_text(GTK_LABEL(lbl_a_ndata),lbl);
	// Fill Torsion types list
	count = 0;
	for (ffb = xff->get_torsions(); ffb != NULL; ffb = ffb->next)
	{
		count ++;
		params = ffb->get_params();
		ff_tlist.append_and_set(&iter,FLT_COL_N,count,
			FLT_COL_I,ffb->get_type(0),
			FLT_COL_J,ffb->get_type(1),
			FLT_COL_K,ffb->get_type(2),
			FLT_COL_L,ffb->get_type(3),
			FLT_COL_FORM,text_from_TF(ffb->get_funcform().torsionfunc),
			FLT_COL_DATA1,params.data[0],
			FLT_COL_DATA2,params.data[1],
			FLT_COL_DATA3,params.data[2],
			FLT_COL_DATA4,params.data[3],
			FLT_COL_ESCALE,params.data[TF_ESCALE],
			FLT_COL_VSCALE,params.data[TF_VSCALE],-1);
	}
	strcpy(lbl,"Definitions : ");
	strcat(lbl,itoa(count));
	gtk_label_set_text(GTK_LABEL(lbl_t_ndata),lbl);
	dbg_end(DM_CALLS,"ffwin_datalists_update");
}

/*
// Button Click Handlers
*/

void ffwin_showloadffwin(GtkButton *widget, gpointer data)
{
	gui.call_loadff_dialog();
	if (master.get_currentff() != NULL) gui.ffwin_fflist->select(FL_COL_FF,master.get_currentff());
}

void ffwin_click_unload(GtkButton *widget, gpointer data)
{
	master.remove_ff(master.get_currentff());
}

void ffwin_click_associate(GtkButton *widget, gpointer data)
{
	// Associate the current forcefield to the current model
	master.get_currentmodel()->set_ff(master.get_currentff());
}

void ffwin_click_associate_all(GtkButton *widget, gpointer data)
{
	// Associate the current forcefield to all models
	model *m = master.get_models();
	while (m != NULL)
	{
		m->set_ff(master.get_currentff());
		m = m->next;
	}
}

void ffwin_click_fftype_current(GtkButton *widget, gpointer data)
{
	// Type the current model with the model's current forcefield
	master.get_currentmodel()->type_all();
}

void ffwin_change_fflist(GtkTreeSelection *selection, gpointer data)
{
	// Called when a different ffield is selected in the list
	dbg_begin(DM_CALLS,"ffwin_change_fflist");
	GtkTreeIter selected_row;
	GtkTreeModel *active_model;
	forcefield *ff = NULL;
	if (gtk_tree_selection_get_selected(selection,&active_model,&selected_row))
		gtk_tree_model_get(active_model, &selected_row, FL_COL_FF, &ff, -1);;
	master.set_currentff(ff);
	ffwin_datalists_update(master.get_currentff());
        dbg_end(DM_CALLS,"ffwin_change_fflist");
}

void gui_gtk::ffwin_create()
{
	// Create the Forcefield Display window
	dbg_begin(DM_CALLS,"gui_gtk::ffwin_create");
	GObject *tobj;
	GtkButton *b;
	GtkFrame *frame;
	GtkNotebook *notebook;
	GtkTable *table;
	GtkCellRenderer *rend;
	GtkBox *mainbox, *vbox, *hbox, *box, *page;
	GtkLabel *l;

	// Create the window
	windows[GW_FORCEFIELD] = cs_subwindow("Forcefield Manager",FALSE,ffwin_close);

	// Main box for window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(windows[GW_FORCEFIELD]));

	// Forcefield list and information is first
	frame = cs_frame("Available Forcefields",mainbox,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,frame);

	// Create child FF list
	ffwin_fflist = ffmaster.add_child();
	cs_treeview(ffwin_fflist,box,CS_START,TRUE,TRUE,0,ffwin_change_fflist);
	rend = cs_column(ffwin_fflist,"A","pixbuf",FL_COL_AUTOLOAD);
	rend = cs_column(ffwin_fflist,"Name","text",FL_COL_NAME);
	ffwin_fflist->set_size(350,100);

	// Buttons and their button box
	vbox = cs_box(CS_VERTICAL,FALSE,2,box,CS_START,TRUE,TRUE,0);
	b = cs_button("Load",vbox,CS_START,FALSE,FALSE,0,ffwin_showloadffwin,NULL);
	b = cs_button("Unload",vbox,CS_START,FALSE,FALSE,0,ffwin_click_unload,NULL);

	// Next, a Notebook displaying the terms in the selected forcefield
	notebook = cs_notebook(mainbox, CS_START, TRUE, TRUE, 0);

	// Add the notebook pages...
	// First page - non-bonded terms
	page = cs_notebookpage("Non-Bonded Terms",notebook);
	vbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));
	lbl_nb_ndata = cs_label("Definitions : 0",vbox,CS_START,TRUE,TRUE,0);
	// Create treeview to list the nonbond data
	ff_nblist.initialise("ff_nonbondlist","isdsddd");
	cs_treeview(&ff_nblist,vbox,CS_START,TRUE,TRUE,0,NULL);
	// Add the columns
	rend = cs_column(&ff_nblist,"ID","text",FLNB_COL_N);
	rend = cs_column(&ff_nblist,"FFType","text",FLNB_COL_I);
	rend = cs_column(&ff_nblist,"Charge","text",FLNB_COL_Q);
	rend = cs_column(&ff_nblist,"Form","text",FLNB_COL_FORM);
	rend = cs_column(&ff_nblist,"Data1","text",FLNB_COL_DATA1);
	rend = cs_column(&ff_nblist,"Data2","text",FLNB_COL_DATA2);
	rend = cs_column(&ff_nblist,"Data3","text",FLNB_COL_DATA3);
	ff_nblist.set_size(200,100);

	// Second page - bond data
	page = cs_notebookpage("Bond Terms",notebook);
	vbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));
	lbl_b_ndata = cs_label("Definitions : 0",vbox,CS_START,TRUE,TRUE,0);
	// Create  treeview to list the bond data
	ff_blist.initialise("ff_bondlist","isssdd");
	cs_treeview(&ff_blist,vbox,CS_START,TRUE,TRUE,0,NULL);
	// Add the columns
	rend = cs_column(&ff_blist,"ID","text",FLB_COL_N);
	rend = cs_column(&ff_blist,"I","text",FLB_COL_I);
	rend = cs_column(&ff_blist,"J","text",FLB_COL_J);
	rend = cs_column(&ff_blist,"Form","text",FLB_COL_FORM);
	rend = cs_column(&ff_blist,"Data1","text",FLB_COL_DATA1);
	rend = cs_column(&ff_blist,"Data2","text",FLB_COL_DATA2);
	ff_blist.set_size(200,100);

	// Third page - angle data
	page = cs_notebookpage("Angle Terms",notebook);
	vbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));
	lbl_a_ndata = cs_label("Definitions : 0",vbox,CS_START,TRUE,TRUE,0);
	// Create  treeview to list the angle data
	ff_alist.initialise("ff_anglelist","issssdd");
	cs_treeview(&ff_alist,vbox,CS_START,TRUE,TRUE,0,NULL);
	// Add the columns
	rend = cs_column(&ff_alist,"ID","text",FLA_COL_N);
	rend = cs_column(&ff_alist,"I","text",FLA_COL_I);
	rend = cs_column(&ff_alist,"J","text",FLA_COL_J);
	rend = cs_column(&ff_alist,"K","text",FLA_COL_K);
	rend = cs_column(&ff_alist,"Form","text",FLA_COL_FORM);
	rend = cs_column(&ff_alist,"Data1","text",FLA_COL_DATA1);
	rend = cs_column(&ff_alist,"Data2","text",FLA_COL_DATA2);
	ff_alist.set_size(200,100);

	// Fourth page - torsion data
	page = cs_notebookpage("Torsion Terms",notebook);
	vbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));
	lbl_t_ndata = cs_label("Definitions : 0",vbox,CS_START,TRUE,TRUE,0);
	// Create  treeview to list the torsion data
	ff_tlist.initialise("ff_torslist","isssssdddddd");
	cs_treeview(&ff_tlist,vbox,CS_START,TRUE,TRUE,0,NULL);
	// Add the columns
	rend = cs_column(&ff_tlist,"ID","text",FLT_COL_N);
	rend = cs_column(&ff_tlist,"I","text",FLT_COL_I);
	rend = cs_column(&ff_tlist,"J","text",FLT_COL_J);
	rend = cs_column(&ff_tlist,"K","text",FLT_COL_K);
	rend = cs_column(&ff_tlist,"L","text",FLT_COL_L);
	rend = cs_column(&ff_tlist,"Form","text",FLT_COL_FORM);
	rend = cs_column(&ff_tlist,"Data1","text",FLT_COL_DATA1);
	rend = cs_column(&ff_tlist,"Data2","text",FLT_COL_DATA2);
	rend = cs_column(&ff_tlist,"Data3","text",FLT_COL_DATA3);
	rend = cs_column(&ff_tlist,"Data4","text",FLT_COL_DATA4);
	rend = cs_column(&ff_tlist,"1-4 Elec","text",FLT_COL_ESCALE);
	rend = cs_column(&ff_tlist,"1-4 VDW","text",FLT_COL_VSCALE);
	ff_tlist.set_size(200,100);

	// Button box below notebook
	frame = cs_frame("Model Actions",mainbox,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,2,frame);
	b = cs_button("Associate FF",box,CS_END,FALSE,FALSE,0,ffwin_click_associate,NULL);
	b = cs_button("Associate FF to All",box,CS_END,FALSE,FALSE,0,ffwin_click_associate_all,NULL);
	b = cs_button("FF Type",box,CS_END,FALSE,FALSE,0,ffwin_click_fftype_current,NULL);
	//b = cs_button("FF",box,CS_END,FALSE,FALSE,0,ffwin_click_associate,NULL);


	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	box = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",box,CS_END,FALSE,TRUE,0,ffwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::ffwin_create");
}
