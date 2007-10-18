/*
	*** GTK treeview wrapper
	*** src/gui-gtk/treeview.cpp

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
#include "gui-gtk/treeview.h"
#include "base/master.h"
#include <gtk/gtk.h>
#include <iostream>
#include "classes/pattern.h"
#include "classes/forcefield.h"

// Constructor
treeview_list::treeview_list()
{
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_TREEVIEW] ++;
	#endif
}

// Destructor
treeview_list::~treeview_list()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_TREEVIEW] ++;
	#endif
}

// Initialise
void treeview_list::initialise(const char *s, char *cols)
{
	dbg_begin(DM_CALLS,"treeview_list::initialise");
	// First, form a GType list of the column types
	int ncols = strlen(cols);
	//printf("NCOLS = %i, %s\n",ncols,cols);
	GType types[ncols];
	for (int n=0; n<ncols; n++)
	{
		switch (cols[n])
		{
			case ('i') : types[n] = G_TYPE_INT; break;
			case ('s') : types[n] = G_TYPE_STRING; break;
			case ('p') : types[n] = G_TYPE_POINTER; break;
			case ('x') : types[n] = GDK_TYPE_PIXBUF; break;
			case ('d') : types[n] = G_TYPE_DOUBLE; break;
			default : printf("cs_treeview: Unrecognised type '%c' in type string.\n",cols[n]);
		}
	}
	strcpy(name,s);
	treeview = gtk_tree_view_new();
	liststore = gtk_list_store_newv(ncols,types);
	// Associate the liststore model to the treeview component
	gtk_tree_view_set_model(GTK_TREE_VIEW(treeview),GTK_TREE_MODEL(liststore));
	g_object_unref(liststore);    // Free our hands of the liststore - GTK will keep track
	// Acquire the selection and model pointers for future reference
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	treemodel = gtk_tree_view_get_model(GTK_TREE_VIEW(treeview));
	dbg_end(DM_CALLS,"treeview_list::initialise");
}

void treeview_list::initialise_link(GtkListStore *ls,const char *s)
{
	dbg_begin(DM_CALLS,"treeview_list::initialise_link");
	strcpy(name,s);
	treeview = gtk_tree_view_new();
	liststore = ls;
	// Associate the liststore model to the treeview component
	gtk_tree_view_set_model(GTK_TREE_VIEW(treeview),GTK_TREE_MODEL(liststore));
	g_object_unref(liststore);    // Free our hands of the liststore - GTK will keep track
	// Acquire the selection and model pointers for future reference
	selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(treeview));
	treemodel = gtk_tree_view_get_model(GTK_TREE_VIEW(treeview));
	dbg_end(DM_CALLS,"treeview_list::initialise_link");
}

/*
// Row operations
*/

void treeview_list::clear()
{
	// Clear all rows
	dbg_begin(DM_CALLS,"treeview_list::clear");
	gtk_list_store_clear(liststore);
	dbg_end(DM_CALLS,"treeview_list::clear");
}

void treeview_list::append(GtkTreeIter* iter)
{
	// Add an empty row to the model.
	dbg_begin(DM_CALLS,"treeview_list::append");
	//gtk_list_store_append(GTK_LIST_STORE(treemodel),iter);
	gtk_list_store_append(liststore,iter);
	dbg_end(DM_CALLS,"treeview_list::append");
}

void treeview_list::remove(GtkTreeIter *iter)
{
	dbg_begin(DM_CALLS,"treeview_list::remove");
	gtk_list_store_remove(liststore,iter);
	dbg_end(DM_CALLS,"treeview_list::remove");
}

void treeview_list::set(GtkTreeIter *iter, ...)
{
	// Set the data in the specified iteration
	dbg_begin(DM_CALLS,"treeview_list::set");
	va_list arguments;
	va_start(arguments,iter);
	gtk_list_store_set_valist(liststore,iter,arguments);
	va_end(arguments);
	dbg_end(DM_CALLS,"treeview_list::set");
}

void treeview_list::append_and_set(GtkTreeIter* iter, ...)
{
	// Add an empty row to the model and set its data
	dbg_begin(DM_CALLS,"treeview_list::append_and_set");
	va_list arguments;
	va_start(arguments,iter);
	gtk_list_store_append(liststore,iter);
	gtk_list_store_set_valist(liststore,iter,arguments);
	va_end(arguments);
	dbg_end(DM_CALLS,"treeview_list::append_and_set");
}

/*
// Iter search routines
*/

bool treeview_list::get_first(GtkTreeIter *iter)
{
	if (!gui.exists()) return FALSE;
	dbg_begin(DM_CALLS,"treeview_list::get_first");
	if (gtk_tree_model_get_iter_first(treemodel,iter))
	{
		dbg_end(DM_CALLS,"treeview_list::get_first");
		return TRUE;
	}
	else
	{
		//printf("get_first : Couldn't get first iter in model (%s).\n",name.c_str());
		dbg_end(DM_CALLS,"treeview_list::get_first failed (%s)");
		return FALSE;
	}
}

bool treeview_list::get_next(GtkTreeIter *iter)
{
	return gtk_tree_model_iter_next(treemodel,iter);
}


bool treeview_list::find_iter(GtkTreeIter *iter, int column, gpointer data)
{
	// Find the row that has a value in 'column' matching 'data'
	dbg_begin(DM_CALLS,"treeview_list::find_iter");
	if (!get_first(iter))
	{
		dbg_end(DM_CALLS,"treeview_list::find_iter");
		return FALSE;
	}
	int done = FALSE;
	gpointer *m = NULL;
	while (!done)
	{
		gtk_tree_model_get(treemodel,iter,column,&m,-1);
		m == data ? done = TRUE
			  : done = !gtk_tree_model_iter_next(treemodel,iter);
	}
	if (m == NULL)
	{
		printf("find_iter : Couldn't find specified row in list (%s).\n",name);
		dbg_end(DM_CALLS,"treeview_list::find_iter");
		return FALSE;
	}
	dbg_end(DM_CALLS,"treeview_list::find_iter");
	return TRUE;
}

/*
// Selection operations
*/

int treeview_list::nselected()
{
	return gtk_tree_selection_count_selected_rows(selection);
}

bool treeview_list::is_selected(GtkTreeIter *iter)
{
	return gtk_tree_selection_iter_is_selected(selection,iter);
}

void treeview_list::select(GtkTreeIter *iter)
{
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"treeview_list::select");
	gtk_tree_selection_select_iter(selection, iter);
	dbg_end(DM_CALLS,"treeview_list::select");
}

void treeview_list::unselect(GtkTreeIter *iter)
{
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"treeview_list::unselect");
	gtk_tree_selection_unselect_iter(selection, iter);
	dbg_end(DM_CALLS,"treeview_list::unselect");
}

void treeview_list::set_multiple()
{
	dbg_begin(DM_CALLS,"treeview_list::set_multiple");
	gtk_tree_selection_set_mode(selection,GTK_SELECTION_MULTIPLE);
	dbg_end(DM_CALLS,"treeview_list::set_multiple");
}

bool treeview_list::select(int column, gpointer data)
{
	// Activate the row that has a value in 'column' matching 'data'
	if (!gui.exists()) return FALSE;
	dbg_begin(DM_CALLS,"treeview_list::select[int,gpointer]");
	GtkTreeIter iter;
	if (!find_iter(&iter,column,data))
	{
		printf("set_first : Couldn't get first iter in model (%s).\n",name);
		dbg_end(DM_CALLS,"treeview_list::select[int,gpointer]");
		return FALSE;
	}
	gtk_tree_selection_select_iter(selection,&iter);
	dbg_end(DM_CALLS,"treeview_list::select[int,gpointer]");
	return TRUE;
}

void treeview_list::get(GtkTreeIter *iter, int column, gpointer data)
{
	// Get information from the specified iter
	dbg_begin(DM_CALLS,"treeview_list::get");
	gtk_tree_model_get(treemodel,iter,column,data,-1);
	dbg_end(DM_CALLS,"treeview_list::get");
}

bool treeview_list::get_selected(GtkTreeIter *iter)
{
	return gtk_tree_selection_get_selected(selection,NULL,iter);
}

/*
// Misc
*/

void treeview_list::set_size(int w, int h)
{
	dbg_begin(DM_CALLS,"treeview_list::set_size");
	gtk_widget_set_size_request(treeview,w,h);
	dbg_end(DM_CALLS,"treeview_list::set_size");
}

/*
// List Master Routines
*/

treeview_list *treeview_master::add_child()
{
	// Create new treeview in list
	treeview_list *newtree = children.add();
	char newname[128] = "";
	strcat(newname,name);
	strcat(newname,"_child");
	strcat(newname,itoa(children.size()));
	// Link this new treeview to the master treeview
	newtree->initialise_link(liststore,newname);
	return newtree;
}

// Add item
void treeview_master::add_item(gpointer data)
{
	// Add the specified item to the list, calling the virtual set_iter function
	if (!gui.exists()) return;
	dbg_begin(DM_CALLS,"treeview_master::add_item");
	GtkTreeIter iter;
	append(&iter);
	set_iter(&iter,data);
	//gtk_tree_selection_select_iter(selection,&iter);
	dbg_end(DM_CALLS,"treeview_master::add_item");
}

// Update  data in list
void treeview_master::update()
{
	// Update the model data in the model lists
	dbg_begin(DM_CALLS,"treeview_master::update");
	GtkTreeIter iter;
	gpointer data;
	if (!get_first(&iter)) return;
	do
	{
		get(&iter,ML_COL_MODEL,&data); 
		set_iter(&iter,data);
	} while (get_next(&iter) == TRUE);
	//set_active(pointercol,master.get_currentmodel());
	dbg_end(DM_CALLS,"treeview_master::update");
}

/*
// Model master routines
*/

// Set data in model iter
void model_master::set_iter(GtkTreeIter *iter, gpointer data)
{
	dbg_begin(DM_CALLS,"model_master::set_iter");
	model *m = (model*) data;
	set(iter,ML_COL_NAME,m->get_name(),ML_COL_MODEL,m,-1);
	// Assign icons here...
	m->cell.get_type() != CT_NONE
		? set(iter,ML_COL_CELLFLAG,gui.get_xpm(XPM_CELL),-1)
		: set(iter,ML_COL_CELLFLAG,NULL,-1);
	m->is_modified()
		? set(iter,ML_COL_MODFLAG,gui.get_xpm(XPM_EXCLAIM),-1)
		: set(iter,ML_COL_MODFLAG,NULL,-1);
	dbg_end(DM_CALLS,"model_master::set_iter");
}

// Clear and refresh list
void model_master::refresh()
{
	// Clear and update the hidden master list
	dbg_begin(DM_CALLS,"model_master::refresh");
	clear();
	// Add items 
	model *m = master.get_models();
	while (m != NULL)
	{
		add_item(m);
		m = m->next;
	}
	dbg_end(DM_CALLS,"model_master::refresh");
}

/*
// Pattern master routines
*/

// Refresh
void pattern_master::refresh()
{
	// Clear and update the hidden master pattern list
	dbg_begin(DM_CALLS,"pattern_master::refresh");
	clear();
	// Add items 
	pattern *p = master.get_currentmodel()->get_patterns();
	while (p != NULL)
	{
		add_item(p);
		p = p->next;
	}
	dbg_end(DM_CALLS,"pattern_master::refresh");
}

// Set data in pattern iter
void pattern_master::set_iter(GtkTreeIter *iter, gpointer data)
{
	dbg_begin(DM_CALLS,"pattern_master::set_iter");
	pattern *p = (pattern*) data;
	set(iter,PL_COL_NAME,p->get_name(),PL_COL_PATTERN,p,-1);
	dbg_end(DM_CALLS,"pattern_master::set_iter");
}

/*
// FF master routines
*/

// Refresh
void ff_master::refresh()
{
	// Clear and update the hidden master pattern list
	dbg_begin(DM_CALLS,"ff_master::refresh");
	clear();
	// Add items 
	for (forcefield *ff = master.get_ffs(); ff != NULL; ff = ff->next)
		add_item(ff);
	dbg_end(DM_CALLS,"ff_master::refresh");
}

// Set data in pattern iter
void ff_master::set_iter(GtkTreeIter *iter, gpointer data)
{
	dbg_begin(DM_CALLS,"ff_master::set_iter");
	forcefield *ff = (forcefield*) data;
	set(iter,FL_COL_NAME,ff->get_name(),FL_COL_FF,ff,-1);
	dbg_end(DM_CALLS,"ff_master::set_iter");
}

