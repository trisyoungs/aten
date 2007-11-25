/*
	*** GTK treeview wrapper
	*** src/gui-gtk/treeview.h
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

#ifndef H_TVIEW_H
#define H_TVIEW_H

#include <string>
#include <gtk/gtk.h>
#include "templates/list.h"

// Forward declarations
class model;
class pattern;

// General treeview list
class treeview_list
{
	public:
	// Constructor / Destructor
	treeview_list();
	~treeview_list();

	/*
	// GTK
	*/
	protected:
	// Defines the data model (a list) 
	GtkListStore *liststore;
	// Model interface
	GtkTreeModel *treemodel;
	// The actual treeview widget
	GtkWidget *treeview;
	// Selection info
	GtkTreeSelection *selection;

	public:
	// Returns the treeview widget
	GtkTreeView *get_treeview() { return GTK_TREE_VIEW(treeview); }
	// Returns the treeselection
	GtkTreeSelection *get_selection() { return selection; }
	// Returns the treemodel associated with the list
	GtkTreeModel *get_treemodel() { return treemodel; }
	// Sets the treemodel for the listview
	void set_treemodel(GtkTreeModel *tm) { treemodel = tm; }
	// Returns the liststore associated with the model
	GtkListStore *get_liststore() { return liststore; }
	// Create elements of treeview model
	void initialise(const char*, char*);
	// Create elements of treeview, linking to specified list
	void initialise_link(GtkListStore*,const char*);

	/*
	// Operations
	*/
	protected:
	// Identifying name for the treeview structure
	char name[32];
	// Pointer to selected row item (if any)
	gpointer selection_pointer;

	public:
	// Sets the internal name of the treeview
	void set_name(const char *s) { strcpy(name,s); };
	// Clear the list
	void clear();
	// Append an empty row to the list
	void append(GtkTreeIter*);
	// Set the information in the row
	void set(GtkTreeIter*,...);
	// Append an empty row and set its information
	void append_and_set(GtkTreeIter*,...);
	// Count the number of selected rows
	int nselected();
	// Returns whether the specified iter is selected
	bool is_selected(GtkTreeIter*);
	// Select the specified iter
	void select(GtkTreeIter*);
	// Select the row in the model containing column with value specified
	bool select(int,gpointer);
	// Find iter in list
	bool find_iter(GtkTreeIter*, int, gpointer);
	void unselect(GtkTreeIter*);
	// Set the selection type to GTK_SELECTION_MULTIPLE
	void set_multiple();
	// Get the first iter in the model
	bool get_first(GtkTreeIter*);
	// Get the iter after the supplied iter.
	bool get_next(GtkTreeIter*);
	// Get the currently selected row
	bool get_selected(GtkTreeIter*);
	// Find the iter matching the supplied column data
	void get(GtkTreeIter*, int, gpointer);
	void remove(GtkTreeIter *iter);
	// Set widget size
	void set_size(int, int);
	// Sets the value of selection_pointer
	void set_selptr(gpointer p) { selection_pointer = p; }
	// Returns the values of selection_pointer
	gpointer get_selptr() { return selection_pointer; }
	// List pointers
	treeview_list *prev, *next;
};

// Master Treeview List
// Hidden treeview list which has a number of dependent child tree lists
class treeview_master : public treeview_list
{
	private:
	// Treeview column containing pointer data
	int pointercol;
	// List of child treeviews
	list<treeview_list> children;
	// Set data in an iter
	virtual void set_iter(GtkTreeIter*,gpointer)=0;
	//	{ printf("treeview_master::set_iter() called.\n"); }
	public:
	// Sets the pointer column id
	void set_pointer_column(int i) { pointercol = i; }
	// Add a new child treeview to the master
	treeview_list *add_child();
	// Add an item to the treeview list
	void add_item(gpointer);
	void update();
	virtual void refresh() = 0;
};

// Model master
class model_master : public treeview_master
{
	private:
	void set_iter(GtkTreeIter*,gpointer);
	public:
	void refresh();
};

// Pattern master
class pattern_master : public treeview_master
{
	private:
	void set_iter(GtkTreeIter*,gpointer);
	public:
	void refresh();
};

// Forcefield master
class ff_master : public treeview_master
{
	private:
	void set_iter(GtkTreeIter*,gpointer);
	public:
	void refresh();
};


#endif
