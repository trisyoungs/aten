/*
	*** GTK atom menu
	*** src/gui-gtk/menu_atom.cpp
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

using namespace std;
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "model/model.h"
#include "classes/forcefield.h"
#include "base/master.h"

// Variables
GtkMenuItem *atompopup_hideshow;
reflist<GtkWidget*> atompopup_fftypes;
atom *atompopup_atom;

/*
// Menu Callbacks
*/

void atommenu_change(GtkMenuItem *widget, gpointer data)
{
	dbg_begin(DM_CALLS,"atommenu_click_change");
	// Parse the action passed
	model *m = master.get_currentmodel();
	parser.get_args_delim((char*)data,PO_DEFAULTS);
	if (strcmp(parser.argc(0),"style") == 0)
	{
		// Change atom draw style
		draw_style ds = DS_from_text(parser.argc(1));
		if (atompopup_atom->is_selected()) m->selection_set_style(ds);
		else atompopup_atom->set_style(ds);
	}
	else if (strcmp(parser.argc(0),"label") == 0)
	{
		atom_label al = AL_from_text(parser.argc(1));
		if (atompopup_atom->is_selected()) master.get_currentmodel()->selection_set_atom_labels(al);
		else atompopup_atom->add_label(al);
	}
	else if (strcmp(parser.argc(0),"clearlabel") == 0)
	{
		if (atompopup_atom->is_selected()) master.get_currentmodel()->selection_clear_atom_labels();
		else atompopup_atom->clear_labels();
	}
	else if (strcmp(parser.argc(0),"fixatoms") == 0)
	{
		// Sets the 'fixed' variable of the atoms to TRUE
		if (atompopup_atom->is_selected()) master.get_currentmodel()->selection_set_fixed();
		else atompopup_atom->fixed = TRUE;
	}
	else if (strcmp(parser.argc(0),"freeatoms") == 0)
	{
		// Sets the 'fixed' variable of the atoms to TRUE
		if (atompopup_atom->is_selected()) master.get_currentmodel()->selection_set_free();
		else atompopup_atom->fixed = FALSE;
	}
	gui.refresh();
	dbg_end(DM_CALLS,"atommenu_click_change");
}

/*
// GUI Creation
*/

void gui_gtk::call_atompopup(GtkWidget *widget,GdkEventButton *event,atom *target)
{
	// Create the popup menu for atoms in the main display
	GtkMenu *shell, *subshell;
	GtkMenuItem *m;
	// Set the called atom variable
	atompopup_atom = target;
	shell = GTK_MENU(gtk_menu_new());

	// TODO Do we need to replace the 'deactivate' signal handler here?
	//g_signal_connect(shell,"deactivate",G_CALLBACK(gtk_widget_destroy),NULL);

	// 'Name' of popup menu - specifies target of operation
	if (target->is_selected()) m = cs_menuitem("Selection",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	else m = cs_menuitem("Atom",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	gtk_widget_set_sensitive(GTK_WIDGET(m),FALSE);
	cs_menuseparator(shell);

	// -- Rendering style submenu
	m = cs_menuitem("Style",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	m = cs_menuitem("Stick",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"style,stick");
	m = cs_menuitem("Tube",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"style,tube");
	m = cs_menuitem("Sphere",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"style,sphere");
	m = cs_menuitem("Scaled Sphere",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"style,scaled");

	// -- Atom show / hide
	m = cs_menuitem("Toggle Visible",shell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"showhide");

	// -- Atom labels submenu
	m = cs_menuitem("Labels",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	m = cs_menuitem("ID",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"label,id");
	m = cs_menuitem("Element",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"label,element");
	m = cs_menuitem("FF Type",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"label,fftype");
	m = cs_menuitem("FF Equiv",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"label,ffequiv");
	m = cs_menuitem("Charge",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"label,charge");
	m = cs_menuitem("Clear all",subshell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"clearlabel");

	// -- Forcefield type
	m = cs_menuitem("Fix FF Type",shell,NULL,0,GDK_CONTROL_MASK,NULL,NULL);
	subshell = cs_menusubshell(m);
	// ---- Build up list of menu items based on types in the current forcefield
	forcefield *ff = master.get_currentmodel()->get_ff();
	if (ff != NULL)
	{
	//	for (int n=1; n<=ff->natomtypes; n++)
	//		if (ff->atomtypes[n].el == target->el)
				
	}

	// -- Fixed/Free status of atoms
	m = cs_menuitem("Fix Position",shell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"fixatoms");
	m = cs_menuitem("Free Position",shell,NULL,0,GDK_CONTROL_MASK,atommenu_change,"freeatoms");
	// Show the popup menu
	cs_showall(shell);
	gtk_menu_popup(GTK_MENU(shell), NULL, NULL, NULL, NULL, event->button, event->time);
}
