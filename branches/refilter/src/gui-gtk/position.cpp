/*
	*** GTK atom position window
	*** src/gui-gtk/position.cpp
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
#include "base/master.h"
#include "base/sysfunc.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"

// Variables
GtkSpinButton *orient_from[3], *orient_to[3], *rotate_step, *rotate_vector[3], *rotate_origin[3];
GtkRadioButton *trans_radios[4];
GtkSpinButton *trans_steps[4], *trans_origin[3], *trans_vector[3];
GtkButton *trans_btn[6];

gint positionwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// When the window is hidden, we want to return to atom picking. So, activate the atom picking
	// button in the toolbar
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(gui.UAtoolitem[UA_PICKSELECT]),TRUE);
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_POSITION]);
	gui.refresh();
	return TRUE;
}

void positionwin_click_closebtn(GtkButton *widget, gpointer data)
{
	positionwin_close(NULL,NULL,NULL);
}

/*
// Atom Positioning Callbacks
*/

void positionwin_transtype_changed(GtkRadioButton *widget, gpointer data)
{
}

void positionwin_click_translate(GtkButton *widget, gpointer data)
{
	char *action = (char*) data;
	double step;
	mat3<double> cellmat;
	vec3<double> tvec;
	int transtype;
	// Determine which type of translation is selected and get values
	for (transtype = 0; transtype < 4; transtype ++)
		if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(trans_radios[transtype]))) break;
	step = gtk_spin_button_get_value(trans_steps[transtype]);
	// Set translation vector
	if (transtype < 2)		// Local frame (atomic coordinates) or world coordinates (camera)
	{
		if (strcmp(action,"negx") == 0) tvec.set(-step,0.0,0.0);
		else if (strcmp(action,"posx") == 0) tvec.set(step,0.0,0.0);
		if (strcmp(action,"negy") == 0) tvec.set(0.0,-step,0.0);
		else if (strcmp(action,"posy") == 0) tvec.set(0.0,step,0.0);
		if (strcmp(action,"negz") == 0) tvec.set(0.0,0.0,-step);
		else if (strcmp(action,"posz") == 0) tvec.set(0.0,0.0,step);
	}
	else if (transtype == 2)	// Along cell axes
	{
		if (master.get_currentmodel()->cell.get_type() == CT_NONE)
		{
			msg(DM_NONE,"No unit cell defined for model.\n");
			return;
		}
		else cellmat = master.get_currentmodel()->cell.get_axes();
		if (strcmp(action,"negx") == 0) tvec = -cellmat.rows[0];
		else if (strcmp(action,"posx") == 0) tvec = cellmat.rows[0];
		if (strcmp(action,"negy") == 0) tvec = -cellmat.rows[1];
		else if (strcmp(action,"posy") == 0) tvec = cellmat.rows[1];
		if (strcmp(action,"negz") == 0) tvec = -cellmat.rows[2];
		else if (strcmp(action,"posz") == 0) tvec = cellmat.rows[2];
		tvec *= step;
	}
	else				// Along vector
	{
	}
	// Now translate selection
	if (transtype == 1) master.get_currentmodel()->translate_selection_world(tvec);
	else master.get_currentmodel()->translate_selection_local(tvec);
	gui.refresh();
}

void positionwin_click_rotate(GtkButton *widget, gpointer data)
{
	vec3<double> v,o;
	double step;
	char *action = (char*) data;
	model *m = master.get_currentmodel();
	if (strcmp(action,"defineo") == 0)
	{
		// Get geometric centre of selection
		v = m->selection_get_cog();
		// Set widgets
		gtk_spin_button_set_value(rotate_origin[0],v.x);
		gtk_spin_button_set_value(rotate_origin[1],v.y);
		gtk_spin_button_set_value(rotate_origin[2],v.z);
	}
	else if (strcmp(action,"definev") == 0)
	{
		// Get geometric centre of selection
		v = m->selection_get_cog();
		o.x = gtk_spin_button_get_value(rotate_origin[0]);
		o.y = gtk_spin_button_get_value(rotate_origin[1]);
		o.z = gtk_spin_button_get_value(rotate_origin[2]);
		// Set widgets
		v -= o;
		gtk_spin_button_set_value(rotate_vector[0],v.x);
		gtk_spin_button_set_value(rotate_vector[1],v.y);
		gtk_spin_button_set_value(rotate_vector[2],v.z);
	}
	else
	{
		o.x = gtk_spin_button_get_value(rotate_origin[0]);
		o.y = gtk_spin_button_get_value(rotate_origin[1]);
		o.z = gtk_spin_button_get_value(rotate_origin[2]);
		v.x = gtk_spin_button_get_value(rotate_vector[0]);
		v.y = gtk_spin_button_get_value(rotate_vector[1]);
		v.z = gtk_spin_button_get_value(rotate_vector[2]);
		step = gtk_spin_button_get_value(rotate_step);
		if (strcmp(action,"clock") == 0) m->rotate_selection_vector(o,v,step);
		else m->rotate_selection_vector(o,v,-step);
	}
	gui.refresh();
}

/*
// Notebook pages and main window
*/

void gui_gtk::positionwin_create_translate(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::positionwin_create_translate");
	GtkBox *mainbox, *box, *page, *subbox;
	GtkFrame *frame;
	GtkButton *b;
	GtkLabel *l;
	GtkCheckButton *check;
	GtkTable *table;
	GtkObject *adj;
	// Create the page
	page = cs_notebookpage("Translate",notebook);
	mainbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));

	// Translation buttons
	table = cs_table(5,5,FALSE,mainbox,CS_START,TRUE,TRUE,0);
	trans_btn[0] = cs_pixbutton(xpms[XPM_TRANSLATE_LEFT],table,2,3,1,2,positionwin_click_translate,"negx");
	trans_btn[1] = cs_pixbutton(xpms[XPM_TRANSLATE_RIGHT],table,4,5,1,2,positionwin_click_translate,"posx");
	trans_btn[2] = cs_pixbutton(xpms[XPM_TRANSLATE_UP],table,3,4,0,1,positionwin_click_translate,"posy");
	trans_btn[3] = cs_pixbutton(xpms[XPM_TRANSLATE_DOWN],table,3,4,2,3,positionwin_click_translate,"negy");
	trans_btn[4] = cs_pixbutton(xpms[XPM_TRANSLATE_OUT],table,4,5,2,3,positionwin_click_translate,"posz");
	trans_btn[5] = cs_pixbutton(xpms[XPM_TRANSLATE_IN],table,2,3,2,3,positionwin_click_translate,"negz");

	// Reference frame radio buttons
	//l = cs_label("Step",table,0,1,1,2);
	// -- Local frame (requires stepsize)
	trans_radios[0] = cs_radio(NULL,"Local",table,0,1,0,1,positionwin_transtype_changed);
	adj = cs_adjustment(1.0,0.0,100.0,0.1,0.1);
	trans_steps[0] = cs_spin(adj,1.0,3,table,1,2,0,1,NULL,NULL);
	// -- World frame (requires stepsize)
	trans_radios[1] = cs_radio(trans_radios[0],"World",table,0,1,1,2,positionwin_transtype_changed);
	adj = cs_adjustment(1.0,0.0,100.0,0.1,0.1);
	trans_steps[1] = cs_spin(adj,1.0,3,table,1,2,1,2,NULL,NULL);
	// -- Unit cell frame (requires stepsize)
	trans_radios[2] = cs_radio(trans_radios[0],"Cell",table,0,1,2,3,positionwin_transtype_changed);
	adj = cs_adjustment(1.0,0.0,100.0,0.1,0.1);
	trans_steps[2] = cs_spin(adj,1.0,3,table,1,2,2,3,NULL,NULL);
	// -- Vector frame (requires stepsize and direction
	trans_radios[3] = cs_radio(trans_radios[0],"Vector",table,0,1,3,4,positionwin_transtype_changed);
	adj = cs_adjustment(1.0,0.0,100.0,0.1,0.1);
	trans_steps[2] = cs_spin(adj,1.0,3,table,1,2,3,4,NULL,NULL);

	gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(trans_radios[0]),TRUE);

	dbg_end(DM_CALLS,"gui_gtk::positionwin_create_translate");
}

void gui_gtk::positionwin_create_orient(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::positionwin_create_orient");
	GtkBox *box, *mainbox, *page;
	GtkFrame *frame;
	GtkTable *table;
	GtkToolbar *toolbar;
	GtkLabel *l;
	GtkToolItem *t;
	GtkObject *adj;
	// Create the page
	page = cs_notebookpage("Orient",notebook);
	mainbox = cs_box(CS_HORIZONTAL,FALSE,0,GTK_CONTAINER(page));

	// 'From' vector frame
	frame = cs_frame("From Vector",mainbox,CS_START,FALSE,TRUE,0);
	table = cs_table(4,2,FALSE,frame);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_from[0] = cs_spin(adj,0.0,4,table,1,2,0,1,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_from[1] = cs_spin(adj,0.0,4,table,1,2,1,2,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_from[2] = cs_spin(adj,0.0,4,table,1,2,2,3,NULL,NULL);
	l = cs_label("X",table,0,1,0,1);
	l = cs_label("Y",table,0,1,1,2);
	l = cs_label("Z",table,0,1,2,3);

	// 'To' vector frame
	frame = cs_frame("To Vector",mainbox,CS_END,FALSE,TRUE,0);
	table = cs_table(3,2,FALSE,frame);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_to[0] = cs_spin(adj,0.0,4,table,1,2,0,1,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_to[1] = cs_spin(adj,0.0,4,table,1,2,1,2,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	orient_to[2] = cs_spin(adj,0.0,4,table,1,2,2,3,NULL,NULL);
	l = cs_label("X",table,0,1,0,1);
	l = cs_label("Y",table,0,1,1,2);
	l = cs_label("Z",table,0,1,2,3);

	dbg_end(DM_CALLS,"gui_gtk::positionwin_create_orient");
}

void gui_gtk::positionwin_create_rotate(GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"gui_gtk::positionwin_create_rotate");
	GtkBox *box, *mainbox, *page;
	GtkFrame *frame;
	GtkTable *table;
	GtkToolbar *toolbar;
	GtkButton *b;
	GtkLabel *l;
	GtkObject *adj;
	// Create the page
	page = cs_notebookpage("Rotate",notebook);
	mainbox = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(page));

	box = cs_box(CS_HORIZONTAL,FALSE,0,mainbox,CS_START,TRUE,TRUE,0);
	// Origin (from)
	frame = cs_frame("Axis Origin",box,CS_START,TRUE,TRUE,0);
	table = cs_table(4,2,FALSE,frame);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_origin[0] = cs_spin(adj,0.0,4,table,1,2,0,1,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_origin[1] = cs_spin(adj,0.0,4,table,1,2,1,2,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_origin[2] = cs_spin(adj,0.0,4,table,1,2,2,3,NULL,NULL);
	l = cs_label("X",GTK_TABLE(table),0,1,0,1);
	l = cs_label("Y",GTK_TABLE(table),0,1,1,2);
	l = cs_label("Z",GTK_TABLE(table),0,1,2,3);
	b = cs_button("Define",table,0,2,3,4,positionwin_click_rotate,"defineo");

	// Vector frame
	frame = cs_frame("Vector",box,CS_END,TRUE,TRUE,0);
	table = cs_table(3,2,FALSE,frame);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_vector[0] = cs_spin(adj,0.0,4,table,1,2,0,1,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_vector[1] = cs_spin(adj,0.0,4,table,1,2,1,2,NULL,NULL);
	adj = cs_adjustment(0.0,-1000.0,1000.0,1.0,1.0);
	rotate_vector[2] = cs_spin(adj,0.0,4,table,1,2,2,3,NULL,NULL);
	l = cs_label("X",table,0,1,0,1);
	l = cs_label("Y",table,0,1,1,2);
	l = cs_label("Z",table,0,1,2,3);
	b = cs_button("Define",table,0,2,3,4,positionwin_click_rotate,"definev");

	// Action frame
	box = cs_box(CS_HORIZONTAL,FALSE,0,mainbox,CS_END,TRUE,TRUE,0);
	frame = cs_frame("Rotate",box,CS_START,TRUE,TRUE,0);
	table = cs_table(1,3,FALSE,frame);
	adj = cs_adjustment(60.0,0.0,360.0,1.0,1.0);
	rotate_step = cs_spin(adj,55.0,2,table,1,2,0,1,NULL,NULL);
	b = cs_button("Rotate -",table,0,1,0,1,positionwin_click_rotate,"anticlock");
	b = cs_button("Rotate +",table,2,3,0,1,positionwin_click_rotate,"clock");

	dbg_end(DM_CALLS,"gui_gtk::positionwin_create_rotate");
}

void gui_gtk::positionwin_create()
{
	// Create the window of measuring tools.
	dbg_begin(DM_CALLS,"gui_gtk::positionwin_create");
	GtkButton *b;
	GtkBox *mainbox, *box, *hbox;
	GtkNotebook *notebook;
	int i;
	windows[GW_POSITION] = cs_subwindow("Positioning",FALSE,positionwin_close);

	// A single vbox will hold the frames for the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_POSITION]);

	// A notebook will hold the different pages
	notebook = cs_notebook(mainbox,CS_START,TRUE,TRUE,0);
	// ** Atom translate
	positionwin_create_translate(notebook);
	// ** Atom orient
	positionwin_create_orient(notebook);
	// ** Atom rotate
	positionwin_create_rotate(notebook);

	// Buttons at foot of window (OK etc.)
	box = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,box,CS_START,FALSE,TRUE,0);
	hbox = cs_box(CS_HORIZONTAL,FALSE,0,box,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",hbox,CS_END,FALSE,TRUE,0,positionwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::positionwin_create");
}

