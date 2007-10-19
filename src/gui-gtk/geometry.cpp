/*
	*** GTK geometry window
	*** src/gui-gtk/geometry.cpp
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
#include "base/prefs.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"

gint geomwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// When the window is hidden, we want to return to atom picking. So, activate the atom picking
	// button in the toolbar
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(gui.UAtoolitem[UA_PICKSELECT]),TRUE);
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_MEASURE]);
	gui.refresh();
	return TRUE;
}

void geomwin_click_closebtn(GtkButton *widget, gpointer data)
{
	geomwin_close(NULL,NULL,NULL);
}

void geomwin_click_geom_mode(GtkRadioToolButton *widget, gpointer data)
{
	dbg_begin(DM_CALLS,"geomwin_click_geom_mode");
	// Find which button has just been pressed...
	for (int i=UA_GEOMSELECT; i<=UA_GEOMTORSION; i++)
		if (widget == GTK_RADIO_TOOL_BUTTON(gui.UAtoolitem[i])) gui.mainview.set_selectedmode((user_action) i);
	dbg_end(DM_CALLS,"geomwin_click_geom_mode");
}

// All in selection callbacks
void geomwin_click_measuresel(GtkButton *widget, gpointer data)
{
	string action = (char*) data;
	model *m = master.get_currentmodel();
	if (action == "bonds") m->add_measurements_in_selection(GT_DISTANCE);
	else if (action == "angles") m->add_measurements_in_selection(GT_ANGLE);
	else if (action == "torsions") m->add_measurements_in_selection(GT_TORSION);
}

/*
// Measurement callbacks
*/

void geomwin_click_clear_all_geom(GtkButton *widget, gpointer data)
{
	// Clear all geometry measurements in the current model
	dbg_begin(DM_CALLS,"geomwin_click_clear_all_geom");
	master.get_currentmodel()->clear_measurements();
	gui.refresh();
	dbg_end(DM_CALLS,"geomwin_click_clear_all_geom");
}

void geomwin_toggle_geom_annot(GtkCheckButton *widget, gpointer data)
{
	// Toggle display of geometry measurements
	dbg_begin(DM_CALLS,"geomwin_toggle_geom_annot");
	prefs.set_visible(VO_MEASUREMENTS, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
	gui.refresh();
	dbg_end(DM_CALLS,"geomwin_toggle_geom_annot");
}

void gui_gtk::geomwin_create()
{
	// Create the window of measuring tools.
	dbg_begin(DM_CALLS,"gui_gtk::geomwin_create");
	GtkIconSize icon_size;
	GtkBox *mainbox, *vbox, *hbox;
	GtkFrame *frame;
	GtkButton *b;
	GtkToolItem *t;
	GtkToolbar *toolbar;
	GtkObject *adj;
	int i;
	windows[GW_MEASURE] = cs_subwindow("Measure",FALSE,geomwin_close);

	// A single vbox will hold the frames for the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_MEASURE]);
	GtkTooltips *tips = gtk_tooltips_new();
	gtk_tooltips_enable(tips);

	// Standard measuring tools
	vbox = cs_box(CS_VERTICAL,FALSE,2,mainbox);
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,vbox,CS_START,TRUE,TRUE,0);
	t = UAtoolitem[UA_PICKSELECT];	// All radio btns linked to this one
	UAtoolitem[UA_GEOMSELECT] = cs_radiotoolbtn(toolbar,t,tips,"Select / Rotate",
		xpms[XPM_SELECT_PICK],geomwin_click_geom_mode);
	UAtoolitem[UA_GEOMDIST] = cs_radiotoolbtn(toolbar,t,tips,"Distance between two atoms",
		xpms[XPM_GEOMETRY_DISTANCE],geomwin_click_geom_mode);
	UAtoolitem[UA_GEOMANGLE] = cs_radiotoolbtn(toolbar,t,tips,"Angles between atoms",
		xpms[XPM_GEOMETRY_ANGLE],geomwin_click_geom_mode);
	UAtoolitem[UA_GEOMTORSION] = cs_radiotoolbtn(toolbar,t,tips,"Dihedral angles",
		xpms[XPM_GEOMETRY_TORSION],geomwin_click_geom_mode);

	// 'All in selection' measuring tools
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,vbox,CS_START,TRUE,TRUE,0);
	t = cs_toolbtn(toolbar,tips,"All bonds in selection",xpms[XPM_GEOMETRY_DISTANCE],geomwin_click_measuresel,"bonds");
	t = cs_toolbtn(toolbar,tips,"All angles in selection",xpms[XPM_GEOMETRY_ANGLE],geomwin_click_measuresel,"angles");
	t = cs_toolbtn(toolbar,tips,"All torsions in selection",xpms[XPM_GEOMETRY_TORSION],geomwin_click_measuresel,"torsions");
		
	cs_toolseparator(toolbar);
	// ---- Checkbox for turning on/off geometry
	check[W_CHK_SHOWGEOM] = cs_check("Show measurements", TRUE, vbox, CS_START, FALSE, TRUE,0,geomwin_toggle_geom_annot,NULL);
	// ---- Button to clear all measurements
	b = cs_button("Clear All",vbox,CS_START,FALSE,TRUE,0,geomwin_click_clear_all_geom,NULL);

	// Buttons at foot of window (OK etc.)
	vbox = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,vbox,CS_START,FALSE,TRUE,0);
	hbox = cs_box(CS_HORIZONTAL,FALSE,0,vbox,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",hbox,CS_END,FALSE,TRUE,0,geomwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::geomwin_create");
}

