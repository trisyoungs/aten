/*
	*** GTK atom builder window
	*** src/gui-gtk/atombuild.cpp

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
#include "base/elements.h"
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"

// Variables
GtkToolItem *elementbtn[8];
GtkWidget *other_element_label;

gint buildwin_close(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// When the window is hidden, we want to return to atom picking, so activate the UA_PICKSELECT toolbutton.
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(gui.UAtoolitem[UA_PICKSELECT]),TRUE);
	// 'Delete Event' handler, called when we try to close the window.
	cs_hide(gui.windows[GW_ATOMBUILDER]);
	gui.refresh();
	return TRUE;
}

void buildwin_click_closebtn(GtkButton *widget, gpointer data)
{
	buildwin_close(NULL,NULL,NULL);
}

void buildwin_click_change_mode(GtkRadioToolButton *widget, gpointer data)
{
	// Sketch mode has been changed, so show/hide the correct sub-panel.
	// We receive two events - one for the previous button returning to 'unselected' and one for the actual button pressed going in to its 'selected' state.
	dbg_begin(DM_CALLS,"buildwin_click_change_mode");
	// Find which button has just been pressed and store it in lastmode[]
	for (int i=UA_NONE; i<=UA_NITEMS; i++)
		if (widget == GTK_RADIO_TOOL_BUTTON(gui.UAtoolitem[i]))
		{
			gui.mainview.set_selectedmode((user_action) i);
			break;
		}
	dbg_end(DM_CALLS,"buildwin_click_change_mode");
}

/*
// Selection Callbacks
*/

void buildwin_click_invert_selection(GtkButton *widget, gpointer data)
{
	dbg_begin(DM_CALLS,"buildwin_click_invert_selection");
	master.get_currentmodel()->selection_invert();
	gui.refresh();
	dbg_end(DM_CALLS,"buildwin_click_invert_selection");
}

void buildwin_click_expand_selection(GtkButton *widget, gpointer data)
{
	dbg_begin(DM_CALLS,"buildwin_click_expand_selection");
	master.get_currentmodel()->selection_expand();
	gui.refresh();
	dbg_end(DM_CALLS,"buildwin_click_expand_selection");
}

/*
// Sketching Callbacks
*/

void buildwin_changed_depth(GtkSpinButton *widget, gpointer data)
{
	// Set the drawing guide depth in canvas
	prefs.set_draw_depth(gtk_spin_button_get_value(GTK_SPIN_BUTTON(widget)));
	gui.refresh();
}

void buildwin_click_guidecheck(GtkCheckButton *widget, gpointer data)
{
	// Visibility of drawing guide has been changed
	prefs.set_guide_visible(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)) ? TRUE : FALSE);
	gui.refresh();
}

void buildwin_change_guideshape(GtkComboBox *widget, gpointer data)
{
	// Style of the drawing guide has been changed.
	// Will need to re-initialise the OpenGL compiled lists
	prefs.set_guide_shape((guide_geometry) gtk_combo_box_get_active(GTK_COMBO_BOX(widget)));
	gui.refresh();
}

void buildwin_click_element(GtkRadioToolButton *widget, gpointer data)
{
	// Sketching element changed, so update the master.sketchelement variable
	dbg_begin(DM_CALLS,"buildwin_click_element");
	int el = atoi((char*) data);
	master.sketchelement = el;
	dbg_end(DM_CALLS,"buildwin_click_element");
}

void buildwin_click_other_element(GtkRadioToolButton *widget, gpointer data)
{
	// Set element from text of toolbutton label
	const char *elname = gtk_label_get_text(GTK_LABEL(other_element_label));
	master.sketchelement = elements.find(elname);
}

void buildwin_click_hydrogen_add(GtkButton *widget, gpointer data)
{
	master.get_currentmodel()->hydrogen_satisfy();
	gui.refresh();
}

// Bond editing callbacks
void buildwin_click_bonding(GtkButton *widget, gpointer data)
{
	dbg_begin(DM_CALLS,"buildwin_click_bonding");
	string action = (char*) data;
	if (action == "calcbonding")
	{
		master.get_currentmodel()->clear_bonding();
		master.get_currentmodel()->calculate_bonding();
	}
	else if (action == "clearbonding") master.get_currentmodel()->clear_bonding();
	else if (action == "selcalcbonding") master.get_currentmodel()->selection_calculate_bonding();
	else if (action == "selclearbonding") master.get_currentmodel()->selection_clear_bonding();
	else if (action == "augbonding") master.get_currentmodel()->augment_bonding();
	//TODO else if (action == "selaugbonding")
	gui.refresh();
	dbg_end(DM_CALLS,"buildwin_click_bonding");
}

void buildwin_click_sel_bond_all(GtkButton *widget, gpointer data)
{
	// Bond all pairs of atoms in the current selection
	dbg_begin(DM_CALLS,"buildwin_click_sel_bond_all");
	master.get_currentmodel()->selection_bond_all();
	gui.refresh();
	dbg_end(DM_CALLS,"buildwin_click_sel_bond_all");
}

void buildwin_change_bondtol(GtkSpinButton *sender, gpointer data)
{
	// Change the proximity tolerance for bonding calculation
	dbg_begin(DM_CALLS,"buildwin_change_bondtol");
	prefs.set_bond_tolerance(gtk_spin_button_get_value(sender));
	master.get_currentmodel()->clear_bonding();
	master.get_currentmodel()->calculate_bonding();
	gui.refresh();
	dbg_end(DM_CALLS,"buildwin_change_bondtol");
}

void gui_gtk::atombuildwin_create()
{
	// Create the window of drawing tools.
	dbg_begin(DM_CALLS,"gui_gtk::atombuildwin_create");
	GtkIconSize icon_size;
	GtkBox *mainbox, *vbox, *hbox, *vbox2;
	GtkFrame *frame;
	GtkToolbar *toolbar;
	GtkTable *table;
	GtkCheckButton *check;
	GtkWidget *menu;
	GtkSpinButton *spin;
	GtkComboBox *combo;
	GtkLabel *l;
	GtkScale *scale;
	GtkButton *b;
	GtkToolItem *t;
	GtkObject *adj;
	int i;
	windows[GW_ATOMBUILDER] = cs_subwindow("Atomic Builder",FALSE,buildwin_close);

	// A single vbox will hold the frames for the window
	mainbox = cs_box(CS_VERTICAL,FALSE,2,windows[GW_ATOMBUILDER]);
	GtkTooltips *tips = gtk_tooltips_new();
	gtk_tooltips_enable(tips);

	// Subframe "Select"
	frame = cs_frame("Select",mainbox,CS_START,FALSE,TRUE,0);
	//frame = gtk_expander_new_with_mnemonic("Select");
	//gtk_box_pack_start(GTK_BOX(mainbox),frame,FALSE,TRUE,0);
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,GTK_CONTAINER(frame));
	gtk_widget_set_size_request(GTK_WIDGET(toolbar),256,-1);
	UAtoolitem[UA_PICKSELECT] = cs_radiotoolbtn(toolbar,NULL,tips,"Select / rotate",
		xpms[XPM_SELECT_PICK],buildwin_click_change_mode);
	t = UAtoolitem[UA_PICKSELECT];
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(t),TRUE);
	UAtoolitem[UA_PICKFRAG] = cs_radiotoolbtn(toolbar,t,tips,"Select molecules",
		xpms[XPM_SELECT_FRAGMENT],buildwin_click_change_mode);
	UAtoolitem[UA_PICKELEMENT] = cs_radiotoolbtn(toolbar,t,tips,"Select by element",
		xpms[XPM_SELECT_ELEMENT],buildwin_click_change_mode);
	UAtoolitem[UA_PICKRADIAL] = cs_radiotoolbtn(toolbar,t,tips,"Radial select",
		xpms[XPM_SELECT_SPHERE],buildwin_click_change_mode);
	cs_toolseparator(toolbar);
	t = cs_toolbtn(toolbar,tips,"Invert current selection",xpms[XPM_SELECT_INVERT],buildwin_click_invert_selection,NULL);
	t = cs_toolbtn(toolbar,tips,"Expand current selection",xpms[XPM_SELECT_EXPAND],buildwin_click_expand_selection,NULL);
	// Subframe "Sketcher"
	frame = cs_frame("Build",mainbox,CS_START,FALSE,TRUE,0);
	vbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(frame));
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,vbox,CS_START,FALSE,TRUE,0);
	t = UAtoolitem[UA_PICKSELECT];
	UAtoolitem[UA_DRAWATOM] = cs_radiotoolbtn(toolbar,t,tips,"Place atoms",
		xpms[XPM_DRAW_SINGLEATOM],buildwin_click_change_mode);
	UAtoolitem[UA_DRAWCHAIN] = cs_radiotoolbtn(toolbar,t,tips,"Draw chains",
		xpms[XPM_DRAW_ATOMS],buildwin_click_change_mode);
	UAtoolitem[UA_TRANSATOM] = cs_radiotoolbtn(toolbar,t,tips,"Transmute atom",
		xpms[XPM_DRAW_TRANSMUTE],buildwin_click_change_mode);
	UAtoolitem[UA_DELATOM] = cs_radiotoolbtn(toolbar,t,tips,"Delete atoms",
		xpms[XPM_DRAW_DELATOM],buildwin_click_change_mode);
	cs_toolseparator(toolbar);
	t = cs_toolbtn(toolbar,tips,"Hydrogen add",xpms[XPM_DRAW_HADD],buildwin_click_hydrogen_add,NULL);
	// ---- Sketch Sub-toolbar 
	vbox2 = cs_box(CS_VERTICAL,FALSE,2,vbox,CS_START,FALSE,TRUE,0);
	// ----- ** Element selection
	toolbar = cs_toolbar(GTK_TOOLBAR_TEXT,CS_HORIZONTAL,vbox2,CS_END,FALSE,TRUE,0);
	elementbtn[0] = cs_textradiotoolbtn(toolbar,NULL,tips,"Carbon","C",buildwin_click_element,"6");
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(elementbtn[0]),TRUE);
	elementbtn[1] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Hydrogen","H",buildwin_click_element,"1");
	elementbtn[2] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Oxygen","O",buildwin_click_element,"8");
	elementbtn[3] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Nitrogen","N",buildwin_click_element,"7");
	elementbtn[4] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Phosphorus","P",buildwin_click_element,"15");
	elementbtn[5] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Sulfur","S",buildwin_click_element,"16");
	// Special radiobutton, where we create and store the label widget ourselves
	elementbtn[6] = gtk_radio_tool_button_new_from_widget(GTK_RADIO_TOOL_BUTTON(elementbtn[0]));
	other_element_label = gtk_label_new("Cl");
	gtk_tool_button_set_label_widget(GTK_TOOL_BUTTON(elementbtn[6]),other_element_label);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar),GTK_TOOL_ITEM(elementbtn[6]),-1);
	g_signal_connect(elementbtn[6],"clicked",G_CALLBACK(buildwin_click_other_element),NULL);
	cs_toolseparator(toolbar);
	//elementbtn[7] = cs_textradiotoolbtn(toolbar,elementbtn[0],tips,"Select...","XX",buildwin_click_element);

	// ----- ** Draw depth / guide control
	hbox = cs_box(CS_HORIZONTAL,FALSE,2,vbox2,CS_START,FALSE,TRUE,0);
	l = cs_label("Draw Z",hbox,CS_START,FALSE,FALSE,0);
	adj = cs_adjustment(prefs.get_draw_depth(),-10.0,1000.0,1.0,1.0);
	spin = cs_spin(adj,1.0,2,hbox,CS_START,TRUE,TRUE,0,buildwin_changed_depth,NULL);
	check = cs_check("Guide", prefs.is_guide_visible(), hbox, CS_END, FALSE, FALSE, 0, buildwin_click_guidecheck, NULL);
	hbox = cs_box(CS_HORIZONTAL,FALSE,2,vbox2,CS_START,FALSE,TRUE,0);
	l = cs_label("Guide Style",hbox,CS_START,FALSE,FALSE,0);
	combo = cs_combo(get_GG_strings(),GG_NITEMS,0,hbox,CS_START,FALSE,TRUE,0,buildwin_change_guideshape);


	// Frame "Bonding"
	// Contains two toolbars - one for general bonding, one for 'extra' functions
	frame = cs_frame("Bonding",mainbox,CS_START,FALSE,TRUE,0);
	vbox = cs_box(CS_VERTICAL,FALSE,2,GTK_CONTAINER(frame));
	// User Actions
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,vbox,CS_END,TRUE,TRUE,0);
	t = UAtoolitem[UA_PICKSELECT];
	UAtoolitem[UA_BONDSINGLE] = cs_radiotoolbtn(toolbar,t,tips,"Add single bond",
		xpms[XPM_BOND_MAKESINGLE],buildwin_click_change_mode);
	UAtoolitem[UA_BONDDOUBLE] = cs_radiotoolbtn(toolbar,t,tips,"Add double bond",
		xpms[XPM_BOND_MAKEDOUBLE],buildwin_click_change_mode);
	UAtoolitem[UA_BONDTRIPLE] = cs_radiotoolbtn(toolbar,t,tips,"Add triple bond",
		xpms[XPM_BOND_MAKETRIPLE],buildwin_click_change_mode);
	cs_toolseparator(toolbar);
	UAtoolitem[UA_DELBOND] = cs_radiotoolbtn(toolbar,t,tips,"Delete bonds",
		xpms[XPM_BOND_DELETE],buildwin_click_change_mode);
	// Tools
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,vbox,CS_START,TRUE,TRUE,0);
	t = cs_toolbtn(toolbar,tips,"Calculate bonding",xpms[XPM_BOND_CALCULATE],buildwin_click_bonding,"calcbonding");
	t = cs_toolbtn(toolbar,tips,"Clear bonding",xpms[XPM_BOND_CLEAR],buildwin_click_bonding,"clearbonding");
	t = cs_toolbtn(toolbar,tips,"Calculate bonding in selection",xpms[XPM_BOND_CALCULATESEL],buildwin_click_bonding,"selcalcbonding");
	t = cs_toolbtn(toolbar,tips,"Clear bonding in selection",xpms[XPM_BOND_CLEARSEL],buildwin_click_bonding,"selclearbonding");
	t = cs_toolbtn(toolbar,tips,"Augment bonding",xpms[XPM_BOND_AUGMENT],buildwin_click_bonding,"augbonding");

	//w = cs_toolbtn(toolbar,tips,"Bond all in selection",xpm_bond_bondallsel,buildwin_click_sel_bond_all);
	// ---- Bonding Sub-options
	hbox = cs_box(CS_HORIZONTAL,FALSE,2,vbox,CS_START,FALSE,TRUE,0);
	l = cs_label("Tolerance",hbox,CS_START,FALSE,TRUE,0);
	adj = cs_adjustment(prefs.get_bond_tolerance(),0.0,3.0,0.01,0.05);
	spin = cs_spin(adj,prefs.get_bond_tolerance(),2,hbox,CS_END,TRUE,TRUE,0,buildwin_change_bondtol,NULL);

	// Buttons at foot of window (OK etc.)
	vbox = cs_box(CS_VERTICAL,FALSE,0,mainbox,CS_END,FALSE,TRUE,2);
	cs_separator(CS_HORIZONTAL,vbox,CS_START,FALSE,TRUE,0);
	hbox = cs_box(CS_HORIZONTAL,FALSE,0,vbox,CS_START,FALSE,TRUE,0);
	b = cs_button("Close",hbox,CS_END,FALSE,TRUE,0,buildwin_click_closebtn,NULL);

	cs_showall(mainbox);
	dbg_end(DM_CALLS,"gui_gtk::atombuildwin_create");
}

