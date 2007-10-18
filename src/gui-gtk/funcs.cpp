/*
	*** GTK creation functions
	*** src/gui-gtk/funcs.cpp

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
#include "base/master.h"
#include "base/constants.h"
#include "file/parse.h"

/* 
// Packing / Visibility Functions
*/
void cs_pack(gpointer w, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_pack");
	GtkWidget *widget = GTK_WIDGET(w);
	pos == CS_START ? gtk_box_pack_start(box,widget,expand,fill,pad)
			: gtk_box_pack_end(box,widget,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_pack");
}

void cs_showall(gpointer w)
	{ gtk_widget_show_all(GTK_WIDGET(w)); }
void cs_hide(gpointer w)
	{ gtk_widget_hide(GTK_WIDGET(w)); }
void cs_show(gpointer w)
	{ gtk_widget_show(GTK_WIDGET(w)); }

/*
// Window
*/
GtkWindow *cs_subwindow(const char *title, bool modal, int (*cb)(GtkWidget*,GdkEvent*,gpointer))
{
	// Create a new window
	dbg_begin(DM_CALLS,"cs_subwindow");
	GtkWidget *newwin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(newwin),title);
	gtk_window_set_transient_for(GTK_WINDOW(newwin),GTK_WINDOW(gui.windows[GW_MAIN]));
	if (modal) gtk_window_set_modal(GTK_WINDOW(newwin),TRUE);
	gtk_window_set_position(GTK_WINDOW(newwin),GTK_WIN_POS_CENTER_ON_PARENT);
	gtk_window_set_skip_taskbar_hint(GTK_WINDOW(newwin),TRUE);
	g_signal_connect_swapped(G_OBJECT(newwin),"delete_event",G_CALLBACK(cb),NULL);
	gtk_container_set_border_width(GTK_CONTAINER(newwin),2);
	dbg_end(DM_CALLS,"cs_subwindow");
	return GTK_WINDOW(newwin);
}

/*
// Labels
*/
GtkLabel *cs_label(const char *label, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	// Create a label widget and add it to the box specified
	dbg_begin(DM_CALLS,"cs_label");
	GtkWidget *newlabel = gtk_label_new(label);
	cs_pack(newlabel,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_label");
	return GTK_LABEL(newlabel);
}

GtkLabel *cs_label(const char *label, GtkTable *table, int l, int r, int t, int b)
{
	// Create a label widget and add it to the table cell specified
	dbg_begin(DM_CALLS,"cs_label_table");
	GtkWidget *newlabel = gtk_label_new(label);
	gtk_table_attach_defaults(table,newlabel,l,r,t,b);
	dbg_end(DM_CALLS,"cs_label_table");
	return GTK_LABEL(newlabel);
}

void cs_set_label_pango(const char *s, GtkLabel *label)
{
	// Sets Pango-formatted text for the specified label
	//gchar *st = "<span foreground=\"blue\"><b>Bold</b> <u>is</u> <i>beautiful</i></span>";
	gchar *plaintext;
	PangoAttrList *attrList;
	pango_parse_markup(s,-1,0,&attrList,&plaintext,NULL,NULL);
	gtk_label_set_text(label,plaintext);
	gtk_label_set_attributes(label,attrList);
}

/*
// Standard Buttons
*/
GtkButton *cs_button(const char *label, GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_button");
	GtkWidget *newbutton = gtk_button_new_with_label(label);
	cs_pack(newbutton,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newbutton,"clicked",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_button");
	return GTK_BUTTON(newbutton);
}

GtkButton *cs_button(const char *label, GtkTable *table, int l, int r, int t, int b, void (*cb)(GtkButton*,void*),const char *data)
{
	// Create a label widget and add it to the table cell specified
	dbg_begin(DM_CALLS,"cs_button_table");
	GtkWidget *newbutton = gtk_button_new_with_label(label);
	if (cb != NULL) g_signal_connect(newbutton,"clicked",G_CALLBACK(cb),(gpointer) data);
	gtk_table_attach_defaults(table,newbutton,l,r,t,b);
	dbg_end(DM_CALLS,"cs_button_table");
	return GTK_BUTTON(newbutton);
}

GtkButton *cs_pixbutton(GdkPixbuf *icon, GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkButton*,void*), const char* data)
{
	// Create a button using a pixbuf for its image
	dbg_begin(DM_CALLS,"cs_pixbutton");
	GtkWidget *newbutton = gtk_button_new();
	// Create box for image and label
	GtkBox *imagebox = GTK_BOX(gtk_hbox_new(TRUE,0));
	gtk_container_set_border_width(GTK_CONTAINER(imagebox),0);
	// Load the image
	GtkWidget *image = gtk_image_new_from_pixbuf(icon);
	// Pack the image and label into the box
	cs_pack(image,imagebox,CS_START,TRUE,TRUE,0);
	gtk_widget_show(image);
	gtk_container_add(GTK_CONTAINER(newbutton),GTK_WIDGET(imagebox));
	cs_pack(newbutton,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newbutton,"clicked",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_pixbutton");
	return GTK_BUTTON(newbutton);
}

GtkButton *cs_pixbutton(GdkPixbuf *icon, GtkTable *table, int l, int r, int t, int b, void (*cb)(GtkButton*,void*),const char *data)
{
	// Create a button using a pixbuf for its image
	dbg_begin(DM_CALLS,"cs_pixbutton");
	GtkWidget *newbutton = gtk_button_new();
	// Create box for image and label
	GtkBox *imagebox = GTK_BOX(gtk_hbox_new(TRUE,0));
	gtk_container_set_border_width(GTK_CONTAINER(imagebox),0);
	// Load the image
	GtkWidget *image = gtk_image_new_from_pixbuf(icon);
	// Pack the image and label into the box
	cs_pack(image,imagebox,CS_START,TRUE,TRUE,0);
	cs_show(image);
	gtk_container_add(GTK_CONTAINER(newbutton),GTK_WIDGET(imagebox));
	gtk_table_attach_defaults(table,newbutton,l,r,t,b);
	if (cb != NULL) g_signal_connect(newbutton,"clicked",G_CALLBACK(cb),(gpointer) data);
	dbg_end(DM_CALLS,"cs_pixbutton");
	return GTK_BUTTON(newbutton);
}

/*
// Menus
*/
// -- Main menu shell
GtkMenu *cs_menushell(const char *label, GtkMenuBar *menubar)
{
	dbg_begin(DM_CALLS,"cs_menushell");
	GtkWidget *newmenu = gtk_menu_item_new_with_mnemonic(label);
	GtkWidget *shell = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(newmenu),GTK_WIDGET(shell));
	gtk_menu_bar_append(menubar,newmenu);
	dbg_end(DM_CALLS,"cs_menushell");
	return GTK_MENU(shell);
}

// -- Menu subshell
GtkMenu *cs_menusubshell(GtkMenuItem *parentitem)
{
	dbg_begin(DM_CALLS,"cs_menusubshell");
	GtkWidget *newsubshell = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(parentitem),newsubshell);
	dbg_end(DM_CALLS,"cs_menusubshell");
	return GTK_MENU(newsubshell);
}

// -- Standard menu item
GtkMenuItem *cs_menuitem(const char *label, GtkMenu *shell, GtkAccelGroup *accel, guint key, GdkModifierType mod, void (*cb)(GtkMenuItem*,void*), const char *data)
{
	// Adds a menu item to the specified shell, with an accelerator if specified
	dbg_begin(DM_CALLS,"cs_menuitem");
	GtkWidget *newitem;
	// Check the label for the '_' character, then create the relevant menu item type.
	char *underscore = strrchr(label,'_');
	underscore == NULL	? newitem = gtk_menu_item_new_with_label(label)
				: newitem = gtk_menu_item_new_with_mnemonic(label);
	if (accel != NULL) gtk_widget_add_accelerator(newitem,"activate",accel,key,mod,GTK_ACCEL_VISIBLE);
	if (cb != NULL) g_signal_connect(G_OBJECT(newitem),"activate",G_CALLBACK(cb),(gpointer)data);
	gtk_menu_shell_append(GTK_MENU_SHELL(shell),newitem);
	dbg_end(DM_CALLS,"cs_menuitem");
	return GTK_MENU_ITEM(newitem);
}

// -- Radio menu item
GtkMenuItem *cs_radiomenuitem(const char *label, GtkMenu *shell, GtkMenuItem *parent, void (*cb)(GtkMenuItem*,void*), const char *data)
{
	// Adds a radio menu item to the specified shell.
	dbg_begin(DM_CALLS,"cs_radiomenuitem");
	GtkWidget *newitem;
	GSList *list;
	parent == NULL ? list = NULL : list = gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(parent));
	newitem = gtk_radio_menu_item_new_with_label(list,label);
	gtk_menu_shell_append(GTK_MENU_SHELL(shell),newitem);
	if (cb != NULL) g_signal_connect(G_OBJECT(newitem),"activate",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_radiomenuitem");
	return GTK_MENU_ITEM(newitem);
}

// -- Check menu item
GtkMenuItem *cs_checkmenuitem(const char *label, GtkMenu *shell, void (*cb)(GtkCheckMenuItem*,void*), const char *data)
{
	// Adds a radio menu item to the specified shell.
	dbg_begin(DM_CALLS,"cs_checkmenuitem");
	GtkWidget *newitem;
	newitem = gtk_check_menu_item_new_with_label(label);
	gtk_menu_shell_append(GTK_MENU_SHELL(shell),newitem);
	if (cb != NULL) g_signal_connect(G_OBJECT(newitem),"activate",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_checkmenuitem");
	return GTK_MENU_ITEM(newitem);
}

// -- Menu separator item
void cs_menuseparator(GtkMenu *shell)
{
	// Add a separator to the specified menu
	dbg_begin(DM_CALLS,"cs_menuseparator");
	GtkWidget *newseparator = gtk_separator_menu_item_new();
	gtk_menu_shell_append(GTK_MENU_SHELL(shell),newseparator);
	dbg_end(DM_CALLS,"cs_menuseparator");
}

/*
// Option Widgets - Checkboxes and Comboboxes
*/
// -- Standard Checkbox
GtkCheckButton *cs_check(const char *label, bool state, GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkCheckButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_check");
	GtkWidget *newcheck = gtk_check_button_new_with_label(label);
	cs_pack(newcheck,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newcheck,"clicked",G_CALLBACK(cb),(gpointer)data);
	if (state) gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(newcheck),TRUE);
	dbg_end(DM_CALLS,"cs_check");
	return GTK_CHECK_BUTTON(newcheck);
}

// -- Standard Checkbox (into table)
GtkCheckButton *cs_check(const char *label, bool state, GtkTable *table, int l, int r, int t, int b, void (*cb)(GtkCheckButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_check_table");
	GtkWidget *newcheck = gtk_check_button_new_with_label(label);
	gtk_table_attach_defaults(table,newcheck,l,r,t,b);
	if (cb != NULL) g_signal_connect(newcheck,"clicked",G_CALLBACK(cb),(gpointer)data);
	if (state) gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(newcheck),TRUE);
	dbg_end(DM_CALLS,"cs_check_table");
	return GTK_CHECK_BUTTON(newcheck);
}

// -- Radio Button
GtkRadioButton *cs_radio(GtkRadioButton *parent,const char *label,GtkBox *box,int pos,bool expand,bool fill,int pad,void (*cb)(GtkRadioButton*,void*))
{
	dbg_begin(DM_CALLS,"cs_radio");
	GSList *list;
	GtkWidget *newradio;
	parent == NULL ? list = NULL
		       : list = gtk_radio_button_get_group(GTK_RADIO_BUTTON(parent));
	label == "" ? newradio = gtk_radio_button_new(list)
		    : newradio = gtk_radio_button_new_with_label(list,label);
	cs_pack(newradio,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newradio,"clicked",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_radio");
	return GTK_RADIO_BUTTON(newradio);
}

// -- Radio Button
GtkRadioButton *cs_radio(GtkRadioButton *parent,const char *label,GtkTable *table,int l,int r, int t, int b,void (*cb)(GtkRadioButton*,void*))
{
	dbg_begin(DM_CALLS,"cs_radio");
	GSList *list;
	GtkWidget *newradio;
	parent == NULL ? list = NULL
		       : list = gtk_radio_button_get_group(parent);
	label == "" ? newradio = gtk_radio_button_new(list)
		    : newradio = gtk_radio_button_new_with_label(list,label);
	gtk_table_attach_defaults(table,newradio,l,r,t,b);
	if (cb != NULL) g_signal_connect(newradio,"clicked",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_radio");
	return GTK_RADIO_BUTTON(newradio);
}

// -- Combo box
GtkComboBox *cs_combo(const char **items, int nitems, int active, GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkComboBox*,void*))
{
	dbg_begin(DM_CALLS,"cs_combo");
	GtkComboBox *newcombo = GTK_COMBO_BOX(gtk_combo_box_new_text());
	for (int n=0; n<nitems; n++) gtk_combo_box_append_text(newcombo,items[n]);
	gtk_combo_box_set_active(newcombo,active);
	cs_pack(newcombo,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newcombo,"changed",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_combo");
	return GTK_COMBO_BOX(newcombo);
}
// -- Combo box (into table)
GtkComboBox *cs_combo(const char **items, int nitems, int active, GtkTable *table, int l, int r, int t, int b, void (*cb)(GtkComboBox*,void*))
{
	dbg_begin(DM_CALLS,"cs_combo_table");
	GtkComboBox *newcombo = GTK_COMBO_BOX(gtk_combo_box_new_text());
	for (int n=0; n<nitems; n++) gtk_combo_box_append_text(newcombo,items[n]);
	gtk_combo_box_set_active(newcombo,active);
	gtk_table_attach_defaults(table,GTK_WIDGET(newcombo),l,r,t,b);
	if (cb != NULL) g_signal_connect(newcombo,"changed",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_combo_table");
	return GTK_COMBO_BOX(newcombo);
}

/*
// Toolbars
*/
// Standard toolbar
GtkToolbar *cs_toolbar(GtkToolbarStyle style, int orient, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_toolbar");
	GtkToolbar *toolbar = GTK_TOOLBAR(gtk_toolbar_new());
	gtk_toolbar_set_style(toolbar,style);
	orient == CS_VERTICAL ? gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_VERTICAL)
				: gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_HORIZONTAL);
	cs_pack(toolbar,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_toolbar");
	return toolbar;
}
// Standard toolbar (into container)
GtkToolbar *cs_toolbar(GtkToolbarStyle style, int orient, GtkContainer *container)
{
	dbg_begin(DM_CALLS,"cs_toolbar_container");
	GtkToolbar *toolbar = GTK_TOOLBAR(gtk_toolbar_new());
	gtk_toolbar_set_style(toolbar,style);
	orient == CS_VERTICAL ? gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_VERTICAL)
				: gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_HORIZONTAL);
	gtk_container_add(container,GTK_WIDGET(toolbar));
	dbg_end(DM_CALLS,"cs_toolbar_container");
	return toolbar;
}
// Standard toolbar (into table)
GtkToolbar *cs_toolbar(GtkToolbarStyle style, int orient, GtkTable *table, int l, int r, int t, int b)
{
	dbg_begin(DM_CALLS,"cs_toolbar_table");
	GtkToolbar *toolbar = GTK_TOOLBAR(gtk_toolbar_new());
	gtk_toolbar_set_style(toolbar,style);
	orient == CS_VERTICAL ? gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_VERTICAL)
				: gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_HORIZONTAL);
	gtk_table_attach_defaults(table,GTK_WIDGET(toolbar),l,r,t,b);
	dbg_end(DM_CALLS,"cs_toolbar_table");
	return toolbar;
}
// Standard toolbar (not packed)
GtkToolbar *cs_toolbar(GtkToolbarStyle style, int orient)
{
	dbg_begin(DM_CALLS,"cs_toolbar");
	GtkToolbar *toolbar = GTK_TOOLBAR(gtk_toolbar_new());
	gtk_toolbar_set_style(toolbar,style);
	orient == CS_VERTICAL ? gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_VERTICAL)
				: gtk_toolbar_set_orientation(toolbar,GTK_ORIENTATION_HORIZONTAL);
	dbg_end(DM_CALLS,"cs_toolbar");
	return toolbar;
}
// Standard toolbar item
GtkToolItem *cs_toolbtn(GtkToolbar *toolbar,GtkTooltips *tips,const char *tip,GdkPixbuf *icon,void (*cb)(GtkButton*,void*),const char *data)
{
	dbg_begin(DM_CALLS,"cs_toolbtn");
	GtkWidget *iconw = gtk_image_new_from_pixbuf(icon);
	GtkToolItem *newitem = gtk_tool_button_new(iconw, NULL);
	gtk_toolbar_insert(toolbar, newitem, -1);
	if (tips != NULL) gtk_tool_item_set_tooltip(newitem,tips,tip,"Private Text");
	if (cb != NULL) g_signal_connect(newitem,"clicked",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_toolbtn");
	return newitem;
}
// Radio toolbar item
GtkToolItem *cs_radiotoolbtn(GtkToolbar *toolbar,GtkToolItem *parent,GtkTooltips *tips,const char *tip,GdkPixbuf *icon,void (*cb)(GtkRadioToolButton*,void*))
{
	dbg_begin(DM_CALLS,"cs_radiotoolbtn");
	GtkWidget *iconw = gtk_image_new_from_pixbuf(icon);
	GtkToolItem *newitem;
	if (parent != NULL)
		newitem = gtk_radio_tool_button_new_from_widget(GTK_RADIO_TOOL_BUTTON(parent));
	else
		newitem = gtk_radio_tool_button_new(NULL);
	gtk_tool_button_set_icon_widget(GTK_TOOL_BUTTON(newitem), iconw);
	gtk_toolbar_insert(toolbar, newitem, -1);
	if (tips != NULL) gtk_tool_item_set_tooltip(newitem,tips,tip,"Private Text");
	if (cb != NULL) g_signal_connect(newitem,"clicked",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_radiotoolbtn");
	return newitem;
}
// Radio toolbar item (text)
GtkToolItem *cs_textradiotoolbtn(GtkToolbar *toolbar,GtkToolItem *parent,GtkTooltips *tips,const char *tip,const char *lbl,void (*cb)(GtkRadioToolButton*,void*),const char *data)
{
	dbg_begin(DM_CALLS,"cs_textradiotoolbtn");
	GtkToolItem *newitem;
	if (parent != NULL)
		newitem = gtk_radio_tool_button_new_from_widget(GTK_RADIO_TOOL_BUTTON(parent));
	else
		newitem = gtk_radio_tool_button_new(NULL);
	gtk_tool_button_set_label(GTK_TOOL_BUTTON(newitem),lbl);
	gtk_toolbar_insert(toolbar, newitem, -1);
	gtk_tool_item_set_tooltip(newitem,tips,tip,"Private Text");
	if (cb != NULL) g_signal_connect(newitem,"clicked",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_textradiotoolbtn");
	return newitem;
}
// Menu toolbar item
GtkMenu *cs_menutoolbtn(GtkToolbar *toolbar,GtkTooltips *tips,const char *tip,GdkPixbuf *icon,void (*cb)(GtkButton*,void*))
{
	dbg_begin(DM_CALLS,"cs_menutoolbtn");
	GtkWidget *iconw = gtk_image_new_from_pixbuf(icon);
	GtkToolItem *newitem = gtk_menu_tool_button_new(iconw, NULL);
	gtk_toolbar_insert(toolbar, newitem, -1);
	if (tips != NULL) gtk_tool_item_set_tooltip(newitem,tips,tip,"Private Text");
	if (cb != NULL) g_signal_connect(newitem,"clicked",G_CALLBACK(cb),NULL);
	// Create and associate the menu
	GtkWidget *menu = gtk_menu_new();
	gtk_menu_tool_button_set_menu(GTK_MENU_TOOL_BUTTON(newitem),menu);
	dbg_end(DM_CALLS,"cs_menutoolbtn");
	return GTK_MENU(menu);
}
// Toolbar label
GtkLabel *cs_toollabel(GtkToolbar *toolbar, const char *lbl)
{
	GtkToolItem *newitem = gtk_tool_item_new();
	GtkWidget *label = gtk_label_new(lbl);
	gtk_container_add(GTK_CONTAINER(newitem),label);
	gtk_toolbar_insert(toolbar,newitem,-1);
	return GTK_LABEL(label);
}
// Toolbar separator
void cs_toolseparator(GtkToolbar *toolbar)
{
	dbg_begin(DM_CALLS,"cs_toolseparator");
	GtkToolItem *newsep = gtk_separator_tool_item_new();
	gtk_toolbar_insert(toolbar,newsep,-9999);
	dbg_end(DM_CALLS,"cs_toolseparator");
}

/*
// Boxes, Notebooks, Tables and Frames
*/
// -- Box
GtkBox *cs_box(int orientation, gboolean hom, int spacing, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_box");
	GtkWidget *newbox = (orientation == CS_VERTICAL ? gtk_vbox_new(hom,spacing) : gtk_hbox_new(hom,spacing));
	cs_pack(newbox,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_box");
	return GTK_BOX(newbox);
}

// -- Box (into container)
GtkBox *cs_box(int orientation, gboolean hom, int spacing, GtkContainer *container)
{
	dbg_begin(DM_CALLS,"cs_box_container");
	GtkWidget *newbox;
	orientation == CS_VERTICAL ? newbox = gtk_vbox_new(hom,spacing) : newbox = gtk_hbox_new(hom,spacing);
	gtk_container_add(container,newbox);
	dbg_end(DM_CALLS,"cs_box_container");
	return GTK_BOX(newbox);
}
GtkBox *cs_box(int o, gboolean h, int s, GtkFrame* w)
	{ return cs_box(o,h,s,GTK_CONTAINER(w)); }
GtkBox *cs_box(int o, gboolean h, int s, GtkWindow* w)
	{ return cs_box(o,h,s,GTK_CONTAINER(w)); }
GtkBox *cs_box(int o, gboolean h, int s, GtkBox* w)
	{ return cs_box(o,h,s,GTK_CONTAINER(w)); }

// -- Table
GtkTable *cs_table(int rows, int columns, bool hom, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_table");
	GtkWidget *newtable = gtk_table_new(rows,columns,hom);
	cs_pack(newtable,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_table");
	return GTK_TABLE(newtable);
}

// -- Table (into container)
GtkTable *cs_table(int rows, int columns, bool hom, GtkContainer *container)
{
	dbg_begin(DM_CALLS,"cs_table_container");
	GtkWidget *newtable = gtk_table_new(rows,columns,hom);
	gtk_container_add(container,newtable);
	dbg_end(DM_CALLS,"cs_table_container");
	return GTK_TABLE(newtable);
}
GtkTable *cs_table(int r, int c, bool h, GtkFrame* w)
	{ return cs_table(r,c,h,GTK_CONTAINER(w)); }
GtkTable *cs_table(int r, int c, bool h, GtkBox* w)
	{ return cs_table(r,c,h,GTK_CONTAINER(w)); }

// -- Labelled frame
GtkFrame *cs_frame(const char *label, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_frame");
	GtkFrame *newframe = GTK_FRAME(gtk_frame_new(NULL));
	GtkWidget *newlabel = gtk_label_new(label);
	if (label != "")
	{
		gtk_frame_set_label_widget(newframe,newlabel);
		gtk_label_set_use_markup(GTK_LABEL(newlabel),TRUE);
	}
	if (box != NULL) cs_pack(newframe,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_frame");
	return newframe;
}

// -- Labelled frame (into table)
GtkFrame *cs_frame(const char *label, GtkTable *table, int l, int r, int t, int b)
{
	dbg_begin(DM_CALLS,"cs_frame_table");
	GtkFrame *newframe = GTK_FRAME(gtk_frame_new(NULL));
	GtkWidget *newlabel = gtk_label_new(label);
	if (label != "")
	{
		gtk_frame_set_label_widget(newframe,newlabel);
		gtk_label_set_use_markup(GTK_LABEL(newlabel),TRUE);
	}
	if (table != NULL) gtk_table_attach_defaults(table,GTK_WIDGET(newframe),l,r,t,b);
	dbg_end(DM_CALLS,"cs_frame_table");
	return newframe;
}

// -- Notebook
GtkNotebook *cs_notebook(GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_notebook");
	GtkNotebook *newnotebook = GTK_NOTEBOOK(gtk_notebook_new());
	cs_pack(newnotebook,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_notebook");
	return newnotebook;
}

// -- Notebook page
GtkBox *cs_notebookpage(const char *label, GtkNotebook *notebook)
{
	dbg_begin(DM_CALLS,"cs_notebookpage");
	GtkWidget *newpage = gtk_hbox_new(FALSE,0);
	gtk_container_add(GTK_CONTAINER(notebook),newpage);
	GtkWidget *newlabel = gtk_label_new(label);
	gtk_notebook_set_tab_label(notebook,newpage,newlabel);
	dbg_end(DM_CALLS,"cs_notebookpage");
	return GTK_BOX(newpage);
}

/*
// Adjustment, Scale and Spin Buttons
*/
// Adjustment Widget
GtkObject *cs_adjustment(double value, double min, double max, double stepinc, double pageinc)
{
	dbg_begin(DM_CALLS,"cs_adjustment");
	GtkObject *newadj = gtk_adjustment_new(value,min,max,stepinc,pageinc,0.0);
	dbg_end(DM_CALLS,"cs_adjustment");
	return newadj;
}

// Horizontal scalebar
GtkScale *cs_scale(GtkObject *adj,GtkBox *box,int pos,bool expand,bool fill,int pad,void (*cb)(GtkRange*,GtkScrollType,gdouble,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_scale");
	GtkScale *newscale = GTK_SCALE(gtk_hscale_new(GTK_ADJUSTMENT(adj)));
	gtk_scale_set_value_pos(newscale,GTK_POS_RIGHT);
	cs_pack(newscale,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newscale,"value_changed",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_scale");
	return newscale;
}
GtkScale *cs_scale(GtkObject *adj,GtkTable *table,int l,int r, int t, int b,void (*cb)(GtkRange*,GtkScrollType,gdouble,void*),const char *data)
{
	dbg_begin(DM_CALLS,"cs_scale");
	GtkScale *newscale = GTK_SCALE(gtk_hscale_new(GTK_ADJUSTMENT(adj)));
	gtk_scale_set_value_pos(newscale,GTK_POS_RIGHT);
	gtk_table_attach_defaults(table,GTK_WIDGET(newscale),l,r,t,b);
	if (cb != NULL) g_signal_connect(newscale,"value_changed",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_scale");
	return newscale;
}

// Spin Widget
GtkSpinButton *cs_spin(GtkObject *adj,double step, int d, GtkBox *box,int pos,bool expand,bool fill,int pad,void (*cb)(GtkSpinButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_spin");
	GtkWidget *newspin = gtk_spin_button_new(GTK_ADJUSTMENT(adj),step,d);
	//gtk_scale_set_value_pos(GTK_SCALE(newspin),GTK_POS_RIGHT);
	cs_pack(newspin,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(newspin,"value_changed",G_CALLBACK(cb),(gpointer) data);
	dbg_end(DM_CALLS,"cs_spin");
	return GTK_SPIN_BUTTON(newspin);
}
// Spin Widget (into container)
GtkSpinButton *cs_spin(GtkObject *adj,double step, int d, GtkContainer *container,void (*cb)(GtkSpinButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_spin");
	GtkWidget *newspin = gtk_spin_button_new(GTK_ADJUSTMENT(adj),step,d);
	//gtk_scale_set_value_pos(GTK_SCALE(newspin),GTK_POS_RIGHT);
	gtk_container_add(GTK_CONTAINER(container),newspin);
	if (cb != NULL) g_signal_connect(newspin,"value_changed",G_CALLBACK(cb),(gpointer) data);
	dbg_end(DM_CALLS,"cs_spin");
	return GTK_SPIN_BUTTON(newspin);
}
// Spin Widget (into table)
GtkSpinButton *cs_spin(GtkObject *adj, double step, int d, GtkTable *table,int l, int r, int t, int b,void (*cb)(GtkSpinButton*,void*), const char *data)
{
	dbg_begin(DM_CALLS,"cs_spin_table");
	GtkWidget *newspin = gtk_spin_button_new(GTK_ADJUSTMENT(adj),step,d);
	gtk_table_attach_defaults(GTK_TABLE(table),newspin,l,r,t,b);
	if (cb != NULL) g_signal_connect(newspin,"value_changed",G_CALLBACK(cb),(gpointer)data);
	dbg_end(DM_CALLS,"cs_spin_table");
	return GTK_SPIN_BUTTON(newspin);
}

/*
// Treeviews
*/
GtkScrolledWindow *cs_treeview(treeview_list *tlist,GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkTreeSelection*,void*))
{
	// Creates a scrolled window for the treeview and packs it into the specified box
	dbg_begin(DM_CALLS,"cs_treeview");
	GtkScrolledWindow *scroll = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new(NULL,NULL));
	gtk_scrolled_window_set_policy(scroll,GTK_POLICY_AUTOMATIC,GTK_POLICY_ALWAYS);
	gtk_container_add(GTK_CONTAINER(scroll),GTK_WIDGET(tlist->get_treeview()));
	if (box != NULL) cs_pack(scroll,box,pos,expand,fill,pad);
	if (cb != NULL) g_signal_connect(tlist->get_selection(),"changed",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_treeview");
	return scroll;
}

// Listview Cell Renderer
GtkCellRenderer *cs_column(treeview_list *tlist, const char *title, const char *type, int id)
{
	dbg_begin(DM_CALLS,"cs_column");
	GtkCellRenderer *rend;
	// Assume that if type starts with a 't' it must be 'text'
	type[0] == 't' ? rend = gtk_cell_renderer_text_new() : rend = gtk_cell_renderer_pixbuf_new();
	gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(tlist->get_treeview()),-1,title,
		rend, type, id, NULL);
	dbg_end(DM_CALLS,"cs_column");
	return rend;
}

/*
// Misc
*/
// Text Entry Widget
GtkEntry *cs_entry(GtkBox *box, int pos, bool expand, bool fill, int pad, void (*cb)(GtkEntry*,void*))
{
	dbg_begin(DM_CALLS,"cs_entry");
	GtkWidget *newentry = gtk_entry_new();
	if (cb != NULL) g_signal_connect(G_OBJECT(newentry),"activate",G_CALLBACK(cb),NULL);
	cs_pack(newentry,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_entry");
	return GTK_ENTRY(newentry);
}
// Text Entry Widget (in table)
GtkEntry *cs_entry(GtkTable *table, int l, int r, int t, int b, void (*cb)(GtkEntry*,void*))
{
	dbg_begin(DM_CALLS,"cs_entry_table");
	GtkWidget *newentry = gtk_entry_new();
	gtk_table_attach_defaults(table,newentry,l,r,t,b);
	if (cb != NULL) g_signal_connect(G_OBJECT(newentry),"activate",G_CALLBACK(cb),NULL);
	dbg_end(DM_CALLS,"cs_entry_table");
	return GTK_ENTRY(newentry);
}
// Separator
void cs_separator(int orientation, GtkBox *box, int pos, bool expand, bool fill, int pad)
{
	dbg_begin(DM_CALLS,"cs_separator");
	GtkWidget *newseparator;
	orientation == CS_VERTICAL ? newseparator = gtk_vseparator_new()
				   : newseparator = gtk_hseparator_new();
	cs_pack(newseparator,box,pos,expand,fill,pad);
	dbg_end(DM_CALLS,"cs_separator");
}

// File Filter
GtkFileFilter *cs_filefilter(const char *pattern, const char *desc)
{
	GtkFileFilter *newfilter = gtk_file_filter_new();
	gtk_file_filter_add_pattern(newfilter,pattern);
	gtk_file_filter_set_name(newfilter,desc);
	return newfilter;
}

