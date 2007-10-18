/*
	*** GTK main window
	*** src/gui-gtk/main.cpp

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
#include <gtk/gtkgl.h>
#include <gdk/gdkkeysyms.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "gui-gtk/treeview.h"
#include "model/model.h"
#include "base/master.h"
#include "base/prefs.h"
#include "base/sysfunc.h"
#include "gui/canvas.h"

// Variables
GtkWidget *statustext;
GtkScrolledWindow *textscroll;
GtkTextMark *buffermark;
GtkEntry *cmdentry;
int logonmousedown;

/*
// General Delete.Event and Program Termination handler
*/

void gui_gtk::close_application()
{
	// Perform some checks before we destroy the app.
	// TODO
	gtk_main_quit();
}

gint mainwin_delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
	// 'Delete Event' handler, called when we try to close the window.
	gtk_main_quit();	// Terminate the GTK application.
	return FALSE;		// FALSE = do NOT destroy window. TRUE = DO destroy window.
}

/*
// Status TextView Routines
*/

void gui_gtk::print_message(const char *message)
{
	if (!does_exist) return;
	// Append the supplied string to the textview store.
	GtkTextIter enditer;
	// Insert the text at the end of the buffer
	gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(statusbuf),&enditer);
	gtk_text_buffer_insert_with_tags(GTK_TEXT_BUFFER(statusbuf),&enditer,message,-1,msgtag,NULL);
	// Scroll the textview to the end of the inserted text
	gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(statustext),buffermark,0.0,0,0.0,1.0);
}

void mainwin_textview_clear()
{
	// Clear the contents of the textview.
	GtkTextIter enditer, startiter;
	gtk_text_buffer_get_start_iter(GTK_TEXT_BUFFER(gui.statusbuf),&startiter);
	gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(gui.statusbuf),&enditer);
	gtk_text_buffer_delete(GTK_TEXT_BUFFER(gui.statusbuf),&startiter,&enditer);
}

/*
// Canvas callbacks
*/

void modelview_expose()
{

	gui.mainview.expose();
}

void modelview_configure()
{
	gui.mainview.configure();
}

void modelview_realize()
{
	gui.mainview.realize();
	canvas_master::globs.initialise();
}

/*
// ModelView mouse handlers and window callbacks
*/

gboolean modelview_buttondown(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
	// Handle button presses (button down) from the mouse
	dbg_begin(DM_CALLS,"modelview_buttondown");
	mouse_button button = mouse_button (event->button-1);		// To make compatible with our mb[]
	// Do the requested action as defined in the control panel, but only if another action
	// isn't currently in progress. Set the user_action based on the mouse button that sent
	// the signal, current selection / draw modes and key modifier states.
	// Preliminary check to see if RMB was pressed over an atom - if so , show the popup menu and exit.
	if (button == 2)
	{
		atom *tempi = master.get_currentmodel()->atom_on_screen(event->x,event->y);
		if (tempi != NULL)
		{
			gui.call_atompopup(widget,event,tempi);
			dbg_end(DM_CALLS,"modelview_buttondown");
			return TRUE;
		}
	}
	// If the left mouse button is double-clicked over an atom, show the atomlist window
	if (button == 0 && event->type == GDK_2BUTTON_PRESS)
	{
		atom *tempi = master.get_currentmodel()->atom_on_screen(event->x,event->y);
		if (tempi != NULL)
		{
			gui.show(GW_ATOMLIST);
			gui.atomwin_list_refresh();
			dbg_end(DM_CALLS,"modelview_buttondown");
			return TRUE;
		}
	}
	// Inform the master (!) and main canvas that a button action has occurred
	logonmousedown = master.get_currentmodel()->get_log(LOG_SELECTION);
	gui.mainview.inform_mousedown(button,event->x,event->y);
	dbg_end(DM_CALLS,"modelview_buttondown");
	return FALSE;
}

gboolean modelview_buttonup(GtkWidget *widget, GdkEventButton *event, gpointer data)
{
	// Handle button releases (button up) from the mouse
	mouse_button button = mouse_button (event->button-1);		// To make compatible with our mb[]
	// Finalize the requested action
	gui.mainview.inform_mouseup(button, event->x, event->y);
	if (logonmousedown != master.get_currentmodel()->get_log(LOG_SELECTION)) gui.atomwin_list_update_selection();
	gui.update_labels();
	return FALSE;
}

gboolean modelview_motion(GtkWidget *widget, GdkEventMotion *event, gpointer data)
{
	// Mouse motion handler.
	// While we're here, set the drawing area to have focus (for keypresses)
	gtk_widget_grab_focus(GTK_WIDGET(gui.modelview));
	// Tell the main canvas that the mouse has moved
	gui.mainview.inform_mousemove(event->x,event->y);
	return FALSE;
}

gboolean modelview_scroll(GtkWidget *widget, GdkEventScroll *event, gpointer data)
{
	// Handle mouse-wheel scroll events.
	if (event->direction == GDK_SCROLL_UP) gui.mainview.inform_scroll(TRUE);
	else gui.mainview.inform_scroll(FALSE);
	return FALSE;
}

gboolean modelview_keydown(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	gui.mainview.inform_keydown(gui.convert_to_KC(event->keyval));
	return FALSE;
}

gboolean modelview_keyup(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
	// Handle keypresses on the modelview
	gui.mainview.inform_keyup(gui.convert_to_KC(event->keyval));
	return FALSE;
}

/*
// Model List Callbacks / Updates
*/

void mainwin_modellist_changed(GtkTreeSelection *selection, gpointer data)
{
	// Called when a different model is selected in the ModelList
	dbg_begin(DM_CALLS,"mainwin_modellist_changed");
	GtkTreeModel *active_model;
	GtkTreeIter selected_row;
	model *m;
	if (gtk_tree_selection_get_selected(selection,&active_model,&selected_row))
		gtk_tree_model_get(active_model, &selected_row, ML_COL_MODEL,&m, -1);
	master.set_currentmodel(m);
	gui.select_model(m);
	dbg_end(DM_CALLS,"mainwin_modellist_changed");
}

void mainwin_modellist_name_edited(GtkCellRendererText *cell,gchar *path_string,gchar *new_text,gpointer user_data)
{
	// Name of the model has been edited
	dbg_begin(DM_CALLS,"mainwin_modellist_name_edited");
	master.get_currentmodel()->set_name(new_text);
	gui.modelmaster.update();
	dbg_end(DM_CALLS,"mainwin_modellist_name_edited");
}

/*
// Main Window Button Callbacks
*/

void mainwin_stylechange_local(GtkRadioToolButton *widget, gpointer data)
{
	gui.mainwin_stylechange(widget);
}

// Model Action Callbacks
void mainwin_click_modelaction(GtkButton *widget, gpointer data)
{
	string action = (char*) data;
	if (action == "togglelist")
	{
	        gui.mainwin_modellist_toggle_visible();
        	gui.mainview.do_projection();
	        gui.refresh();
	}
	else if (action == "add") master.add_model();
	else if (action == "remove") master.remove_model(master.get_currentmodel());
	else if (action == "toggletext")
	{
	        GTK_WIDGET_VISIBLE(textscroll) == TRUE ? cs_hide(textscroll) : cs_show(textscroll);
        	gui.mainview.do_projection();
	        gui.refresh();
	}
}

// Playback toolbar callbacks
void mainwin_click_playback(GtkButton *widget, gpointer data)
{
	dnchar action;
	action.set((char*) data);
	model *m = master.get_currentmodel();
	if (action == "seekfirst") m->seek_first_frame();
	else if (action == "seekprev") m->seek_previous_frame();
	else if (action == "playpause") 
	{
		m->render_from_frames();
		//if (m->traj.get_playing()) m->traj.set_playing(FALSE);
		//else m->traj.set_playing(TRUE);  TGAY
		gui.update_trajcontrols();
	}
	else if (action == "seeknext") m->seek_next_frame();
	else if (action == "seeklast") m->seek_last_frame();
	gui.update_labels();
	gui.refresh();
}

// Drawing style change
void gui_gtk::mainwin_stylechange(GtkRadioToolButton *widget)
{
	// The drawing style of the current model has been changed.
	// This routine is called (through the wrapper below) the main menu style list also.
	static bool CHANGING = FALSE;
	// If we are already changing then exit (to prevent infinite loop)
	if (CHANGING) return;
	dbg_begin(DM_CALLS,"gui_gtk::mainwin_stylechange");
	int n;
	CHANGING = TRUE;
	for (n=0; n<DS_NITEMS; n++) if (GTK_TOOL_ITEM(widget) == DSbuttons[n]) break;
	// Check if we're already using this style. If so, return
	if (prefs.get_static_style() == (draw_style) n)
	{
	        dbg_end(DM_CALLS,"gui_gtk::mainwin_stylechange");
		CHANGING = FALSE;
	        return;
	}
	// Otherwise, change the current draw style for both the active model and the canvas
	prefs.set_static_style((draw_style) n);
	// 'Click' the corresponding menu item
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(DSmenuitems[n]),TRUE);
	master.get_currentmodel()->log_change(LOG_VISUAL);
	master.get_currentmodel()->project_all();
	gui.refresh();
	CHANGING = FALSE;
	dbg_end(DM_CALLS,"gui_gtk::mainwin_stylechange");
}

// Command Entry Activated
void cmdentry_activate(GtkEntry *widget, gpointer data)
{
	// Grab text, parse in to script command and run 
	master.cmd_script.commands.clear();
	parser.get_args_delim(gtk_entry_get_text(widget), PO_DEFAULTS);
	if (master.cmd_script.cache_command()) master.cmd_script.run();
	gui.refresh();
	gtk_entry_set_text(widget,"");
}

/*
// GUI Creation
*/

void gui_gtk::mainwin_create()
{
	// Create the modelview window
	dbg_begin(DM_CALLS,"gui_gtk::mainwin_create");
	GObject *tobj;
	GtkMenuBar *mainmenu;
	GtkMenu *menu;
	GtkMenuItem *item;
	GtkBox *box, *mainboxh, *mainboxv, *botbox;
	GtkFrame *frame;
	GtkToolbar *toolbar, *subtoolbar;
	GtkToolItem *t;
	GtkWidget *paned;
	GtkCellRenderer *rend;
	char title[64];
	// Create the window and register some callbacks.
	windows[GW_MAIN] = GTK_WINDOW(gtk_window_new(GTK_WINDOW_TOPLEVEL));
	strcpy(title,"Aten (beta ");
	strcat(title,PACKAGE_VERSION);
	strcat(title,")");
	gtk_window_set_title(windows[GW_MAIN],title);
	g_signal_connect_swapped(G_OBJECT(windows[GW_MAIN]),"delete_event",G_CALLBACK(mainwin_delete_event),NULL);
	gtk_container_set_border_width(GTK_CONTAINER(windows[GW_MAIN]),2);

	// The main window contains a vbox in which live a menu, vpaned, and buttonbar at the bottom.
	mainboxv = cs_box(CS_VERTICAL,FALSE,0,GTK_CONTAINER(windows[GW_MAIN]));

	// Add an accelerator group to the window
	GtkAccelGroup *accel = gtk_accel_group_new();
	gtk_window_add_accel_group(windows[GW_MAIN],accel);


	// -- Main Menu
	mainmenu = mainmenu_create(accel);
	gtk_box_pack_start(GTK_BOX(mainboxv),GTK_WIDGET(mainmenu),FALSE,TRUE,0);

	// -- VPaned - Top half contains an hbox for the model window and other widgets, bottom half contains a scrolled window for
	// the text buffer.
	paned = gtk_vpaned_new();
	gtk_box_pack_start(GTK_BOX(mainboxv),paned,TRUE,TRUE,0);
	// ---- Horizontal box (drawing area and model list)
	mainboxh = GTK_BOX(gtk_hbox_new(FALSE,0));
	gtk_paned_pack1(GTK_PANED(paned),GTK_WIDGET(mainboxh),TRUE,FALSE);
	// ---- Scrolled window for text buffer
	textscroll = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new(NULL,NULL));
	gtk_scrolled_window_set_policy(textscroll,GTK_POLICY_AUTOMATIC,GTK_POLICY_ALWAYS);
	gtk_paned_pack2(GTK_PANED(paned),GTK_WIDGET(textscroll),FALSE,TRUE);


	// -- Horizontal box (in mainboxh) containing vertical toolbar, model list etc.
	box = cs_box(CS_HORIZONTAL,FALSE,0,mainboxh,CS_START,FALSE,TRUE,0);
	// ---- Toolbar
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_VERTICAL,mainboxh,CS_START,FALSE,TRUE,0);
	t = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TOGGLELIST],mainwin_click_modelaction,"togglelist");
	t = cs_toolbtn(toolbar,NULL,"",xpms[XPM_ADD],mainwin_click_modelaction,"add");
	t = cs_toolbtn(toolbar,NULL,"",xpms[XPM_REMOVE],mainwin_click_modelaction,"remove");
	t = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TOGGLELIST],mainwin_click_modelaction,"toggletext");
	cs_toolseparator(toolbar);
	DSbuttons[DS_STICK] = NULL;
	for (int n=0; n<DS_NITEMS; n++)
		DSbuttons[n] = cs_radiotoolbtn(toolbar,DSbuttons[DS_STICK],NULL,"", xpms[XPM_DS_STICK+n],mainwin_stylechange_local);
	gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(DSbuttons[prefs.get_static_style()]),TRUE);

	// ---- Trajectory playback buttons (for convenience, play/pause is last in array)
	cs_toolseparator(toolbar);
	trajcontrols[0] = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TRAJ_START],mainwin_click_playback,"seekfirst");
	trajcontrols[1] = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TRAJ_REWIND],mainwin_click_playback,"seekprev");
	trajcontrols[4] = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TRAJ_PLAYPAUSE],mainwin_click_playback,"playpause");
	//master.trajcontrols[3] = cs_toolbtn(toolbar,NULL,"",pixbufs.xpm_traj_stop,mainwin_click_stop);
	trajcontrols[2] = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TRAJ_FF],mainwin_click_playback,"seeknext");
	trajcontrols[3] = cs_toolbtn(toolbar,NULL,"",xpms[XPM_TRAJ_END],mainwin_click_playback,"seeklast");


	// -- Drawing Area (last into main hbox)
	modelview = gtk_drawing_area_new();
	gtk_box_pack_start(GTK_BOX(mainboxh),modelview,TRUE,TRUE,0);
	gtk_widget_set_size_request(modelview,400,400);
	gui.mainview.set_widget(modelview);
	gui.mainview.set_name("mainview");
	gtk_widget_add_events(modelview,GDK_VISIBILITY_NOTIFY_MASK  | 
		GDK_BUTTON_PRESS_MASK		|	GDK_BUTTON_RELEASE_MASK |
		GDK_POINTER_MOTION_MASK		|	GDK_KEY_PRESS_MASK |
		GDK_KEY_RELEASE_MASK		|	GDK_SCROLL_MASK);
	GTK_WIDGET_SET_FLAGS(GTK_WIDGET(modelview),GTK_CAN_FOCUS);
	tobj = G_OBJECT(modelview);
	// ---- Attach the necessary callbacks to the rendering canvas
	g_signal_connect(tobj,"realize",G_CALLBACK(modelview_realize),NULL);
	g_signal_connect(tobj,"configure_event",G_CALLBACK(modelview_configure),NULL);
	g_signal_connect(tobj,"expose_event",G_CALLBACK(modelview_expose),NULL);
	g_signal_connect(tobj,"motion_notify_event",G_CALLBACK(modelview_motion),NULL);
	g_signal_connect(tobj,"button_press_event",G_CALLBACK(modelview_buttondown),NULL);
	g_signal_connect(tobj,"button_release_event",G_CALLBACK(modelview_buttonup),NULL);
	g_signal_connect(tobj,"scroll_event",G_CALLBACK(modelview_scroll),NULL);
	g_signal_connect(tobj,"key_press_event",G_CALLBACK(modelview_keydown),NULL);
	g_signal_connect(tobj,"key_release_event",G_CALLBACK(modelview_keyup),NULL);

	
	// -- Model List (end of main hbox)
	cs_separator(CS_VERTICAL,mainboxh,CS_START,FALSE,TRUE,0);
	// We use an hbox to contain the listview and vertical button box
	mainwin_modellist_box = cs_box(CS_HORIZONTAL,FALSE,0,mainboxh,CS_END,FALSE,TRUE,0);
	// ---- Listview
	// Add a child list to the modelmaster
	mainwin_modellist = modelmaster.add_child();
	// Put treeview in a scrolled window
	cs_treeview(mainwin_modellist,mainwin_modellist_box,CS_START,TRUE,TRUE,0,mainwin_modellist_changed);
	// Add columns
	rend = cs_column(mainwin_modellist,"C","pixbuf",ML_COL_CELLFLAG);
	rend = cs_column(mainwin_modellist,"M","pixbuf",ML_COL_MODFLAG);
	rend = cs_column(mainwin_modellist,"Model","text",ML_COL_NAME);
	g_object_set(rend,"editable",TRUE,NULL);
	g_signal_connect(rend,"edited",(GCallback)mainwin_modellist_name_edited, NULL);
	mainwin_modellist->set_size(200,300);

	// -- Command Entry
	box = cs_box(CS_HORIZONTAL,TRUE,0,mainboxv,CS_END,FALSE,TRUE,0);
	cmdentry = cs_entry(box,CS_START,TRUE,TRUE,0,cmdentry_activate);

	// -- Text Buffer
	statustext = gtk_text_view_new();
	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(statustext), GTK_WRAP_WORD);
	gtk_text_view_set_editable(GTK_TEXT_VIEW(statustext), FALSE);
	gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(statustext), FALSE);
	statusbuf = gtk_text_view_get_buffer(GTK_TEXT_VIEW(statustext));
	GtkTextIter textiter;
	gtk_text_buffer_get_end_iter(GTK_TEXT_BUFFER(statusbuf),&textiter);
	buffermark = gtk_text_buffer_create_mark(GTK_TEXT_BUFFER(statusbuf),"bufferend",&textiter,FALSE);
	// Create the font tag for the textview
	msgtag = gtk_text_buffer_create_tag(statusbuf,"font","font",prefs.get_msg_font(),NULL);
	gtk_container_add(GTK_CONTAINER(textscroll),statustext);


	// -- Button toolbar
	frame = cs_frame("",mainboxv,CS_END,FALSE,TRUE,0);
	botbox = cs_box(CS_HORIZONTAL,FALSE,0,frame);
	toolbar = cs_toolbar(GTK_TOOLBAR_ICONS,CS_HORIZONTAL,botbox,CS_START,TRUE,TRUE,0);
	// ---- Drawing style buttons

	cs_toolseparator(toolbar);

	label[W_LBL_TRAJ] = cs_toollabel(toolbar,"(0 / 0)");
	// ---- Label for model information
	cs_toolseparator(toolbar);
	label[W_LBL_NATOMS] = cs_toollabel(toolbar,"0 Atoms, ");
	label[W_LBL_MASS] = cs_toollabel(toolbar,"0.0 g ");
	label[W_LBL_CELL] = cs_toollabel(toolbar,"(0.0 g/cm**3) ");

	// Finally, add an icon to the window...
	gtk_window_set_icon(windows[GW_MAIN],xpms[XPM_CAESIOUS]);
	cs_showall(windows[GW_MAIN]);
	dbg_end(DM_CALLS,"gui_gtk::mainwin_create");
}
