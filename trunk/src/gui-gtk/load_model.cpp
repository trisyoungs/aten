/*
	*** GTK load model window
	*** src/gui-gtk/load_model.cpp
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

#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"
#include "base/prefs.h"
#include "parse/filter.h"

// Variables
GtkFileChooser *dialog_loadmodel;

void loadmodelwin_bondonload_changed(GtkComboBox *widget, gpointer data)
{
	prefs.set_bond_on_load((file_prefswitch) (gtk_combo_box_get_active(widget)-1));
}

void loadmodelwin_zmap_changed(GtkComboBox *widget, gpointer data)
{
	prefs.set_zmapping((zmap_type) gtk_combo_box_get_active(widget));
}

void loadmodelwin_fold_changed(GtkCheckButton *widget, gpointer data)
{
	printf("Removed...\n");
	//prefs.set_fold_on_load((file_prefswitch) gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void loadmodelwin_packonload_changed(GtkComboBox *widget, gpointer data)
{
	prefs.set_pack_on_load((file_prefswitch) (gtk_combo_box_get_active(widget)-1));
}

void loadmodelwin_centre_changed(GtkCheckButton *widget, gpointer data)
{
	printf("Removed...\n");
	//prefs.set_centre_on_load(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void loadmodelwin_loadall_changed(GtkCheckButton *widget, gpointer data)
{
	prefs.set_load_all_coords(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)));
}

void gui_gtk::loadmodelwin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::loadmodelwin_create");
	GtkFrame *frame;
	GtkTable *table;
	GtkLabel *l;
	int n;
	model *m;
	const char *infilter[3] = { "As in Filter", "Inhibit", "Force" };
	dialog_loadmodel = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new("Open Model", NULL, GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL));
	// Add file filters to the dialog...
	for (n=0; n<master.filters[FT_MODEL_IMPORT].size(); n++)
		gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(dialog_loadmodel),filters[FT_MODEL_IMPORT][n]);
	// Create frame with extra options...
	frame = cs_frame("General Options",NULL,CS_START,FALSE,FALSE,0);
	table = cs_table(2,5,TRUE,frame);
	// Bonding
	l = cs_label("Bonding",table,0,1,0,1);
	combo[W_COM_BONDONLOAD] = cs_combo(infilter,3,prefs.get_bond_on_load()+1,table,1,2,0,1,loadmodelwin_bondonload_changed);
	// ZMapping
	l = cs_label("Name Mapping",table,0,1,1,2);
	combo[W_COM_ZMAPTYPE] = cs_combo(get_ZM_keywords(),ZM_NITEMS,prefs.get_zmapping(),table,1,2,1,2,loadmodelwin_zmap_changed);
	// Load all frames
	check[W_CHK_LOADALLFRAMES] = cs_check("Load all frames",prefs.load_all_coords(),table,2,3,0,1,loadmodelwin_loadall_changed,NULL);
	// Packing
	l = cs_label("Packing",table,3,4,0,1);
	combo[W_COM_PACKONLOAD] = cs_combo(infilter,3,prefs.get_pack_on_load()+1,table,4,5,0,1,loadmodelwin_packonload_changed);
	// Fold / pack / centre 
	check[W_CHK_FOLDONLOAD] = cs_check("Fold atoms",prefs.get_fold_on_load(),table,2,3,1,2,loadmodelwin_fold_changed,NULL);
	check[W_CHK_CENTREONLOAD] = cs_check("Centre model",prefs.get_centre_on_load(),table,3,4,1,2,loadmodelwin_centre_changed,NULL);
	cs_showall(frame);
	gtk_file_chooser_set_extra_widget(dialog_loadmodel,GTK_WIDGET(frame));
	dbg_end(DM_CALLS,"gui_gtk::loadmodelwin_create");
}

void gui_gtk::call_loadmodel_dialog()
{
	dbg_begin(DM_CALLS,"gui_gtk::call_loadmodel_dialog");
	// 'Run' the widget and get the response
	gtk_widget_show(GTK_WIDGET(dialog_loadmodel));
	int n;
	model *m;
	if (gtk_dialog_run(GTK_DIALOG(dialog_loadmodel)) == GTK_RESPONSE_ACCEPT)
	{
		GtkFileFilter *activefilter;
		// Get the filename and current filter to determine the file type
		activefilter = gtk_file_chooser_get_filter(dialog_loadmodel);
		for (n=0; n<master.filters[FT_MODEL_IMPORT].size(); n++)
			if (activefilter == gui.filters[FT_MODEL_IMPORT][n]) break;
		m = master.filters[FT_MODEL_IMPORT][n]->import_model(gtk_file_chooser_get_filename(dialog_loadmodel));
	}
	gtk_widget_hide(GTK_WIDGET(dialog_loadmodel));
	gui.refresh();
	dbg_end(DM_CALLS,"gui_gtk::call_loadmodel_dialog");
}
