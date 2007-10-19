/*
	*** GTK load forcefield window
	*** src/gui-gtk/load_ff.cpp
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
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"

// Variables
GtkFileChooser *dialog_loadff;
GtkFileFilter *loadff_filters[2];

void gui_gtk::loadffwin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::loadffwin_create");
	GtkWidget *dialog;
	GtkFrame *frame;
	GtkTable *table;
	//GtkWidget *mlwin_calcbonding;
	model *m;
	dialog_loadff = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new ("Open Forcefield",NULL,GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL));
	// Add file filters to the dialog...
	loadff_filters[0] = cs_filefilter("*.*","All files");
	loadff_filters[1] = cs_filefilter("*.ff","Forcefields");
	gtk_file_chooser_add_filter(dialog_loadff,loadff_filters[0]);
	gtk_file_chooser_add_filter(dialog_loadff,loadff_filters[1]);
	// Create frame with extra options...
	frame = cs_frame("General Options",NULL,CS_START,FALSE,FALSE,0);
	table = cs_table(2,2,TRUE,frame);
	//mlwin_calcbonding = cs_check_table("(Re)Calculate Bonding",prefs.model_bondonload,table,0,1,0,1,NULL);
	cs_showall(frame);
	gtk_file_chooser_set_extra_widget(dialog_loadff,GTK_WIDGET(frame));
	dbg_end(DM_CALLS,"gui_gtk::loadffwin_create");
}

void gui_gtk::call_loadff_dialog()
{
	dbg_begin(DM_CALLS,"gui_gtk::call_loadff_dialog");
	// 'Run' the widget and get the response
	if (gtk_dialog_run(GTK_DIALOG(dialog_loadff)) == GTK_RESPONSE_ACCEPT)
	{
		master.load_ff(gtk_file_chooser_get_filename(dialog_loadff));
	}
	gtk_widget_hide(GTK_WIDGET(dialog_loadff));
	gui.refresh();
	dbg_end(DM_CALLS,"gui_gtk::call_loadff_dialog");
}
