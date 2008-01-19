/*
	*** GTK save field window
	*** src/gui-gtk/save_field.cpp
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
#include "parse/filter.h"
#include "parse/temp_dlpfield.h"

// Variables
GtkFileChooser *dialog_savefield;

void gui_gtk::savefieldwin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::savefieldwin_create");
	GtkFrame *frame;
	GtkTable *table;
	GtkFileFilter *filter;
	int n;
	dialog_savefield = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new ("Save Forcefield Specification",NULL,GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL));
	#ifdef HAVE_GTK_NEW
	gtk_file_chooser_set_do_overwrite_confirmation(dialog_savefield,TRUE);
	#endif
	gtk_file_chooser_set_current_folder(dialog_savefield,master.workdir.get());
	//gtk_file_chooser_set_current_name(dialog_savemodel,master.get_currentmodel()->get_filename());
	//else gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog), filename_for_existing_document);

	// Add file filters to the dialog...
	for (n=0; n<master.filters[FT_FIELD_EXPORT].size(); n++)
		gtk_file_chooser_add_filter(dialog_savefield,filters[FT_FIELD_EXPORT][n]);

	// Create frame with extra options...
	//frame = cs_frame("Save Options",NULL,CS_START,FALSE,FALSE,0);
	//table = cs_table(2,2,TRUE,frame);
	//mlwin_calcbonding = cs_check_table("(Re)Calculate Bonding",prefs.model_bondonload,table,0,1,0,1,NULL);
	//cs_showall(frame);
	//gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog),frame);
	dbg_end(DM_CALLS,"gui_gtk::savefieldwin_create");
}

void gui_gtk::call_savefield_dialog()
{
	dbg_begin(DM_CALLS,"gui_gtk::call_savefield_dialog");
	int n;
	model *m;
	if (gtk_dialog_run(GTK_DIALOG(dialog_savefield)) == GTK_RESPONSE_ACCEPT)
	{
		// Get the filename and current filter to determine the save file type
		GtkFileFilter *filefilter = gtk_file_chooser_get_filter(dialog_savefield);
		for (n=0; n<master.filters[FT_FIELD_EXPORT].size(); n++)
			if (filefilter == filters[FT_FIELD_EXPORT][n]) break;
		// Set filename of model
		m = master.get_currentmodel();
		master.filters[FT_FIELD_EXPORT][n]->export_field(m, gtk_file_chooser_get_filename(dialog_savefield));
		//savedlpfield(gtk_file_chooser_get_filename(dialog_savefield),m);
	}
	gtk_widget_hide(GTK_WIDGET(dialog_savefield));
	gui.refresh();
	dbg_end(DM_CALLS,"gui_gtk::call_savefield_dialog");
}
