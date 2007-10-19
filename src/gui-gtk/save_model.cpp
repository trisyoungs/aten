/*
	*** GTK save model window
	*** src/gui-gtk/save_model.cpp
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
#include "file/filter.h"

// Variables
GtkFileChooser* dialog_savemodel;

void gui_gtk::savemodelwin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::savemodelwin_create");
	GtkWidget *dialog;
	GtkFrame *frame;
	GtkTable *table;
	int n;
	model *m;
	bool success;
	dialog_savemodel = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new ("Save Model",NULL,GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL));
	#ifdef HAVE_GTK_NEW
	gtk_file_chooser_set_do_overwrite_confirmation(dialog_savemodel,TRUE);
	#endif
	// Set the initial name in the dialog...
	gtk_file_chooser_set_current_folder(dialog_savemodel,master.workdir.get());
	gtk_file_chooser_set_current_name(dialog_savemodel,master.get_currentmodel()->get_filename());
	//else gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (dialog), filename_for_existing_document);

	// Add file filters to the dialog...
	for (n=0; n<master.filters[FT_MODEL_EXPORT].size(); n++)
		gtk_file_chooser_add_filter(dialog_savemodel,filters[FT_MODEL_EXPORT][n]);

	// Create frame with extra options...
	//frame = cs_frame("General Options",NULL,CS_START,FALSE,FALSE,0);
	//table = cs_table(2,2,TRUE,frame);
	//cs_showall(frame);
	//gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog),frame);
	dbg_end(DM_CALLS,"gui_gtk::savemodelwin_create");
}

void gui_gtk::call_savemodel_dialog()
{
	// 'Run' the widget and get the response
	dbg_begin(DM_CALLS,"gui_gtk::call_savemodel_dialog");
	int n;
	model *m;
	if (gtk_dialog_run(GTK_DIALOG(dialog_savemodel)) == GTK_RESPONSE_ACCEPT)
	{
		// Get the filename and current filter to determine the save file type
		GtkFileFilter *filefilter = gtk_file_chooser_get_filter(dialog_savemodel);
		for (n=0; n<master.filters[FT_MODEL_EXPORT].size(); n++)
			if (filefilter == filters[FT_MODEL_EXPORT][n]) break;
		// Set filename of model
		m = master.get_currentmodel();
		m->set_filter(master.filters[FT_MODEL_EXPORT][n]);
		m->set_filename(gtk_file_chooser_get_filename(dialog_savemodel));
		master.filters[FT_MODEL_EXPORT][n]->export_model(m);
	}
	gtk_widget_hide(GTK_WIDGET(dialog_savemodel));
	gui.refresh();
	dbg_end(DM_CALLS,"gui_gtk::call_savemodel_dialog");
}
