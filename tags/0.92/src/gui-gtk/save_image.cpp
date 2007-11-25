/*
	*** GTK save image window
	*** src/gui-gtk/save_image.cpp
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
#include <gdk-pixbuf/gdk-pixbuf-io.h>
#include <gdk-pixbuf/gdk-pixbuf-loader.h>
#include "gui-gtk/funcs.h"
#include "gui-gtk/gui-gtk.h"
#include "base/master.h"

// Variables
GtkFileChooser *dialog_saveimage;

void add_if_writable(GdkPixbufFormat *data, GSList **list)
{
	if (gdk_pixbuf_format_is_writable(data)) *list = g_slist_prepend(*list, data);
}

void gui_gtk::saveimagewin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::saveimagewin_create");
	GtkWidget *dialog;
	GtkFrame *frame;
	GtkTable *table;
	model *m;
	int mf;
	dialog_saveimage = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new ("Save Image",NULL,GTK_FILE_CHOOSER_ACTION_SAVE, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, NULL));
	#ifdef HAVE_GTK_NEW
		gtk_file_chooser_set_do_overwrite_confirmation(dialog_saveimage,TRUE);
	#endif

	// Add file filters to the dialog. This will be the list of writable formats
	// Grab list of formats that GDK can write to
	GSList *formats = gdk_pixbuf_get_formats();
	// Initialise a list in which to store the writable formats
	GSList *writable_formats = NULL;
	g_slist_foreach(formats,(void (*)(void*,void*))add_if_writable,&writable_formats);
	// Now, get the number of elements in the list and initialise our filter array
	int numformats = g_slist_length(writable_formats);
	printf("Num formats = %i\n",numformats);
	GtkFileFilter *saveimage_filters[numformats];
	for (int n=0; n<numformats; n++)
	{
		// Get the information of the format in the list
		gpointer format = g_slist_nth_data(writable_formats,n);
		//const char* ext = gdk_pixbuf_format_get_extensions(GdkPixbufFormat(format));
// TODO		saveimage_filters[n] = new_file_filter(filter_from_MF(model_format(n)),text_from_MF(model_format(n)));
		gtk_file_chooser_add_filter(dialog_saveimage,saveimage_filters[n]);
	}
	// Create frame with extra options...
	frame = cs_frame("General Options",NULL,CS_START,FALSE,FALSE,0);
	table = cs_table(2,2,TRUE,frame);
	//mlwin_calcbonding = cs_check_table("(Re)Calculate Bonding",prefs.model_bondonload,table,0,1,0,1,NULL);
	cs_showall(frame);
	//gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog),frame);
	//g_slist_free(formats);
	dbg_end(DM_NONE,"gui_gtk:saveimagewin_create");
}

void gui_gtk::call_saveimage_dialog()
{
	dbg_begin(DM_NONE,"gui_gtk::call_saveimage_dialog");
	// Set the initial name in the dialog...
	gtk_file_chooser_set_filename(dialog_saveimage,"screenshot.bmp");
	if (gtk_dialog_run(GTK_DIALOG(dialog_saveimage)) == GTK_RESPONSE_ACCEPT)
	{
		string filename = gtk_file_chooser_get_filename(dialog_saveimage);
		// Get the filename and current filter to determine the file type
		string selected_file = gtk_file_chooser_get_filename(dialog_saveimage);
		GtkFileFilter *filter = gtk_file_chooser_get_filter(dialog_saveimage);
	}
	gtk_widget_hide(GTK_WIDGET(dialog_saveimage));

	gui.refresh();
	dbg_end(DM_CALLS,"gui_gtk::call_saveimage_dialog");
}
