/*
	*** GTK user dialog
	*** src/gui-gtk/dialog.cpp
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

#include "gui-gtk/gui-gtk.h"
#include <gtk/gtk.h>
#include "parse/parser.h"
#include "base/debug.h"

// Dialog with message and at least one buttons
// Response ID is the integer order of the buttons
int gui_gtk::user_question(const char *text, const char *buttons)
{
	dbg_begin(DM_CALLS,"gui_gtk::user_question");
	// Create dialog
	GtkDialog *dialog;
	GtkWidget *w;
	dialog = GTK_DIALOG(gtk_dialog_new());
	// Add label
	w = gtk_label_new(text);
	gtk_container_add(GTK_CONTAINER(dialog->vbox),w);
	// Add on buttons
	parser.get_args_delim(buttons,PO_USEQUOTES);
	for (int n=0; n<parser.get_nargs(); n++) w = gtk_dialog_add_button(dialog,parser.argc(n),n);
	int result = gtk_dialog_run(dialog);
	printf("Dialog returned %i\n",result);
	gtk_widget_destroy(GTK_WIDGET(dialog));
	dbg_end(DM_CALLS,"gui_gtk::user_question");
	return result;
}
