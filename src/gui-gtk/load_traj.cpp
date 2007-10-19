/*
	*** GTK load trajectory window
	*** src/gui-gtk/load_traj.cpp
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
#include "file/filter.h"
#include "base/master.h"
#include "base/prefs.h"

// Variables
GtkFileChooser *dialog_loadtraj;

void gui_gtk::loadtrajwin_create()
{
	// Creates the dialog and adds the extra widgets etc.
	dbg_begin(DM_CALLS,"gui_gtk::loadtrajwin_create");
	GtkFrame *frame;
	GtkTable *table;
	int n;
	dialog_loadtraj = GTK_FILE_CHOOSER(gtk_file_chooser_dialog_new("Open Trajectory", NULL, GTK_FILE_CHOOSER_ACTION_OPEN, GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, NULL));
	// Add file filters to the dialog...
	for (n=0; n<master.filters[FT_TRAJECTORY_IMPORT].size(); n++)
		gtk_file_chooser_add_filter(dialog_loadtraj,filters[FT_TRAJECTORY_IMPORT][n]);

	// Create frame with extra options...
	//frame = cs_frame("General Options",NULL,CS_START,FALSE,FALSE,0);
	//table = cs_table(2,2,TRUE,frame);
	//mlwin_calcbonding = cs_check_table("(Re)Calculate Bonding",prefs.model_bondonload,table,0,1,0,1,NULL);
	//cs_showall(frame);
	//gtk_file_chooser_set_extra_widget(GTK_FILE_CHOOSER(dialog),GTK_WIDGET(frame));
	dbg_end(DM_CALLS,"gui_gtk::loadtrajwin_create");
}

void gui_gtk::call_loadtraj_dialog()
{
	dbg_begin(DM_CALLS,"gui_gtk::call_loadtraj_dialog");
	// 'Run' the widget and get the response
	int n;
	if (gtk_dialog_run(GTK_DIALOG(dialog_loadtraj)) == GTK_RESPONSE_ACCEPT)
	{
		// Get the filename and current filter to determine the file type
		GtkFileFilter *filefilter = gtk_file_chooser_get_filter(dialog_loadtraj);
		for (n=0; n<master.filters[FT_TRAJECTORY_IMPORT].size(); n++)
			if (filefilter == filters[FT_TRAJECTORY_IMPORT][n]) break;
		//printf("N stopped at %i, nfilters was %i\n",n,master.trajfilters.size());
		if (master.get_currentmodel()->initialise_trajectory(gtk_file_chooser_get_filename(dialog_loadtraj), master.filters[FT_TRAJECTORY_IMPORT][n]))
		{
			master.get_currentmodel()->render_from_frames();
			gui.update_trajcontrols();
			gui.update_labels();
		}
	}
	cs_hide(dialog_loadtraj);

	dbg_end(DM_CALLS,"gui_gtk::call_loadtraj_dialog");
}
