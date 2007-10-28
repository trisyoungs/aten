/*
	*** User interface stub
	*** src/gui/gui.cpp
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

#include "base/master.h"
#include "gui/gui.h"

// External Declarations
#ifndef HAS_GUI
	gui_master gui;
#endif

// Constructor
gui_master::gui_master()
{
	#ifdef MEMDEBUG
		printf("Constructor : gui_master\n");
	#endif
	does_exist = FALSE;
	is_available = FALSE;
	NORENDER = FALSE;
}

// Destructor
gui_master::~gui_master()
{
	#ifdef MEMDEBUG
		printf("Destructor : gui_master\n");
	#endif
}

// Early doors commands
void gui_master::prepare()
{
	 if (is_available) printf("gui_master::prepare - Not defined.\n");
}

// Initialise and create GUI
void gui_master::run(int argc, char **argv)
{
	 if (is_available) printf("gui_master::run - Not defined.\n");
}

// Set GUI to reflect prefs
void gui_master::set_controls()
{
	 if (is_available) printf("gui_master::set_controls - Not defined.\n");
}

// Question user dialog
int gui_master::user_question(const char *a, const char *b)
{
	 if (is_available) printf("gui_master::user_question - Not defined.\n");
	return -1;
}

// Show window
void gui_master::show(gui_window gw)
{
	 if (is_available) printf("gui_master::show - Not defined.\n");
}

// Update trajectory controls
void gui_master::update_trajcontrols()
{
	 if (is_available) printf("gui_master::update_trajcontrols - Not defined.\n");
}

// Change msgbox font
void gui_master::change_msg_font(const char *font)
{
	 if (is_available) printf("gui_master::change_msg_font - Not defined.\n");
}

// Update labels
void gui_master::update_labels()
{
	 if (is_available) printf("gui_master::update_labels - Not defined.\n");
}

/*
// General GUI Routines
*/

// Add a message to the main window's message output box
void gui_master::print_message(const char *s)
{
	 if (is_available) printf("gui_master::print_message - Not defined.\n");
}

// Refresh main canvas
void gui_master::refresh()
{
	 if (is_available) printf("gui_master::refresh - Not defined.\n");
}

// Process GUI messages
void gui_master::process_events()
{
	 if (is_available) printf("gui_master::process_events - Not defined.\n");
}

// Terminate GUI
void gui_master::close_application()
{
	 if (is_available) printf("gui_master::close_application - Not defined.\n");
}

// Add model to list
void gui_master::add_model(model *m)
{
	 if (is_available) printf("gui_master::add_model - Not defined.\n");
}

// Remove model from list
void gui_master::remove_model(model *m)
{
	 if (is_available) printf("gui_master::remove_model - Not defined.\n");
}

// Select model in list and show in main/sub windows
void gui_master::select_model(model *m)
{
	 if (is_available) printf("gui_master::select_model - Not defined.\n");
}

// Add ff to list
void gui_master::add_ff(forcefield *ff)
{
	 if (is_available) printf("gui_master::add_ff - Not defined.\n");
}

// Remove ff from list
void gui_master::remove_ff(forcefield *ff)
{
	 if (is_available) printf("gui_master::remove_ff - Not defined.\n");
}

// Select ff in list
void gui_master::select_ff(forcefield *ff)
{
	 if (is_available) printf("gui_master::select_ff - Not defined.\n");
}

// Add surface to list
void gui_master::add_surface(surface *m)
{
	 if (is_available) printf("gui_master::add_surface - Not defined.\n");
}

// Remove surface from list
void gui_master::remove_surface(surface *m)
{
	 if (is_available) printf("gui_master::remove_surface - Not defined.\n");
}

// Select surface in list and show in main/sub windows
void gui_master::select_surface(surface *m)
{
	 if (is_available) printf("gui_master::select_surface - Not defined.\n");
}

// Create GUI file filters from filter list
void gui_master::init_filters()
{
	 if (is_available) printf("gui_master::init_filters- Not defined.\n");
}

// Instantiate a progress dialog
void gui_master::progress_create(const char *jobtitle, int stepstodo)
{
}

// Update the progress dialog
bool gui_master::progress_update(int currentstep)
{
}

// Terminate the progress dialog
void gui_master::progress_terminate()
{
}

// Instantiate text-based progress dialog
void gui_master::text_progress_create(const char *jobtitle, int stepstodo)
{
	// Reset the counters
	textprogress_stepstodo = stepstodo;
	textprogress_percent = 0;
	// Print out the empty progress indicator
	printf("--- %s\n", jobtitle);
	printf("Progress [-]                              (  0%%)");
}

// Update the text progress dialog
bool gui_master::text_progress_update(int currentstep)
{
	static char *twister = "-\\|/";
	static char *c = twister;
	static int n, ndots;
	static double dpercent;
	static int percent;
	// Work out percentage and print dots and spaces
	dpercent = double(currentstep) / double(textprogress_stepstodo);
	percent = int(dpercent * 100.0);
	ndots = int(dpercent * 30.0);
	dpercent *= 100.0;
	if (percent != textprogress_percent)
	{
		// Print the header
		printf("\rProgress [%c]",*c);
		// Increase the twoster character
		c ++;
		if (*c == '\0') c = twister;
		for (n=0; n<ndots; n++) printf(".");
		for (n=ndots; n<30; n++) printf(" ");
		// Lastly, print percentage
		printf("(%-3i%%)",percent);
		textprogress_percent = percent;
	}
}

// Terminate the text progress dialog
void gui_master::text_progress_terminate()
{
	printf("\n");
}
