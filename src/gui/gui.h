/*
	*** User interface stub
	*** src/gui/gui.h

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

#ifndef H_GUIMASTER_H
#define H_GUIMASTER_H

#include "gui/canvas.h"

// Application Windows
enum gui_window { GW_MAIN, GW_ATOMBUILDER, GW_ATOMLABEL, GW_ATOMLIST, GW_ATOMSELECT, GW_AVERAGEMOL, GW_COLOUR, GW_CELLEDIT, GW_CELLEXPAND, GW_CELLREPEAT, GW_CELLVIEW, GW_ENERGYSETUP, GW_EPSRSETUP, GW_FORCEFIELD, GW_FREEVOLUME, GW_LABELS, GW_LOADMODEL, GW_MCINSERT, GW_MCOPT, GW_MEASURE, GW_MINIMISER, GW_PERIODICTABLE, GW_POSITION, GW_PREFS, GW_PATTERN, GW_SAVEMODEL, GW_SDOPT, GW_SIMPOPT, GW_NITEMS };

// Forward definitions
class filter;
class model;
class forcefield;

// GUI Interface
// All functions here must be satisfied by the different GUIs, even if there is none.
class gui_master
{
	/*
	// Existence of GUI
	*/
	protected:
	// Is a GUI available?
	bool is_available;
	// Does the GUI exist (has it been created)
	bool does_exist;

	public:
	// Early doors functions
	virtual void prepare();
	// Initialises all aspects of the GUI and hands over control
	virtual void run(int, char**);
	// Returns if a GUI is available
	bool available() { return is_available; }
	// Returns if the GUI has been created
	bool exists() { return does_exist; }
	// Constructor // Destructor
	gui_master();
	~gui_master();

	/*
	// Basic Window Functions
	*/
	public:
	// Show specified window
	virtual void show(gui_window);
	// Set values of widgets from current prefs data
	virtual void set_controls();
	// Question user dialog
	virtual int user_question(const char*, const char*);

	/*
	// Main Canvas and Rendering
	*/
	protected:
	// Inhibits rendering
	bool NORENDER;
	public:
	// Blocks rendering calls (e.g., for config/model is being updated)
	void pause_rendering() { NORENDER = TRUE; }
	// Removes rendering block
	void resume_rendering() { NORENDER = FALSE; }
	// Return whether rendering is prohibited
	bool no_rendering() { return NORENDER; }

	/*
	// Specific Window Functions
	*/
	public:
	// Add a message to the main window's message output box
	virtual void print_message(const char*);
	// Refresh main canvas
	virtual void refresh();
	// Change the font used in the message box
	virtual void change_msg_font(const char*);
	// Update trajectory control widgets
	virtual void update_trajcontrols();
	// Update main window labels
	virtual void update_labels();
	// Process events from GUI
	virtual void process_events();
	// Terminate GUI
	virtual void close_application();

	/*
	// Progress Dialog
	*/
	public:
	// Instantiate a progress dialog
	virtual void progress_create(const char *jobtitle, int stepstodo);
	// Update the progress dialog
	virtual bool progress_update(int currentstep);
	// Terminate the progress dialog
	virtual void progress_terminate();
	// Instantiate a text-based progress dialog
	void text_progress_create(const char *jobtitle, int stepstodo);
	// Update the text progress dialog
	bool text_progress_update(int currentstep);
	// Terminate the progress dialog
	void text_progress_terminate();
	// Variables for the position and maximum of the text progress dialog
	int textprogress_stepstodo, textprogress_percent;

	/*
	// Object management
	*/
	public:
	// Add model (adds new model to list)
	virtual void add_model(model*);
	// Remove model from list
	virtual void remove_model(model*);
	// Select model (show in main/sub windows)
	virtual void select_model(model*);
	// Add forcefield (adds ff to list)
	virtual void add_ff(forcefield*);
	// Remove ff from list
	virtual void remove_ff(forcefield*);
	// Select forcefield in list
	virtual void select_ff(forcefield*);

	/*
	// GUI file filters
	*/
	public:
	// Initialise GUI file filters array
	virtual void init_filters();
};

// Define here a basic GUI class if no proper GUI has been specified
#ifndef HAS_GUI
	extern gui_master gui;
#else
	#ifdef WITH_GTKGUI
		#include "gui-gtk/gui-gtk.h"
	#endif
	#ifdef WITH_QTGUI
		#include "gui-qt/gui-qt.h"
	#endif
#endif

#endif
