/*
	*** Qt user interface
	*** src/gui/gui.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_GUIQT_H
#define ATEN_GUIQT_H

#include "gui/gui.h"
#include "gui/canvas.h"

// Forward declarations
class AtenForm;
class AtenPrefs;
class atom;
class model;
class forcefield;
class grid;
class QApplication;
class QProgressDialog;

// QT4 GUI
class gui_qt
{
	/*
	// Existence of GUI
	*/
	private:
	// Is a GUI available?
	bool is_available;
	// Does the GUI exist (has it been created)
	bool does_exist;

	/*
	// Basic Window Functions
	*/
	public:
	// Constructor / Destructor
	gui_qt();
	~gui_qt();
	// Returns if the GUI is available
	bool available() { return is_available; }
	// Returns if the GUI has been created
	bool exists() { return does_exist; }
	// Early doors functions
	void prepare();
	// Initialises all aspects of the GUI and hands over control
	void run(int, char**);
	// Question user dialog
	int user_question(const char*, const char*);

	/*
	// Main Canvas and Rendering
	*/
	protected:
	// Render inhibition flag
	bool NORENDER;

	public:
	// Blocks rendering calls (e.g., for config/model is being updated)
	void pause_rendering() { NORENDER = TRUE; }
	// Removes rendering block
	void resume_rendering() { NORENDER = FALSE; }
	// Return whether rendering is prohibited
	bool no_rendering() { return NORENDER; }

	/*
	// General Window Functions
	*/
	public:
	// Add a message to the main window's message output box
	void print_message(const char*);
	// Refresh main canvas
	void refresh();
	// Update trajectory control widgets
	void update_trajcontrols();
	// Update main window labels
	void update_labels();
	// Process events from GUI
	void process_events();
	// Save before close
	bool save_before_close();
	// Update Undo/Redo menu items
	void update_undoredo();

	/*
	// Object management
	*/
	public:
	// Add model (adds new model to list)
	void add_model(model*);
	// Remove model from list
	void remove_model(int id);
	// Add forcefield (adds ff to list)
	void add_ff(forcefield*);
	// Remove ff from list
	void remove_ff(forcefield*);
	// Select forcefield in list
	void select_ff(forcefield*);
	// Add surface (adds new surface to list)
	void add_grid(grid*);
	// Remove surface from list
	void remove_grid(grid*);
	// Select surface (show in main/sub windows)
	void select_grid(grid*);

	/*
	// Files
	*/
	public:
	// Initialise GUI file filters array
	void init_filters();

	/*
	// Misc
	*/
	public:
	// Convert Qt key code value to internal key
	key_code convert_to_KC(int);

	/*
	// Windows / Dialogs
	*/
	public:
	// Main Qt widget for the interface
	AtenForm *mainwindow;
	// Main application structure
	QApplication *app;
	// Preferences Dialog
	AtenPrefs *prefsdialog;
	// Call the atompopup menu
	void call_atompopup(atom*, int, int);
	// Main view
	canvas mainview;

	/*
	// Trajectory State
	*/
	private:
	// Whether the trajectory is currently playing
	bool trajectory_playing;
	// ID of rtrajectory timer
	int trajectory_timerid;

	public:
	// Return state of trajectory playback
	bool get_trajectory_playing() { return trajectory_playing; }
	// Set state of trajectory playback
	void set_trajectory_playing(bool b) { trajectory_playing = b; }
	// Return trajectory timer id
	int get_trajectory_timerid() { return trajectory_timerid; }
	// Set state of trajectory playback
	void set_trajectory_timerid(int i) { trajectory_timerid = i; }
	// Stop trajectory playback
	void stop_trajectory_playback();

	/*
	// Progress Dialog
	*/
	private:
	// Indicator that the 'Cancel' button was pressed
	bool progress_canceled;

	public:
	// Notify that the progress indicator should be canceled
	void notify_progress_canceled() { progress_canceled; }
	// Instantiate a progress dialog
	void progress_create(const char *jobtitle, int stepstodo);
	// Update the progress dialog
	bool progress_update(int currentstep);
	// Terminate the progress dialog
	void progress_terminate();
	// Instantiate a text-based progress dialog
	void text_progress_create(const char *jobtitle, int stepstodo);
	// Update the text progress dialog
	void text_progress_update(int currentstep);
	// Terminate the progress dialog
	void text_progress_terminate();
	// Variables for the position and maximum of the text progress dialog
	int textprogress_stepstodo, textprogress_percent;

	/*
	// Basic Offscreen Canvas
	*/
	public:
	// Offscreen canvas (for use by, e.g., g2ps routines)
	canvas offscreencanvas;
};

extern gui_qt gui;

#endif
