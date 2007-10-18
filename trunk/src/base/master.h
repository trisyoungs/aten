/*
	*** Master structure
	*** src/base/master.h

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

#ifndef H_MASTER_H
#define H_MASTER_H

#include "classes/clipboard.h"
#include "classes/surface.h"
#include "file/filter.h"
#include "script/script.h"
#include <getopt.h>

#define MAXCLIOPTS 50

// Timer events status
enum timer_mode { TM_INACTIVE, TM_REQUEST_STOP, TM_ACTIVE };

// Program Modes
enum prog_mode { PM_SCRIPT, PM_BATCH, PM_INTERACTIVE, PM_CONSOLE, PM_GUI };

// Master
class master_data
{
	public:
	// Constructor / Destructor
	master_data();
	~master_data();
	// Remove all dynamic data
	void clear();

	/*
	// Models
	*/
	private:
	// Internal count for naming new models.
	int modelid;
	// Current active model for editing
	model *currentmodel;
	// Atomic colouring scheme to use.
	atom_colour scheme;
	// List of models
	list<model> models;

	public:
	// Sets the current active model for editing
	void set_currentmodel(model*);
	// Return current active model for editing
	model *get_currentmodel() { return currentmodel; }
	// Return first item in the model list
	model *get_models() { return models.first(); }
	// Return nth item in the model list
	model *get_model(int n) { return models[n]; }
	// Return the current model's index in the model list
	int get_currentmodelindex() { return models.index_of(currentmodel); }
	// Return index of specified model
	int get_modelindex(model *m) { return models.index_of(m); }
	// Return the number of models in the model list
	int get_nmodels() { return models.size(); }
	// Return the model ID counter
	int get_modelid() { return ++modelid; }
	// Add a new model to the workspace
	model* add_model();
	// Remove specified model from the list
	void remove_model(model*);
	// Remove FF references from the model list
	void dereference_ff(forcefield*);
	// Find model by name
	model *find_model(const char*);
	// Set the atomic colour scheme
	void set_colour_scheme(atom_colour ac) { scheme = ac; }
	// Return the current atomic colouring scheme
	atom_colour get_colour_scheme() { return scheme; }

	/*
	// Import / Export
	*/
	private:
	// Load filter(s) from specified file
	bool load_filter(const char*);
	// Set export partners for import filters
	void partner_filters();

	public:
	// List of file filters 
	list<filter> filters[FT_NITEMS];
	// Load filters from specified location
	bool open_filters(const char* dir, bool isdatadir);
	// Probe file for its format
	filter *probe_file(const char*, filter_type);

	/*
	// Forcefields
	*/
	private:
	// List of loaded forcefields
	list<forcefield> ffs;
	// Currently-selected FF (in GUI, selection in ff_flist)
	forcefield *currentff;

	public:
	// Returns the first ff in the list
	forcefield *get_ffs() { return ffs.first(); }
	// Returns the number of loaded forcefields
	int get_nffs() { return ffs.size(); }
	// Set active forcefield
	void set_currentff(forcefield *ff) { currentff = ff; }
	// Returns the active forcefield
	forcefield *get_currentff() { return currentff; }
	// Remove specified forcefield
	void remove_ff(forcefield*);
	// Load the specified forcefield
	forcefield *load_ff(const char*);
	// Find forcefield by name
	forcefield *find_ff(const char*);

	/*
	// Surfaces
	*/
	public:
	// Currently loaded surface
	list<surface> surfaces;

	/*
	// Clipboards
	*/
	public:
	// User clipboard
	clipboard userclip;
	// Program clipboard
	clipboard privclip;

	/*
	// System Variables
	*/
	private:
	// Previous ticks count registered by on_idle()
	clock_t time_last;
	// Current ticks count registered by on_idle()
	clock_t time_current;
	// Current delta (+1 or -1) of the changing variable
	int timer_changedelta;
	// Changing variable (1 - prefs.timer_eventsize)
	int timer_eventcurrent;
	// Scaled changing var (between 0.0 and 1.0)
	float timer_eventscaled;
	// Number of 'ticks' per second for changing var
	int timer_delay;
	// Whether timer events are currently active
	timer_mode timer_status;
	// Number of steps in the changing var
	int timer_eventsize;
	// Number of times per second to perform idle 'events'
	int timer_period;

	public:
	// Number of clock cycles per millisecond
	int clocks_per_ms;
	// Whether to use timer events
	bool use_timer;
	// Sets the current timer_per_second
	void set_timer_period(int);
	// Returns the current timer_per_second
	int get_timer_period() { return timer_period; }
	// Sets the length scale of the timer event
	void set_timer_eventsize(int);
	// Returns the length scale of the timer event
	int get_timer_eventsize() { return timer_eventsize; }
	// Idle function (called by (*)(void*)onidle)
	bool on_idle();
	// Activate timer events
	void start_timer_events();
	// Deactivate timer events
	void stop_timer_events();
	// Location of user's home directory
	dnchar homedir;
	// Current working directory
	dnchar workdir;
	// Data directory
	dnchar datadir;

	/*
	// Program Modes
	*/
	private:
	// Current mode of program operation
	prog_mode program_mode;

	public:
	// Sets the current program mode
	void set_program_mode(prog_mode pm) { program_mode = pm; }
	// Cached script data
	list<script> scripts;
	// Script to store temporary typed commands
	script cmd_script;

	/*
	// Building
	*/
	// Selected drawing element
	int sketchelement;

	/*
	// Progress Indicators
	*/
	public:
	// Initialise a progress indicator
	void initialise_progress(const char *jobtitle, int totalsteps);
	// Update the number of steps (returns if the dialog was canceled)
	bool update_progress(int currentstep);
	// Terminate the current progress
	void cancel_progress();

	/*
	// CLI
	*/
	private:
	// Array of long CLI options
	option longopts[MAXCLIOPTS];
	// Add long option to list
	void add_cli_option(char*,int,int,bool);
	// Print usage information
	void print_usage();
	// Number of CLI options added
	int nopts;
	// List of short options
	dnchar shortopts;

	public:
	// Prepare CLI options
	void prepare_cli();
	// Parse command line options
	int parse_cli(int,char**);
};

extern master_data master;

#endif
