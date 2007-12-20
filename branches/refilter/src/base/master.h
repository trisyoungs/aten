/*
	*** Master structure
	*** src/base/master.h
	Copyright T. Youngs 2007

	This file is part of Aten.

	Atexn is free software: you can redistribute it and/or modify
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
#include "classes/grid.h"
#include "classes/forcefield.h"
#include "classes/bundle.h"
#include "methods/sd.h"
#include "methods/cg.h"
#include "methods/mc.h"
#include "file/filter.h"
#include "command/commandlist.h"
#include <getopt.h>

#define MAXCLIOPTS 50

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
	// Current Objects
	*/
	public:
	// Current object bundle
	bundle current;

	/*
	// Models
	*/
	private:
	// Internal count for naming new models.
	int modelid;
	// List of models
	list<model> models;

	public:
	// Sets the current active model for editing
	void set_currentmodel(model*);
	// Return current active model for editing
	model *get_currentmodel() { return current.m; }
	// Return first item in the model list
	model *get_models() { return models.first(); }
	// Return nth item in the model list
	model *get_model(int n) { return models[n]; }
	// Return the current model's index in the model list
	int get_currentmodelindex() { return models.index_of(current.m); }
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

	public:
	// Return the first ff in the list
	forcefield *get_ffs() { return ffs.first(); }
	// Return the number of loaded forcefields
	int get_nffs() { return ffs.size(); }
	// Set active forcefield
	void set_currentff(forcefield *ff) { current.ff = ff; }
	// Set active forcefield by ID
	void set_currentff_by_id(int id) { current.ff = ffs[id]; }
	// Return the active forcefield
	forcefield *get_currentff() { return current.ff; }
	// Return ID of current forcefield
	int get_currentff_id() { return ffs.index_of(current.ff); }
	// Remove specified forcefield
	void remove_ff(forcefield*);
	// Load the specified forcefield
	forcefield *load_ff(const char*);
	// Find forcefield by name
	forcefield *find_ff(const char*);

	/*
	// Volumetric Grid Data
	*/
	private:
	// Currently loaded grids
	list<grid> grids;

	public:
	// Return list of surfaces
	grid *get_grids() { return grids.first(); }
	// Return number of surfaces loaded
	int get_ngrids() { return grids.size(); }
	// Return specified surface
	grid *get_grid(int id) { return grids[id]; }
	// Add new surface
	grid *add_grid();
	// Remove surface
	void remove_grid(grid *s);

	/*
	// Clipboards
	*/
	public:
	// User clipboard
	clipboard userclip;
	// Program clipboard
	clipboard privclip;

	/*
	// Locations
	*/
	public:
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
	list<commandlist> scripts;
	// Script to store temporary typed commands
	commandlist cmd_script;

	/*
	// Building
	*/
	private:
	// Selected drawing element
	int sketchelement;

	public:
	// Set current drawing element
	void set_sketchelement(int el) { sketchelement = el; }
	// Return current drawing element
	int get_sketchelement() { return sketchelement; }

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
	void add_cli_option(const char*, int, int, bool);
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
	int parse_cli(int, char**);

	/*
	// Methods / Algorithms
	*/
	public:
	// Steepest descent minimiser
	sd_method sd;
	// Conjugate gradient minimiser
	cg_method cg;
	// Monte Carlo methods
	mc_method mc;

};

extern master_data master;

#endif
