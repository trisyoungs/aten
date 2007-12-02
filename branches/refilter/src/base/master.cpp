/*
	*** Master structure
	*** src/base/master.cpp
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

#include "model/model.h"
#include "classes/pattern.h"
#include "base/master.h"
#include "gui/gui.h"
#include "file/parse.h"
#include <fstream>

master_data master;

// Constructor
master_data::master_data()
{
	// Models
	modelid = 0;
	currentmodel = NULL;
	currentff = NULL;

	// Modes
	program_mode = PM_GUI;

	// Misc 
	sketchelement = 6;
	homedir = "/tmp";

	#ifdef MEMDEBUG
		printf("Constructor : master_data\n");
	#endif
}

// Destructor
master_data::~master_data()
{
	#ifdef MEMDEBUG
		printf(" Destructor : master_data\n");
	#endif
}

// Clear
void master_data::clear()
{
	models.clear();
	ffs.clear();
	userclip.clear();
	privclip.clear();
	scripts.clear();
	for (int i=0; i<FT_NITEMS; i++) filters[i].clear();
}

/*
// GUI Routines
*/

// Update main window
void master_data::set_currentmodel(model *m)
{
	// Set the active model to that specified, and refresh windows that depend on model data
	dbg_begin(DM_CALLS,"master::set_currentmodel");
	// Set currentmodel and tell the mainview canvas to display it
	currentmodel = m;
	gui.select_model(m);
	currentmodel->calculate_viewmatrix();
	currentmodel->project_all();
	dbg_end(DM_CALLS,"master::set_currentmodel");
}

/*
// Model Management routines
*/

// Add model
model *master_data::add_model()
{
	dbg_begin(DM_CALLS,"master::add_model");
	model *m = models.add();
	currentmodel = m;
	gui.add_model(m);
	gui.select_model(m);
	dbg_end(DM_CALLS,"master::add_model");
	return m;
}

// Remove model
void master_data::remove_model(model *xmodel)
{
	// Remove this model from the model_list in the main window
	dbg_begin(DM_CALLS,"master::remove_model");
	model *m;
	// Unset the datamodel for the canvas
	// Delete the current model, but don't allow there to be zero models...
	if (models.size() == 1)
	{
		// Add a new model to the workspace
		m = master.add_model();
	}
	else
		// If possible, set the active row to the next model. Otherwise, the previous.
		xmodel->next != NULL ? m = xmodel->next : m = xmodel->prev;
	currentmodel = m;
	gui.remove_model(xmodel);
	gui.select_model(m);
	// Finally, delete the old model
	models.remove(xmodel);
	dbg_end(DM_CALLS,"master::remove_model");
}

// Find model by name
model *master_data::find_model(const char *s)
{
	// Search model list for name 's' (script function)
	dbg_begin(DM_CALLS,"master::find_model");
	model *result = NULL;
	model *m = models.first();
	while (m != NULL)
	{
		if (strcmp(s,m->get_name()) == 0) result = m;
		m = m->next;
	}
	dbg_end(DM_CALLS,"master::find_model");
	return result ;
}

/*
// Surface Management Routines
*/

// Add new surface
grid *master_data::add_grid()
{
	grid *g = grids.add();
	gui.add_grid(g);
	gui.select_grid(g);
	return g;
}

// Remove surface
void master_data::remove_grid(grid *xgrid)
{
	grid *g;
	xgrid->next != NULL ? g = xgrid->next : g = xgrid->prev;
	gui.remove_grid(xgrid);
	gui.select_grid(g);
	// Finally, delete the old surface
	grids.remove(xgrid);
}

/*
// Forcefield Management routines
*/

// Load forcefield
forcefield *master_data::load_ff(const char *filename)
{
	dbg_begin(DM_CALLS,"master::load_ff");
	forcefield *newff = ffs.add();
	if (!newff->load(filename))
	{
		msg(DM_NONE,"Couldn't load forcefield file '%s'.\n",filename);
		ffs.remove(newff);
		dbg_end(DM_CALLS,"master::load_ff");
		return NULL;
	}
	else
	{
		gui.add_ff(newff);
		currentff = newff;
	}
	dbg_end(DM_CALLS,"master::load_ff");
	return newff;
}

void master_data::remove_ff(forcefield *xff)
{
	dbg_begin(DM_CALLS,"master::remove_ff");
	forcefield *newff;
	// If possible, set the active row to the next model. Otherwise, the previous.
	xff->next != NULL ? newff = xff->next : newff = xff->prev;
	currentff = newff;
	gui.remove_ff(xff);
	gui.select_ff(newff);
	// Finally, delete the ff
	ffs.remove(xff);
	dbg_end(DM_CALLS,"master::remove_ff");
}

// Find forcefield by name
forcefield *master_data::find_ff(const char *s)
{
	// Search forcefield list for name 's' (script function)
	dbg_begin(DM_CALLS,"master::find_ff");
	forcefield *result = NULL;
	forcefield *ff = ffs.first();
	while (ff != NULL)
	{
		if (strcmp(s,ff->get_name()) == 0)
		{
			result = ff;
			break;
		}
		ff = ff->next;
	}
	dbg_end(DM_CALLS,"master::find_ff");
	return result;
}

// Dereference forcefield
void master_data::dereference_ff(forcefield *xff)
{
	// Remove references to the forcefield in the models
	dbg_begin(DM_CALLS,"master::dereference_ff");
	model *m = models.first();
	while (m != NULL)
	{
		if (m->get_ff() == xff)
		{
			m->remove_typing();
			m->set_ff(NULL);
		}
		pattern *p = m->get_patterns();
		while (p != NULL)
		{
			if (p->get_ff() == xff)
			{
				atom *i = p->get_firstatom();
				for (int n=0; n<p->get_totalatoms(); n++)
				{
					i->set_type(NULL);
					i = i->next;
				}
				p->set_ff(NULL);
			}
			p = p->next;
		}
		m = m->next;
	}
	dbg_end(DM_CALLS,"master::dereference_ff");
}

// Load filters
bool master_data::open_filters(const char *path, bool isdatadir)
{
	dbg_begin(DM_CALLS,"master::open_filters");
	// Load in model filters
	filter *f;
	int n;
	char longname[512];
	// Open the filter list file (in 'path/index') and read in the list of filters to load in...
	if (isdatadir) msg(DM_NONE,"Loading default filters ('%s')...\n",path);
	else msg(DM_NONE,"Loading user filters ('%s')...\n",path);
	strcpy(longname,path);
	strcat(longname,"index");
	ifstream listfile(longname,ios::in);
	if (!listfile.is_open())
	{
		if (isdatadir) msg(DM_NONE,"Index file not found. Have you set $ATENDATA?\n");
		else msg(DM_NONE,"No user filter index found.\n");
	}
	else
	{
		// Read filter names from file and open them
		printf("--> ");
		while (!listfile.eof())
		{
			strcpy(longname,path);
			if (parser.get_args_delim(&listfile,PO_DEFAULTS+PO_SKIPBLANKS) != 0) break;
			strcat(longname,parser.argc(0));
			printf("%s  ",parser.argc(0));
			if (!load_filter(longname))
			{
				dbg_end(DM_CALLS,"master::open_filters");
				return FALSE;
			}
		}
		printf("\n");
		listfile.close();
	}
	// Create gui filter list, partners, and print info (if not 'isdatadir')
	if (!isdatadir)
	{
		// Create arrays for GUI file filters
		gui.init_filters();
		// Set filter partners
		partner_filters();
		// Print data on loaded filters
		msg(DM_NONE,"Found (import/export): Models (%i/%i) ", filters[FT_MODEL_IMPORT].size(), filters[FT_MODEL_EXPORT].size());
		msg(DM_NONE,"Trajectory (%i/%i) ", filters[FT_TRAJECTORY_IMPORT].size(), filters[FT_TRAJECTORY_EXPORT].size());
		msg(DM_NONE,"Field (%i/%i) ", filters[FT_FIELD_IMPORT].size(), filters[FT_FIELD_EXPORT].size());
		msg(DM_NONE,"Grid (%i/%i)\n", filters[FT_GRID_IMPORT].size(), filters[FT_GRID_EXPORT].size());
	}
	dbg_end(DM_CALLS,"master::open_filters");
	return TRUE;
}

// Read commands from filter file
bool master_data::load_filter(const char *filename)
{
	dbg_begin(DM_CALLS,"master::load_filter");
	filter_type ft;
	filter *newfilter;
	bool foundmain, error;
	variable_list *vars;
	int success;
	zmap_type zm;
	ifstream filterfile(filename,ios::in);

	// Pre-read first line to check
	success = parser.get_args_delim(&filterfile,PO_USEQUOTES+PO_SKIPBLANKS);
	error = FALSE;
	while (!filterfile.eof())
	{
		foundmain = TRUE;
		// Get filter type from first argument
		ft = FT_from_text(parser.argc(0));
		// Unrecognised filter section?
		if (ft == FT_NITEMS)
		{
			msg(DM_NONE,"Unrecognised section '%s' in filter.\n",parser.argc(0));
			error = TRUE;
			break;
		}
		// Add main filter section
		newfilter = filters[ft].add();
		newfilter->set_type(ft);
		// Call the filter to load its commands.
		// If the load is not successful, remove the filter we just created
		if (!newfilter->load(filterfile))
		{
			filters[ft].remove(newfilter);
			printf("Error reading '%s' section from file '%s'\n",text_from_FT(newfilter->get_type()),filename);
			error = TRUE;
			break;
		}
	}
	filterfile.close();
	//variables.print();
	dbg_end(DM_CALLS,"master::load_filter");
	return (!error);
}

// Set filter partners
void master_data::partner_filters()
{
	dbg_begin(DM_CALLS,"master::partner_filters");
	// Loop through import filters and search / set export partners
	filter *imp, *exp;
	int importid;
	printf("Model Formats:");
	for (imp = filters[FT_MODEL_IMPORT].first(); imp != NULL; imp = imp->next)
	{
		printf(" %s[r", imp->get_nickname());
		importid = imp->get_id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters[FT_MODEL_EXPORT].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->get_id())
				{
					imp->set_partner(exp);
					printf("w]");
					break;
				}
			}
		}
		if (exp == NULL) printf("o]");
	}
	printf("\n");
	printf("Grid Formats:");
	for (imp = filters[FT_GRID_IMPORT].first(); imp != NULL; imp = imp->next)
	{
		printf(" %s[r", imp->get_nickname());
		importid = imp->get_id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters[FT_GRID_EXPORT].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->get_id())
				{
					imp->set_partner(exp);
					printf("w]");
					break;
				}
			}
		}
		if (exp == NULL) printf("o]");
	}
	printf("\n");
	dbg_end(DM_CALLS,"master::partner_filters");
}

/*
// Progress Indicators
*/

// Initialise a progress indicator
void master_data::initialise_progress(const char *jobtitle, int totalsteps)
{
	gui.progress_create(jobtitle, totalsteps);
}

// Update the number of steps (returns if the dialog was canceled)
bool master_data::update_progress(int currentstep)
{
	return gui.progress_update(currentstep);
}

// Terminate the current progress
void master_data::cancel_progress()
{
	gui.progress_terminate();
}
