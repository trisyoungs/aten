/*
	*** Master structure
	*** src/base/master.cpp
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

#include "model/model.h"
#include "classes/pattern.h"
#include "classes/grid.h"
#include "classes/clipboard.h"
#include "base/spacegroup.h"
#include "base/master.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "parse/parser.h"
#include <fstream>

Master master;

// Constructor
Master::Master()
{
	// Models
	modelId_ = 0;

	// Modes
	programMode_ = Master::GuiMode;

	// Store pointers to member functions
	initCommands();

	// Misc 
	sketchElement_ = 6;
	homeDir = "/tmp";

	// Clipboard
	userClipboard = new Clipboard;
}

// Destructor
Master::~Master()
{
	clear();
	delete userClipboard;
}

// Clear
void Master::clear()
{
	models_.clear();
	forcefields_.clear();
	userClipboard->clear();
	scripts.clear();
	for (int i=0; i<FT_NITEMS; i++) filters_[i].clear();
}

/*
// GUI Routines
*/

// Set the active model
void Master::setCurrentModel(Model *m)
{
	dbgBegin(Debug::Calls,"Master::setCurrentModel");
	// Set current.m and tell the mainview canvas to display it
	current.m = m;
	// Set other Bundle objects based on model
	current.p = m->patterns();
	current.i = NULL;
	current.m->renderSource()->calculateViewMatrix();
	current.m->renderSource()->projectAll();
	dbgEnd(Debug::Calls,"Master::setCurrentModel");
}

/*
// Model Management routines
*/

// Return current active model for editing
Model *Master::currentModel() const
{
	return current.m;
}

// Return first item in the model list
Model *Master::models() const
{
	return models_.first();
}

// Return nth item in the model list
Model *Master::model(int n)
{
	return models_[n];
}

// Return the current model's index in the model list
int Master::currentModelId() const
{
	return models_.indexOf(current.m);
}

// Return index of specified model
int Master::modelIndex(Model *m) const
{
	return models_.indexOf(m);
}

// Return the number of models in the model list
int Master::nModels() const
{
	return models_.nItems();
}

// Add model
Model *Master::addModel()
{
	dbgBegin(Debug::Calls,"Master::addModel");
	current.m = models_.add();
	char newname[16];
	sprintf(newname,"Unnamed%03i",++modelId_);
	current.m->setName(newname);
	if (gui.exists())
	{
		gui.addModel(current.m);
		gui.mainWindow->refreshDisorderPage();
	}
	dbgEnd(Debug::Calls,"Master::addModel");
	return current.m;
}

// Remove model
void Master::removeModel(Model *xmodel)
{
	// Remove this model from the model_list in the main window
	dbgBegin(Debug::Calls,"Master::removeModel");
	Model *m;
	// Unset the datamodel for the canvas
	// Delete the current model, but don't allow there to be zero models_...
	// (if possible, set the active row to the next model, otherwise  the previous)
	if (models_.nItems() == 1) m = master.addModel();
	else xmodel->next != NULL ? m = xmodel->next : m = xmodel->prev;
	current.m = m;
	// Delete the old model (GUI first, then master)
	int id = models_.indexOf(xmodel);
	models_.remove(xmodel);
	if (gui.exists())
	{
		gui.removeModel(id);
		gui.mainWindow->refreshDisorderPage();
	}
	dbgEnd(Debug::Calls,"Master::removeModel");
}

// Find model by name
Model *Master::findModel(const char *s) const
{
	// Search model list for name 's' (script function)
	dbgBegin(Debug::Calls,"Master::findModel");
	Model *result = NULL;
	for (result = models_.first(); result != NULL; result = result->next) if (strcmp(s,result->name()) == 0) break;
	dbgEnd(Debug::Calls,"Master::findModel");
	return result ;
}

/*
// Grid Management Routines
*/

// Return list of surfaces
Grid *Master::grids() const
{
	return grids_.first();
}

// Return number of surfaces loaded
int Master::nGrids() const
{
	return grids_.nItems();
}

// Return specified surface
Grid *Master::grid(int id)
{
	return grids_[id];
}

// Add new surface
Grid *Master::addGrid()
{
	current.g = grids_.add();
	return current.g;
}

// Remove surface
void Master::removeGrid(Grid *xgrid)
{
	xgrid->next != NULL ? current.g = xgrid->next : current.g = xgrid->prev;
	// Finally, delete the old surface
	grids_.remove(xgrid);
}

/*
// Forcefield Management routines
*/

// Add Forcefield
Forcefield *Master::addForcefield()
{
	current.ff = forcefields_.add();
	return current.ff;
}

// Load forcefield
Forcefield *Master::loadForcefield(const char *filename)
{
	dbgBegin(Debug::Calls,"Master::loadForcefield");
	Forcefield *newff = forcefields_.add();
	if (!newff->load(filename))
	{
		msg(Debug::None,"Couldn't load forcefield file '%s'.\n",filename);
		forcefields_.remove(newff);
		dbgEnd(Debug::Calls,"Master::loadForcefield");
		return NULL;
	}
	else current.ff = newff;
	dbgEnd(Debug::Calls,"Master::loadForcefield");
	return newff;
}

// Unload forcefield from the master's list
void Master::removeForcefield(Forcefield *xff)
{
	dbgBegin(Debug::Calls,"Master::removeForcefield");
	Forcefield *newff;
	// If possible, set the active row to the next model. Otherwise, the previous.
	xff->next != NULL ? newff = xff->next : newff = xff->prev;
	current.ff = newff;
	dereferenceForcefield(xff);
	// Finally, delete the ff
	forcefields_.remove(xff);
	dbgEnd(Debug::Calls,"Master::removeForcefield");
}

// Find forcefield by name
Forcefield *Master::findForcefield(const char *s) const
{
	// Search forcefield list for name 's' (script function)
	dbgBegin(Debug::Calls,"Master::findForcefield");
	Forcefield *ff;
	for (ff = forcefields_.first(); ff != NULL; ff = ff->next) if (strcmp(s,ff->name()) == 0) break;
	if (ff == NULL) msg(Debug::None,"Forcefield '%s' is not loaded.\n",s);
	dbgEnd(Debug::Calls,"Master::findForcefield");
	return ff;
}

// Dereference forcefield
void Master::dereferenceForcefield(Forcefield *xff)
{
	// Remove references to the forcefield in the models
	dbgBegin(Debug::Calls,"Master::dereferenceForcefield");
	for (Model *m = models_.first(); m != NULL; m = m->next)
	{
		if (m->forcefield() == xff)
		{
			m->removeTyping();
			m->setForcefield(NULL);
		}
		for (Pattern *p = m->patterns(); p != NULL; p = p->next)
		{
			if (p->forcefield() == xff)
			{
				Atom *i = p->firstAtom();
				for (int n=0; n<p->totalAtoms(); n++)
				{
					i->setType(NULL);
					i = i->next;
				}
				p->setForcefield(NULL);
			}
		}
	}
	dbgEnd(Debug::Calls,"Master::dereferenceForcefield");
}

// Set the default forcefield
void Master::setDefaultForcefield(Forcefield *ff)
{
	defaultForcefield_ = ff;
	if (defaultForcefield_ == NULL) msg(Debug::None,"Default forcefield has been unset.\n");
	else msg(Debug::None,"Default forcefield is now '%s'.\n", defaultForcefield_->name());
}

// Return the first ff in the list
Forcefield *Master::forcefields() const
{
	return forcefields_.first();
}

// Return the nth ff in the list
Forcefield *Master::forcefield(int n)
{
	return forcefields_[n];
}

// Return the number of loaded forcefields
int Master::nForcefields() const
{
	return forcefields_.nItems();
}

// Set active forcefield
void Master::setCurrentForcefield(Forcefield *ff)
{
	current.ff = ff;
}

// Set active forcefield by ID
void Master::setCurrentForcefield(int id)
{
	current.ff = forcefields_[id];
}

// Return the active forcefield
Forcefield *Master::currentForcefield() const
{
	return current.ff;
}

// Return ID of current forcefield
int Master::currentForcefieldId() const
{
	return forcefields_.indexOf(current.ff);
}

// Get the current default forcefield
Forcefield *Master::defaultForcefield() const
{
	return defaultForcefield_;
}

/*
// Filters
*/

// Load filters
bool Master::openFilters(const char *path, bool isdatadir)
{
	dbgBegin(Debug::Calls,"Master::openFilters");
	// Load in model filters
	char longname[512];
	// Open the filter list file (in 'path/index') and read in the list of filters to load in...
	strcpy(longname,path);
	strcat(longname,"index");
	if (isdatadir) msg(Debug::None,"Looking for filter index '%s'...\n", longname);
	else msg(Debug::None,"Loading user filters '%s'...\n", longname);
	ifstream listfile(longname,ios::in);
	if (!listfile.is_open())
	{
		if (isdatadir) msg(Debug::None,"Filter index file not found.\n");
		else msg(Debug::None,"No user filter index found.\n");
		dbgEnd(Debug::Calls,"Master::openFilters");
		return FALSE;
	}
	else
	{
		// Read filter names from file and open them
		printf("--> ");
		while (!listfile.eof())
		{
			strcpy(longname,path);
			if (parser.getArgsDelim(&listfile,Parser::SkipBlanks) != 0) break;
			strcat(longname,parser.argc(0));
			printf("%s  ",parser.argc(0));
			if (!loadFilter(longname))
			{
				
				return FALSE;
			}
		}
		printf("\n");
		listfile.close();
	}
	// Create gui filter list, partners, and print info (if not 'isdatadir')
	if (!isdatadir)
	{
		// Set filter partners
		partnerFilters();
		// Print data on loaded filters
		msg(Debug::None,"Found (import/export):  Models (%i/%i) ", filters_[FT_MODEL_IMPORT].nItems(), filters_[FT_MODEL_EXPORT].nItems());
		msg(Debug::None,"Trajectory (%i/%i) ", filters_[FT_TRAJECTORY_IMPORT].nItems(), filters_[FT_TRAJECTORY_EXPORT].nItems());
		msg(Debug::None,"Expression (%i/%i) ", filters_[FT_EXPRESSION_IMPORT].nItems(), filters_[FT_EXPRESSION_EXPORT].nItems());
		msg(Debug::None,"Grid (%i/%i)\n", filters_[FT_GRID_IMPORT].nItems(), filters_[FT_GRID_EXPORT].nItems());
	}
	dbgEnd(Debug::Calls,"Master::openFilters");
	return TRUE;
}

// Read commands from filter file
bool Master::loadFilter(const char *filename)
{
	dbgBegin(Debug::Calls,"Master::loadFilter");
	FilterType ft;
	Filter *newfilter;
	bool error;
	int success;
	ifstream filterfile(filename,ios::in);

	// Pre-read first line to check
	success = parser.getArgsDelim(&filterfile,Parser::UseQuotes+Parser::SkipBlanks);
	error = FALSE;
	while (!filterfile.eof())
	{
		// Get filter type from first argument
		ft = FT_from_text(parser.argc(0));
		// Unrecognised filter section?
		if (ft == FT_NITEMS)
		{
			msg(Debug::None,"Unrecognised section '%s' in filter.\n",parser.argc(0));
			error = TRUE;
			break;
		}
		// Add main filter section
		newfilter = filters_[ft].add();
		newfilter->setType(ft);
		// Call the filter to load its commands.
		// If the load is not successful, remove the filter we just created
		if (!newfilter->load(filterfile))
		{
			filters_[ft].remove(newfilter);
			printf("Error reading '%s' section from file '%s'\n",text_from_FT(newfilter->type()),filename);
			error = TRUE;
			break;
		}
	}
	filterfile.close();
	//variables.print();
	dbgEnd(Debug::Calls,"Master::loadFilter");
	return (!error);
}

// Set filter partners
void Master::partnerFilters()
{
	dbgBegin(Debug::Calls,"Master::partnerFilters");
	// Loop through import filters and search / set export partners
	Filter *imp, *exp;
	int importid;
	printf("Model Formats:");
	for (imp = filters_[FT_MODEL_IMPORT].first(); imp != NULL; imp = imp->next)
	{
		printf(" %s[r", imp->nickname());
		importid = imp->id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters_[FT_MODEL_EXPORT].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->id())
				{
					imp->setPartner(exp);
					printf("w]");
					break;
				}
			}
		}
		if (exp == NULL) printf("o]");
	}
	printf("\n");
	printf("Grid Formats:");
	for (imp = filters_[FT_GRID_IMPORT].first(); imp != NULL; imp = imp->next)
	{
		printf(" %s[r", imp->nickname());
		importid = imp->id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters_[FT_GRID_EXPORT].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->id())
				{
					imp->setPartner(exp);
					printf("w]");
					break;
				}
			}
		}
		if (exp == NULL) printf("o]");
	}
	printf("\n");
	dbgEnd(Debug::Calls,"Master::partnerFilters");
}

// Find filter with specified type and nickname
Filter *Master::findFilter(FilterType ft, const char *nickname) const
{
	dbgBegin(Debug::Calls,"Master::findFilter");
	Filter *result;
	for (result = filters_[ft].first(); result != NULL; result = result->next)
		if (strcmp(result->nickname(), nickname) == 0) break;
	if (result == NULL) msg(Debug::None,"No %s filter with nickname '%s' defined.\n",text_from_FT(ft),nickname);
	dbgEnd(Debug::Calls,"Master::findFilter");
	return result;
}

// Return first filter in list (of a given type)
Filter *Master::filters(FilterType ft) const
{
	return filters_[ft].first();
}

/*
// Progress Indicators
*/

// Initialise a progress indicator
void Master::initialiseProgress(const char *jobtitle, int totalsteps)
{
	gui.progressCreate(jobtitle, totalsteps);
}

// Update the number of steps (returns if the dialog was canceled)
bool Master::updateProgress(int currentstep)
{
	return gui.progressUpdate(currentstep);
}

// Terminate the current progress
void Master::cancelProgress()
{
	gui.progressTerminate();
}

// Spacegroup name search
int Master::findSpacegroupByName(const char *name) const
{
	dbgBegin(Debug::Calls,"Master::findSpacegroupByName");
	static char lcname[256], lcsg[256];
	strcpy(lcname,lowerCase(name));
	int result = 0;
	for (int n=1; n<231; n++)
	{
		strcpy(lcsg,lowerCase(spacegroups[n].name));
		if (strcmp(lcsg,lcname) == 0)
		{
			result = n;
			break;
		}
	}
	dbgEnd(Debug::Calls,"Master::findSpacegroupByName");
	return result;
}

// Cell type from spacegrgoup
Cell::CellType Master::spacegroupCellType(int sg) const
{
	dbgBegin(Debug::Calls,"Master::spacegroupCellType");
	Cell::CellType result = Cell::NoCell;
	// None
	if (sg == 0) result = Cell::NoCell;
	// Triclinic and monoclinic
	else if (sg < 16) result = Cell::ParallelepipedCell;
	// Orthorhombic and tetragonal
	else if (sg < 143) result = Cell::OrthorhombicCell;
	// Trigonal
	else if (sg < 168) result = Cell::ParallelepipedCell;
	// Hexagonal
	else if (sg < 195) result = Cell::NoCell;
	// Cubic
	else result = Cell::CubicCell;
	dbgBegin(Debug::Calls,"Master::spacegroupCellType");
	return result;
}
