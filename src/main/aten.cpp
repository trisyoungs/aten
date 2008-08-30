/*
	*** Aten's master structure
	*** src/aten/aten.cpp
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

#include "main/aten.h"
#include "gui/gui.h"
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/grids.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "ff/forcefield.h"
#include "classes/grid.h"
#include "base/pattern.h"

// Singleton definition
Aten aten;

// Constructor
Aten::Aten()
{
	// Models
	modelId_ = 0;

	// Default program mode
	programMode_ = Aten::GuiMode;

	// Store pointers to command functions
	initCommands();

	// Misc 
	sketchElement_ = 6;
	homeDir_ = "/tmp";
	defaultForcefield_ = NULL;

	// Clipboards
	userClipboard = new Clipboard;
	gridClipboard_ = NULL;

	// CommandLists
	tempScript.createModelVariables("");
	interactiveScript.createModelVariables("");

	// Single-shot mode variables
	exportFilter_ = NULL;
}

// Destructor
Aten::~Aten()
{
	clear();
	delete userClipboard;
	if (gridClipboard_ != NULL) delete gridClipboard_;
}

// Clear
void Aten::clear()
{
	models_.clear();
	forcefields_.clear();
	userClipboard->clear();
	scripts.clear();
	for (int i=0; i<Filter::nFilterTypes; i++) filters_[i].clear();
}

// Sets the current program mode
void Aten::setProgramMode(ProgramMode pm)
{
	programMode_ = pm;
}

// Return the current program mode
Aten::ProgramMode Aten::programMode()
{
	return programMode_;
}

// Set current drawing element
void Aten::setSketchElement(short int el)
{
	sketchElement_ = el;
}

// Return current drawing element
short int Aten::sketchElement()
{
	return sketchElement_;
}

/*
// GUI Routines
*/

// Set the active model
void Aten::setCurrentModel(Model *m)
{
	msg.enter("Aten::setCurrentModel");
	// Set current.m and tell the mainview canvas to display it
	current.m = m;
	current.rs = (current.m == NULL ? NULL : current.m->renderSource());
	// Set other Bundle objects based on model
	current.p = m->patterns();
	current.g = m->grids();
	current.i = NULL;
	current.m->renderSource()->calculateViewMatrix();
	current.m->renderSource()->projectAll();
	// Set window title
	// Set the title of the main window to reflect the version
	char title[256];
	sprintf(title, "Aten (%s) - %s [%s]", ATENVERSION, current.m->name(), current.m->filename());
	if (gui.exists())
	{
		gui.mainWindow->setWindowTitle(title);
		gui.gridsWindow->refresh();
	}
	msg.exit("Aten::setCurrentModel");
}

/*
// Model Management routines
*/

// Return current active model for editing
Model *Aten::currentModel() const
{
	return current.m;
}

// Return first item in the model list
Model *Aten::models() const
{
	return models_.first();
}

// Return nth item in the model list
Model *Aten::model(int n)
{
	return models_[n];
}

// Return the current model's index in the model list
int Aten::currentModelId() const
{
	return models_.indexOf(current.m);
}

// Return index of specified model
int Aten::modelIndex(Model *m) const
{
	return models_.indexOf(m);
}

// Return the number of models in the model list
int Aten::nModels() const
{
	return models_.nItems();
}

// Add model
Model *Aten::addModel()
{
	msg.enter("Aten::addModel");
	Model *m = models_.add();
	char newname[16];
	sprintf(newname,"Unnamed%03i", ++modelId_);
	m->setName(newname);
	gui.addModel(m);
	gui.disorderWindow->refresh();
	setCurrentModel(m);
	msg.exit("Aten::addModel");
	return m;
}

// Remove model
void Aten::removeModel(Model *xmodel)
{
	// Remove this model from the model_list in the main window
	msg.enter("Aten::removeModel");
	Model *m;
	// Unset the datamodel for the canvas
	// Delete the current model, but don't allow there to be zero models_...
	// (if possible, set the active row to the next model, otherwise  the previous)
	if (models_.nItems() == 1) m = aten.addModel();
	else m = (xmodel->next != NULL ? xmodel->next : xmodel->prev);
	setCurrentModel(m);
	// Delete the old model (GUI first, then master)
	int id = models_.indexOf(xmodel);
	models_.remove(xmodel);
	gui.removeModel(id);
	gui.disorderWindow->refresh();
	msg.exit("Aten::removeModel");
}

// Find model by name
Model *Aten::findModel(const char *s) const
{
	// Search model list for name 's' (script function)
	msg.enter("Aten::findModel");
	Model *result = NULL;
	for (result = models_.first(); result != NULL; result = result->next) if (strcmp(s,result->name()) == 0) break;
	msg.exit("Aten::findModel");
	return result ;
}

/*
// Forcefield Management routines
*/

// Add Forcefield
Forcefield *Aten::addForcefield()
{
	current.ff = forcefields_.add();
	return current.ff;
}

// Load forcefield
Forcefield *Aten::loadForcefield(const char *filename)
{
	msg.enter("Aten::loadForcefield");
	// Try some different locations to find the supplied forcefield.
	static char s[512];
	bool result;
	Forcefield *newff = forcefields_.add();
	// First try - actual / absolute path
	msg.print(Messenger::Verbose,"Looking for forcefield in absolute path (%s)...\n",filename);
	if (fileExists(filename)) result = newff->load(filename);
	else
	{
		// Second try - aten.dataDir/ff
		sprintf(s,"%s/ff/%s", dataDir_.get(), filename);
		msg.print(Messenger::Verbose,"Looking for forcefield in installed location (%s)...\n",s);
		if (fileExists(s)) result = newff->load(s);
		else
		{
			// Last try - user home datadir/ff
			sprintf(s,"%s/.aten/ff/%s", homeDir_.get(), filename);
			msg.print(Messenger::Verbose,"Looking for forcefield in user's data directory (%s)...\n",s);
			if (fileExists(s)) result = newff->load(s);
			else msg.print("Can't find forcefield file '%s' in any location.\n", filename);
		}
	}
	if (result)
	{
		current.ff = newff;
		// If this is the first (only) forcefield loaded, make it the default
		if (forcefields_.nItems() == 1)
		{
			defaultForcefield_ = newff;
			msg.print("Forcefield '%s' is now the default.\n", newff->name());
		}
	}
	else
	{
		msg.print("Couldn't load forcefield file '%s'.\n", filename);
		forcefields_.remove(newff);
		newff = NULL;
	}
	msg.exit("Aten::loadForcefield");
	return newff;
}

// Unload forcefield from the master's list
void Aten::removeForcefield(Forcefield *xff)
{
	msg.enter("Aten::removeForcefield");
	Forcefield *newff;
	// If possible, set the active row to the next model. Otherwise, the previous.
	xff->next != NULL ? newff = xff->next : newff = xff->prev;
	current.ff = newff;
	dereferenceForcefield(xff);
	// Finally, delete the ff
	forcefields_.remove(xff);
	// Set a new default if necessary
	if (defaultForcefield_ == xff) defaultForcefield_ = forcefields_.first();
	msg.exit("Aten::removeForcefield");
}

// Find forcefield by name
Forcefield *Aten::findForcefield(const char *s) const
{
	// Search forcefield list for name 's' (script function)
	msg.enter("Aten::findForcefield");
	Forcefield *ff;
	for (ff = forcefields_.first(); ff != NULL; ff = ff->next) if (strcmp(s,ff->name()) == 0) break;
	if (ff == NULL) msg.print("Forcefield '%s' is not loaded.\n",s);
	msg.exit("Aten::findForcefield");
	return ff;
}

// Dereference forcefield
void Aten::dereferenceForcefield(Forcefield *xff)
{
	// Remove references to the forcefield in the models
	msg.enter("Aten::dereferenceForcefield");
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
	msg.exit("Aten::dereferenceForcefield");
}

// Set the default forcefield
void Aten::setDefaultForcefield(Forcefield *ff)
{
	defaultForcefield_ = ff;
	if (defaultForcefield_ == NULL) msg.print("Default forcefield has been unset.\n");
	else msg.print("Default forcefield is now '%s'.\n", defaultForcefield_->name());
}

// Return the first ff in the list
Forcefield *Aten::forcefields() const
{
	return forcefields_.first();
}

// Return the nth ff in the list
Forcefield *Aten::forcefield(int n)
{
	return forcefields_[n];
}

// Return the number of loaded forcefields
int Aten::nForcefields() const
{
	return forcefields_.nItems();
}

// Set active forcefield
void Aten::setCurrentForcefield(Forcefield *ff)
{
	current.ff = ff;
}

// Set active forcefield by ID
void Aten::setCurrentForcefield(int id)
{
	current.ff = forcefields_[id];
}

// Return the active forcefield
Forcefield *Aten::currentForcefield() const
{
	return current.ff;
}

// Return ID of current forcefield
int Aten::currentForcefieldId() const
{
	return forcefields_.indexOf(current.ff);
}

// Get the current default forcefield
Forcefield *Aten::defaultForcefield() const
{
	return defaultForcefield_;
}

/*
// Locations
*/

// Set location of users's home directory
void Aten::setHomeDir(const char *path)
{
	homeDir_ = path;
}

// Return the home directory path
const char *Aten::homeDir()
{
	return homeDir_.get();
}

// Set working directory
void Aten::setWorkDir(const char *path)
{
	workDir_ = path;
}

// Return the working directory path
const char *Aten::workDir()
{
	return workDir_.get();
}

// Set data directory
void Aten::setDataDir(const char *path)
{
	dataDir_ = path;
}

// Return the data directory path
const char *Aten::dataDir()
{
	return dataDir_.get();
}

/*
// Filters
*/

// Load filters
bool Aten::openFilters()
{
	msg.enter("Aten::openFilters");
	char filename[512], path[512];
	bool found = FALSE, failed = FALSE;
	ifstream *listfile;

	// If ATENDATA is set, take data from there
	if (!aten.dataDir_.empty())
	{
		msg.print(Messenger::Verbose, "$ATENDATA points to '%s'.\n", dataDir_.get());
		sprintf(path,"%s%s", dataDir_.get(), "/filters/");
		sprintf(filename,"%s%s", dataDir_.get(), "/filters/index");
		listfile = new ifstream(filename,ios::in);
		if (listfile->is_open())
		{
			if (parseFilterIndex(path, listfile)) found = TRUE;
			else failed = TRUE;
		}
		listfile->close();
		delete listfile;
	}
	else msg.print(Messenger::Verbose, "$ATENDATA has not been set. Searching default locations...\n");
	if ((!found) && (!failed))
	{
		// Try a list of known locations. Set dataDir again if we find a valid one
		sprintf(path,"%s%s", "/usr/share/aten", "/filters/");
		msg.print(Messenger::Verbose, "Looking for filter index in '%s'...\n", path);
		sprintf(filename,"%s%s", path, "index");
		listfile = new ifstream(filename, ios::in);
		if (listfile->is_open())
		{
			if (parseFilterIndex(path, listfile))
			{
				found = TRUE;
				dataDir_ = "/usr/share/aten/";
			}
			else failed = TRUE;
		}
		listfile->close();
		delete listfile;

		if ((!found) && (!failed))
		{
			sprintf(path,"%s%s", "/usr/local/share/aten", "/filters/");
			msg.print(Messenger::Verbose, "Looking for filter index in '%s'...\n", path);
			sprintf(filename,"%s%s", path, "index");
			listfile = new ifstream(filename, ios::in);
			if (listfile->is_open())
			{
				if (parseFilterIndex(path, listfile))
				{
					found = TRUE;
					dataDir_ = "/usr/local/share/aten/";
				}
				else failed = TRUE;
			}
			listfile->close();
			delete listfile;
		}

		if ((!found) && (!failed))
		{
			sprintf(path,"%s%s", qPrintable(gui.app->applicationDirPath()), "/../share/aten/filters/");
			msg.print(Messenger::Verbose, "Looking for filter index in '%s'...\n", path);
			sprintf(filename,"%s%s", path, "index");
			listfile = new ifstream(filename, ios::in);
			if (listfile->is_open())
			{
				if (parseFilterIndex(path, listfile))
				{
					found = TRUE;
					dataDir_ = qPrintable(gui.app->applicationDirPath() + "/../share/aten/");
				}
				else failed = TRUE;
			}
			listfile->close();
			delete listfile;
		}

		if ((!found) && (!failed))
		{
			sprintf(path,"%s%s", qPrintable(gui.app->applicationDirPath()), "/../SharedSupport/filters/");
			msg.print(Messenger::Verbose, "Looking for filter index in '%s'...\n", path);
			sprintf(filename,"%s%s", path, "index");
			listfile = new ifstream(filename, ios::in);
			if (listfile->is_open())
			{
				if (parseFilterIndex(path, listfile))
				{
					found = TRUE;
					dataDir_ = qPrintable(gui.app->applicationDirPath() + "/../SharedSupport/");
				}
				else failed = TRUE;
			}
			listfile->close();
			delete listfile;
		}

		if (!found)
		{
			msg.print(Messenger::Error, "No filter index found in any of these locations.\n");
			msg.print(Messenger::Error, "Set $ATENDATA to point to the (installed) location of the 'data' directory, or to the directory that contains Aten's 'filters' directory.\n");
			msg.print(Messenger::Error, "e.g. (in bash) 'export ATENDATA=/usr/share/aten/' on most systems.\n");
		}
	}

	// Try to load user filters
	if (found && (!failed))
	{
		sprintf(path,"%s%s", homeDir_.get(), "/.aten/filters/");
		msg.print(Messenger::Verbose, "Looking for user filter index in '%s'...\n", path);
		sprintf(filename, "%s%s", path, "index");
		listfile = new ifstream(filename, ios::in);
		if (listfile->is_open())
		{
			if (parseFilterIndex(path, listfile)) found = TRUE;
			else failed = TRUE;
		}
		listfile->close();
		delete listfile;
	}

	// Print out info and partner filters if all was successful
	if ((!failed) && found)
	{
		partnerFilters();
		msg.print(Messenger::Verbose, "Found (import/export):  Models (%i/%i) ", filters_[Filter::ModelImport].nItems(), filters_[Filter::ModelExport].nItems());
		msg.print(Messenger::Verbose, "Trajectory (%i/%i) ", filters_[Filter::TrajectoryImport].nItems(), filters_[Filter::TrajectoryExport].nItems());
		msg.print(Messenger::Verbose, "Expression (%i/%i) ", filters_[Filter::ExpressionImport].nItems(), filters_[Filter::ExpressionExport].nItems());
		msg.print(Messenger::Verbose, "Grid (%i/%i)\n", filters_[Filter::GridImport].nItems(), filters_[Filter::GridExport].nItems());
	}
	msg.exit("Aten::openFilters");
	if (failed || (!found)) return FALSE;
	else return TRUE;
}

// Reload filters
bool Aten::reloadFilters()
{
	msg.enter("Aten::reloadFilters");
	char indexfile[512], path[512], filterfile[128], message[512], shortname[128];
	ifstream *file;
	msg.print("Clearing current filters....\n");
	filters_[Filter::ModelImport].clear();
	filters_[Filter::ModelExport].clear();
	filters_[Filter::TrajectoryImport].clear();
	filters_[Filter::TrajectoryExport].clear();
	filters_[Filter::ExpressionImport].clear();
	filters_[Filter::ExpressionExport].clear();
	filters_[Filter::GridImport].clear();
	filters_[Filter::GridExport].clear();
	sprintf(path,"%s%s", dataDir_.get(), "/filters");
	msg.print("Reading filters from '%s'...\n", path);
	sprintf(indexfile, "%s/%s", path, "index");
	file = new ifstream(indexfile, ios::in);
	if (file->is_open())
	{
		while (!file->eof())
		{
			if (parser.getArgsDelim(file, Parser::SkipBlanks) != 0) break;
			strcpy(shortname, parser.argc(0));
			sprintf(filterfile, "%s/%s", path, shortname);
			if (!loadFilter(filterfile))
			{
				sprintf(message, "Error(s) encountered while reading filter file '%s' (see message box for details).\nOne or more filters contained within will be unavailable.\n", shortname);
				QMessageBox::warning(gui.mainWindow, "Aten", message, QMessageBox::Ok);
			}
		}
		// Print out info and partner filters 
		partnerFilters();
		msg.print("Found (import/export):  Models (%i/%i) ", filters_[Filter::ModelImport].nItems(), filters_[Filter::ModelExport].nItems());
		msg.print("Trajectory (%i/%i) ", filters_[Filter::TrajectoryImport].nItems(), filters_[Filter::TrajectoryExport].nItems());
		msg.print("Expression (%i/%i) ", filters_[Filter::ExpressionImport].nItems(), filters_[Filter::ExpressionExport].nItems());
		msg.print("Grid (%i/%i)\n", filters_[Filter::GridImport].nItems(), filters_[Filter::GridExport].nItems());
	}
	file->close();
	delete file;
	msg.exit("Aten::reloadFilters");
	return TRUE;
}

// Parse filter index file
bool Aten::parseFilterIndex(const char *path, ifstream *indexfile)
{
	msg.enter("Aten::parseFilterIndex");
	// Read filter names from file and open them
	char filterfile[512], s[512], bit[64];
	strcpy(s, "--> ");
	while (!indexfile->eof())
	{
		strcpy(filterfile,path);
		if (parser.getArgsDelim(indexfile, Parser::SkipBlanks) != 0) break;
		strcat(filterfile, parser.argc(0));
		sprintf(bit, "%s  ",parser.argc(0));
		strcat(s, bit);
		if (!loadFilter(filterfile))
		{
			msg.exit("Aten::parseFilterIndex");
			return FALSE;
		}
	}
	strcat(s, "\n");
	msg.print(Messenger::Verbose, s);
	msg.exit("Aten::parseFilterIndex");
	return TRUE;
}

// Read commands from filter file
bool Aten::loadFilter(const char *filename)
{
	msg.enter("Aten::loadFilter");
	Filter::FilterType ft;
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
		ft = Filter::filterType(parser.argc(0));
		// Unrecognised filter section?
		if (ft == Filter::nFilterTypes)
		{
			msg.print(Messenger::Error, "Unrecognised section '%s' in filter.\n",parser.argc(0));
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
			msg.print(Messenger::Error, "Error reading '%s' section from file '%s'\n", Filter::filterType(newfilter->type()), filename);
			filters_[ft].remove(newfilter);
			error = TRUE;
			break;
		}
	}
	filterfile.close();
	//variables.print();
	msg.exit("Aten::loadFilter");
	return (!error);
}

// Set filter partners
void Aten::partnerFilters()
{
	msg.enter("Aten::partnerFilters");
	// Loop through import filters and search / set export partners
	char s[512], bit[32];
	Filter *imp, *exp;
	int importid;
	strcpy(s,"Model Formats:");
	for (imp = filters_[Filter::ModelImport].first(); imp != NULL; imp = imp->next)
	{
		importid = imp->id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters_[Filter::ModelExport].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->id())
				{
					msg.print(Messenger::Verbose, "--- Partnering model filters for '%s', id = %i\n", imp->nickname(), imp->id());
					imp->setPartner(exp);
					break;
				}
			}
		}
		sprintf(bit, " %s[r%c]", imp->nickname(), exp == NULL ? 'o' : 'w');
		strcat(s,bit);
	}
	strcat(s, "\n");
	msg.print(Messenger::Verbose, s);
	strcpy(s,"Grid Formats:");
	for (imp = filters_[Filter::GridImport].first(); imp != NULL; imp = imp->next)
	{
		importid = imp->id();
		exp = NULL;
		if (importid != -1)
		{
			// Search for export filter with same ID as the importfilter
			for (exp = filters_[Filter::GridExport].first(); exp != NULL; exp = exp->next)
			{
				if (importid == exp->id())
				{
					msg.print(Messenger::Verbose, "--- Partnering grid filters for '%s', id = %i\n", imp->nickname(), imp->id());
					imp->setPartner(exp);
					printf("w]");
					break;
				}
			}
		}
		sprintf(bit, " %s[r%c]", imp->nickname(), exp == NULL ? 'o' : 'w');
		strcat(s,bit);
	}
	strcat(s, "\n");
	msg.print(Messenger::Verbose, s);
	msg.exit("Aten::partnerFilters");
}

// Find filter with specified type and nickname
Filter *Aten::findFilter(Filter::FilterType ft, const char *nickname) const
{
	msg.enter("Aten::findFilter");
	Filter *result;
	for (result = filters_[ft].first(); result != NULL; result = result->next)
		if (strcmp(result->nickname(), nickname) == 0) break;
	if (result == NULL) msg.print("No %s filter with nickname '%s' defined.\n", Filter::filterType(ft), nickname);
	msg.exit("Aten::findFilter");
	return result;
}

// Return first filter in list (of a given type)
Filter *Aten::filters(Filter::FilterType ft) const
{
	return filters_[ft].first();
}

/*
// Progress Indicators
*/

// Initialise a progress indicator
void Aten::initialiseProgress(const char *jobtitle, int totalsteps)
{
	gui.progressCreate(jobtitle, totalsteps);
}

// Update the number of steps (returns if the dialog was canceled)
bool Aten::updateProgress(int currentstep)
{
	return gui.progressUpdate(currentstep);
}

// Terminate the current progress
void Aten::cancelProgress()
{
	gui.progressTerminate();
}

/*
// Grid clipboard functions
*/

// Copy specified grid
void Aten::copyGrid(Grid *g)
{
	// If there is an old grid here, delete it first
	if (gridClipboard_ != NULL) delete gridClipboard_;
	*gridClipboard_ = *g;
}

// Return grid on clipboard
Grid *Aten::gridClipboard()
{
	return gridClipboard_;
}

