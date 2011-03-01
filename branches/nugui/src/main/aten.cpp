/*
	*** Aten's master structure
	*** src/main/aten.cpp
	Copyright T. Youngs 2007-2011

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
#include "main/version.h"
#include "gui/gui.h"
#include "gui/tcanvas.uih"
#include "gui/mainwindow.h"
#include "gui/disorder.h"
#include "gui/grids.h"
#include "gui/modellist.h"
#include "model/model.h"
#include "model/clipboard.h"
#include "ff/forcefield.h"
#include "classes/grid.h"
#include "base/pattern.h"
#include "base/sysfunc.h"
#include "parser/parser.h"

// Singleton definition
Aten aten;

// Constructor
Aten::Aten()
{
	// Models
	modelId_ = 0;
	targetModelList_ = Aten::MainModelList;

	// Default program mode
	programMode_ = Aten::GuiMode;

	// Program control / settings (not prefs)
	typeExportMapping_ = FALSE;

	// Misc 
	#ifdef _WIN32
	homeDir_ = "C:\\";
	atenDir_ = "aten";
	#else
	homeDir_ = "/tmp";
	atenDir_ = ".aten";
	#endif
	defaultForcefield_ = NULL;
	nFiltersFailed_ = 0;
	dataDirSet_ = FALSE;

	// Clipboards
	userClipboard = new Clipboard;
	gridClipboard_ = NULL;

	// Single-shot mode variables
	exportFilter_ = NULL;

	// Fragments
	fragmentModelId_ = 0;
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
	scripts_.clear();
	for (int i=0; i<FilterData::nFilterTypes; i++) filters_[i].clear();
}

// Sets the current program mode
void Aten::setProgramMode(ProgramMode pm)
{
	programMode_ = pm;
}

// Return the current program mode
Aten::ProgramMode Aten::programMode() const
{
	return programMode_;
}

/*
// Program Control / Settings (not prefs)
*/

// Set whether type export conversion is enabled
void Aten::setTypeExportMapping(bool b)
{
	typeExportMapping_ = b;
}

// Return whether type export conversion is enabled
bool Aten::typeExportMapping() const
{
	return typeExportMapping_;
}

// Convert supplied type name according to export type map
const char *Aten::typeExportConvert(const char *oldname) const
{
	if (!typeExportMapping_) return oldname;
	KVPair *kvp = aten.typeExportMap.search(oldname);
	return (kvp == NULL ? oldname : kvp->value());
}

/*
// GUI Routines
*/

// Set the active model
void Aten::setCurrentModel(Model *m)
{
	msg.enter("Aten::setCurrentModel");
	if (m == NULL)
	{
		current.clear();
		msg.exit("Aten::setCurrentModel");
		return;
	}
	// Set current.m and tell the mainview canvas to display it
	current.m = m;
	current.rs = (current.m == NULL ? NULL : current.m->renderSourceModel());
	// Set other Bundle objects based on model
	current.p = m->patterns();
	current.g = m->grids();
	current.i = NULL;
	
	// Update GUI
	if (gui.exists())
	{
		gui.modelListWidget->refresh();
		gui.update(GuiQt::AllTarget);
	}

	msg.exit("Aten::setCurrentModel");
}

/*
// Model Management routines
*/

// Set usage of working model list
void Aten::setUseWorkingList(bool b)
{
	static Bundle originalBundle;
	if (b)
	{
		originalBundle = aten.current;
		targetModelList_ = Aten::WorkingModelList;
	}
	else 
	{
		aten.current = originalBundle;
		workingModels_.clear();
		targetModelList_ = Aten::MainModelList;
	}
}

// Return list of working models
Model *Aten::workingModels() const
{
	return workingModels_.first();
}

// Return current active model for editing
Model *Aten::currentModel() const
{
	return current.m;
}

// Return current active model for editing, accounting for trajectory frames
Model *Aten::currentModelOrFrame() const
{
	return (current.m == NULL ? NULL : current.m->renderSourceModel());
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

// Return pointer to the actual model list
const List<Model> *Aten::modelList() const
{
	return &models_;
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
	Dnchar newname;
	Model *m = NULL;
	// Check current list target for model creation
	switch (targetModelList_)
	{
		case (Aten::MainModelList):
			m = models_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("Unnamed%03i", ++modelId_);
			m->setName(newname);
			m->changeLog.reset();
			setCurrentModel(m);
			break;
		case (Aten::FragmentLibraryList):
			m = fragmentModels_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("Fragment%03i", ++fragmentModelId_);
			m->setName(newname);
			m->changeLog.reset();
			m->disableUndoRedo();
			break;
		case (Aten::WorkingModelList):
			m = workingModels_.add();
			m->setType(Model::ParentModelType);
			newname.sprintf("TempModel%03i", workingModels_.nItems());
			m->setName(newname);
			m->changeLog.reset();
			m->disableUndoRedo();
			break;
		default:
			printf("Internal Error: No target list set for model creation.\n");
			break;
	}
	msg.exit("Aten::addModel");
	return m;
}

// Remove model
void Aten::removeModel(Model *xmodel)
{
	// Remove this model from the model_list in the main window
	msg.enter("Aten::removeModel");
	Model *m;
	// Delete the current model, but don't allow there to be zero models...
	if (models_.nItems() == 1) m = aten.addModel();
	else m = (xmodel->next != NULL ? xmodel->next : xmodel->prev);
	setCurrentModel(m);
	// Delete the old model (GUI first, then master)
	int id = models_.indexOf(xmodel);
	models_.remove(xmodel);
	gui.modelListWidget->refresh();
	gui.update(GuiQt::AllTarget);
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

// Set visible flag for specified model
void Aten::setModelVisible(Model *m, bool visible)
{
	// Check model pointer
	if (m == NULL) return;
	m->setVisible(visible);
	// Search list for specified model
	Refitem<Model,int> *ri = visibleModels_.contains(m);
	if ((ri == NULL) && visible) visibleModels_.add(m);
	else if ((ri != NULL) && (!visible)) visibleModels_.remove(m);
	gui.update();
}

// Return number of visible models
int Aten::nVisibleModels()
{
	return visibleModels_.nItems();
}

// Return reflist of visible models
Refitem<Model,int> *Aten::visibleModels()
{
	return visibleModels_.first();
}

/*
// Forcefield Management routines
*/

// Add Forcefield
Forcefield *Aten::addForcefield(const char *name)
{
	current.ff = forcefields_.add();
	if (name != NULL) current.ff->setName(name);
	if (forcefields_.nItems() == 1) setDefaultForcefield(current.ff);
	return current.ff;
}

// Load forcefield
Forcefield *Aten::loadForcefield(const char *filename)
{
	msg.enter("Aten::loadForcefield");
	// Try some different locations to find the supplied forcefield.
	static Dnchar filepath;
	bool result;
	Forcefield *newff = forcefields_.add();
	// First try - actual / absolute path
	msg.print(Messenger::Verbose,"Looking for forcefield in absolute path (%s)...\n",filename);
	if (fileExists(filename)) result = newff->load(filename);
	else
	{
		// Second try - aten.dataDir/ff
		filepath.sprintf("%s%cff%c%s", dataDir_.get(), PATHSEP, PATHSEP, filename);
		msg.print(Messenger::Verbose,"Looking for forcefield in installed location (%s)...\n",filepath.get());
		if (fileExists(filepath)) result = newff->load(filepath);
		else
		{
			// Last try - user home datadir/ff
			filepath.sprintf("%s%c.aten%cff%c%s", homeDir_.get(), PATHSEP, PATHSEP, PATHSEP, filename);
			msg.print(Messenger::Verbose,"Looking for forcefield in user's data directory (%s)...\n",filepath.get());
			if (fileExists(filepath)) result = newff->load(filepath);
			else
			{
				msg.print("Can't find forcefield file '%s' in any location.\n", filename);
				result = FALSE;
			}
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
			m->invalidateExpression();
		}
		if (m->patterns() != NULL)
		{
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
					m->invalidateExpression();
				}
			}
		}
		else
		{
			int count = 0;
			for (Atom *i = m->atoms(); i != NULL; i = i->next) if (xff->containsType(i->type()))
			{
				++count;
				i->setType(NULL);
			}
			if (count != 0) m->invalidateExpression();
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
const char *Aten::homeDir() const
{
	return homeDir_.get();
}

// Set working directory
void Aten::setWorkDir(const char *path)
{
	workDir_ = path;
}

// Return the working directory path
const char *Aten::workDir() const
{
	return workDir_.get();
}

// Set data directory
void Aten::setDataDir(const char *path)
{
	dataDir_ = path;
	dataDirSet_ = TRUE;
}

// Return the data directory path
const char *Aten::dataDir() const
{
	return dataDir_.get();
}

// Return whether the data dir has already been set
bool Aten::dataDirSet() const
{
	return dataDirSet_;
}

// Return the aten directory
const char *Aten::atenDir() const
{
	return atenDir_.get();
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
	gridClipboard_ = NULL;
	if (g != NULL)
	{
		gridClipboard_ = new Grid;
		*gridClipboard_ = *g;
	}
}

// Return grid on clipboard (if any)
Grid *Aten::gridClipboard()
{
	return gridClipboard_;
}

/*
// Fragment Library
*/

// Add new fragment model from specified model's current selection
void Aten::addFragmentFromSelection(Model *source, const char *parentgroup)
{
	msg.enter("Aten::addFragmentFromSelection");

	// Check source model and selection
	if (source == NULL)
	{
		printf("Internal Error : NULL model pointer passed to Aten::addFragmentFromSelection.\n");
		msg.exit("Aten::addFragmentFromSelection");
		return;
	}
	if (source->nSelected() == 0)
	{
		msg.print("Source model '%s' has no selected atoms from which to make a fragment.\n", source->name());
		msg.exit("Aten::addFragmentFromSelection");
		return;
	}

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Create new fragment model and paste in source model selection
	Clipboard clip;
	clip.copySelection(source);
	Model *m = addModel();
	clip.pasteToModel(m, FALSE);

	// Does the named fragment group already exist? If not, create new one
	FragmentGroup *fg = findFragmentGroup( parentgroup == NULL ? "New Fragments" : parentgroup );
	if (fg == NULL)
	{
		// Add default fragment group...
		fg = fragmentGroups_.add();
		fg->setName(parentgroup);
	}

	// Store the last model on the list.
	Fragment *f = fg->addFragment();
	if (!f->setMasterModel(m)) fg->removeFragment(f);

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;

	msg.exit("Aten::addFragmentFromSelection");
}

// Parse fragment directory
bool Aten::parseFragmentDir(const char *path, const char *groupname)
{
	msg.enter("Aten::parseFragmentDir");
	int i;
	Tree *t;
	Fragment *f;
	FragmentGroup *fg;
	QPixmap pixmap;

	// First check - does this directory actually exist
	QDir fragmentdir(path);
	if (!fragmentdir.exists())
	{
		msg.exit("Aten::parseFragmentDir");
		return FALSE;
	}

	// Filter the directory contents - show only files and exclude '.' and '..'
	QStringList fragmentlist = fragmentdir.entryList(QDir::Files | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<fragmentlist.size(); i++)
	{
		// Construct full filepath
		QString filename(path);
		filename += "/";
		filename += fragmentlist.at(i);
		t = aten.probeFile(qPrintable(filename), FilterData::ModelImport);
		if (t == NULL) continue;
		if (!t->executeRead(qPrintable(filename))) continue;

		// Does the named fragment group already exist? If not, create new one
		fg = findFragmentGroup(groupname);
		if (fg == NULL)
		{
			// Add default fragment group...
			fg = fragmentGroups_.add();
			fg->setName(groupname);
		}

		// Store the last model on the list.
		f = fg->addFragment();
		if (!f->setMasterModel(fragmentModels_.last())) fg->removeFragment(f);
	}

	// Check for other directories
	fragmentlist = fragmentdir.entryList(QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name);
	for (i=0; i<fragmentlist.size(); i++)
	{
		// Construct full filepath
		QString filename(path);
		filename += "/";
		filename += fragmentlist.at(i);
		parseFragmentDir(qPrintable(filename), qPrintable(fragmentlist.at(i)));
	}

	msg.exit("Aten::parseFragmentDir");
	return TRUE;
}

// Load fragment library
void Aten::openFragments()
{
	msg.enter("Aten::openFragments");
	Dnchar path;
	int nfailed;

	// Redirect model creation to fragment list
	targetModelList_ = Aten::FragmentLibraryList;

	// Default search path should have already been set by openFilters()...
	path.sprintf("%s%cfragments", dataDir_.get(), PATHSEP);
	msg.print(Messenger::Verbose, "Looking for fragments in '%s'...\n", qPrintable(QDir::toNativeSeparators(path.get())));
	nfailed = parseFragmentDir(path, "Ungrouped");

	// Try to load user fragments - we don't mind if the directory doesn't exist...
	path.sprintf("%s%c.aten%cfragments%c", homeDir_.get(), PATHSEP, PATHSEP, PATHSEP);
	msg.print(Messenger::Verbose, "Looking for user fragments in '%s'...\n", path.get());
	nfailed = parseFragmentDir(path, "Ungrouped");

	// Return model creation to main list
	targetModelList_ = Aten::MainModelList;
	aten.setCurrentModel(NULL);

	// Print out info
	int nfragments = 0;
	for (FragmentGroup *fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) nfragments += fg->nFragments();
	msg.print("Loaded %i fragments into library.\n", nfragments);

	msg.exit("Aten::openFragments");
}

// Search for name fragment group
FragmentGroup *Aten::findFragmentGroup(const char *name)
{
	for (FragmentGroup *fg = fragmentGroups_.first(); fg != NULL; fg = fg->next) if (strcmp(name,fg->name()) == 0) return fg;
	return NULL;
}

// Return head of fragments list
FragmentGroup *Aten::fragmentGroups()
{
	return fragmentGroups_.first();
}

/*
// Scripts
*/

// Add script to list
Program *Aten::addScript()
{
	return scripts_.add();
}

// Remove specified script
void Aten::removeScript(Program *script)
{
	scripts_.remove(script);
}

// Return number of loaded scripts
int Aten::nScripts()
{
	return scripts_.nItems();
}

// Return first script in list
Program *Aten::scripts()
{
	return scripts_.first();
}

// Return n'th script in list
Program *Aten::script(int n)
{
	if ((n < 0) || (n >= scripts_.nItems())) msg.print("Script %i is out of range.\n", n);
	else return scripts_[n];
	return NULL;
}

