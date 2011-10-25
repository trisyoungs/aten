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
	redirectedImagesActive_ = FALSE;
	redirectedImageFormat_ = "";
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = 100;

	// Misc 
	#ifdef _WIN32
	homeDir_ = "C:\\";
	atenDir_ = "aten";
	#else
	homeDir_ = "/tmp";
	atenDir_ = ".aten";
	#endif
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

// Return whether saveImage redirect is active (for scripted movie making)
bool Aten::redirectedImagesActive()
{
	return redirectedImagesActive_;
}

// Initialise image redirection
void Aten::initialiseImageRedirect(const char *filenameFormat, int maxImages)
{
	redirectedImageCount_ = 0;
	maxRedirectedImages_ = maxImages;
	redirectedImageFormat_ = filenameFormat;
	redirectedImagesActive_ = TRUE;
	msg.print(Messenger::Verbose, "Image redirect active - name format = [%s], maxImages = %i\n", redirectedImageFormat_.get(), maxRedirectedImages_);
}

// Return next filename for image redirection
const char *Aten::nextRedirectedFilename()
{
	if (redirectedImageCount_ == maxRedirectedImages_) return NULL;
	static Dnchar filename;
	filename.sprintf(redirectedImageFormat_.get(), redirectedImageCount_++);
	return filename.get();
}

// Cancel image redirection
int Aten::cancelImageRedirect()
{
	redirectedImagesActive_ = FALSE;
	return redirectedImageCount_;
}

/*
// GUI Routines
*/

// Set the active model
void Aten::setCurrentModel(Model *m, bool deselectOthers)
{
	msg.enter("Aten::setCurrentModel");
	if (m == NULL)
	{
		current.clear();
		msg.exit("Aten::setCurrentModel");
		return;
	}
	// Set Bundle pointers
	current.m = m;
	current.p = m->patterns();
	current.g = m->grids();
	current.i = NULL;
	// Deselect all other models if specified
	if (deselectOthers)
	{
		// Unset visible flags on all currently-visible models
		for (Refitem<Model,int> *ri = visibleModels_.first(); ri != NULL; ri = ri->next) ri->item->setVisible(FALSE);
		visibleModels_.clear();
	}
	// Its the current model, so it must be visible also... add to visible list
	setModelVisible(m, TRUE);
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
			setCurrentModel(m, TRUE);
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
	// Delete the old model
	models_.remove(xmodel);
	visibleModels_.remove(xmodel);
	msg.exit("Aten::removeModel");
}

// Close specified model, saving first if requested
bool Aten::closeModel(Model *m)
{
	// If the current model has been modified, ask for confirmation before we close it
	Dnchar text;
	Tree *filter;
	if (m->changeLog.isModified())
	{
		// Create a modal message dialog
		text.sprintf("Model '%s' has been modified.\n", m->name());
		int returnvalue = QMessageBox::warning(gui.mainWindow(), "Aten", text.get(), QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel, QMessageBox::Save);
		switch (returnvalue)
		{
			// Discard changes
			case (QMessageBox::Discard):
				aten.removeModel(m);
				// Update GUI
				gui.update(GuiQt::AllTarget);
				break;
				// Cancel close
			case (QMessageBox::Cancel):
				return FALSE;
				// Save model before quit
			case (QMessageBox::Save):
				// Temporarily disable undo/redo for the model, save, and re-enable
				m->disableUndoRedo();
				// If model has a filter set, just save it
				filter = m->filter();
				if (filter != NULL) filter->executeWrite(m->filename());
				else if (gui.mainWindow()->runSaveModelDialog())
				{
					m->setFilter(gui.mainWindow()->saveModelFilter);
					m->setFilename(gui.mainWindow()->saveModelFilename.get());
					if (!gui.mainWindow()->saveModelFilter->executeWrite(gui.mainWindow()->saveModelFilename.get()))
					{
						msg.print("Not saved.\n");
						m->enableUndoRedo();
						return FALSE;
					}
				}
				else
				{
					m->enableUndoRedo();
					return FALSE;
				}
				aten.removeModel(m);
				// Update GUI
				gui.update(GuiQt::AllTarget);
				break;
		}
	}
	else aten.removeModel(m);
	return TRUE;
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

// Return n'th visible model
Model *Aten::visibleModel(int id)
{
	if ((id < 0) || (id >= visibleModels_.nItems()))
	{
		printf("Index %i is out of range for visible model list.\n", id);
		return NULL;
	}
	return visibleModels_[id]->item;
}

// Log specified change(s) in all models
void Aten::globalLogChange(Log::LogType log)
{
	// Loop over all loaded models and log change in their current rendersource
	for (Model *m = models_.first(); m != NULL; m = m->next) m->renderSourceModel()->changeLog.add(log);
}

/*
// Forcefield Management routines
*/

// Add Forcefield
Forcefield *Aten::addForcefield(const char *name)
{
	current.ff = forcefields_.add();
	if (name != NULL) current.ff->setName(name);
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
			filepath.sprintf("%s%c%s%cff%c%s", homeDir_.get(), PATHSEP, atenDir_.get(), PATHSEP, PATHSEP, filename);
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
		msg.print("Forcefield '%s' is now the default.\n", newff->name());
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
	if (current.ff == xff) current.ff = forcefields_.first();
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
	if (current.ff == NULL) msg.print("Default forcefield has been unset.\n");
	else msg.print("Default forcefield is now '%s'.\n", current.ff->name());
}

// Set active forcefield by ID
void Aten::setCurrentForcefield(int id)
{
	setCurrentForcefield(forcefields_[id]);
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

