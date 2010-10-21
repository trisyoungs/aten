/*
	*** Aten's master structure
	*** src/main/aten.h
	Copyright T. Youngs 2007-2010

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

#ifndef ATEN_ATEN_H
#define ATEN_ATEN_H

#include "base/bundle.h"
#include "base/kvmap.h"
#include "model/fragment.h"
#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/namemap.h"
#include "command/commands.h"
#include "parser/forest.h"
#include "parser/tree.h"
#include "parser/variablelist.h"

// Forward Declarations
class Model;
class Forcefield;
class Grid;
class Clipboard;

// Master class
class Aten
{
	public:
	// Constructor / Destructor
	Aten();
	~Aten();
	// Program mode enum
	enum ProgramMode { CommandMode, InteractiveMode, GuiMode, BatchExportMode, BatchProcessMode, ProcessAndExportMode, NoMode };
	// Target list for model creation
	enum TargetModelList { MainModelList, FragmentLibraryList };
	// Remove all dynamic data
	void clear();


	/*
	// Current Objects
	*/
	public:
	// Current object Bundle
	Bundle current;


	/*
	// Models
	*/
	private:
	// Internal count for naming new models.
	int modelId_;
	// List of models
	List<Model> models_;
	// Current target list for new generation of models
	Aten::TargetModelList targetModelList_;

	public:
	// Sets the current active model for editing
	void setCurrentModel(Model*);
	// Return current active model for editing
	Model *currentModel() const;
	// Return current active model for editing, accounting for trajectory frames
	Model *currentModelOrFrame() const;
	// Return first item in the model list
	Model *models() const;
	// Return nth item in the model list
	Model *model(int n);
	// Return pointer to model list
	const List<Model> *modelList() const;
	// Return the current model's index in the model list
	int currentModelId() const;
	// Return index of specified model
	int modelIndex(Model *m) const;
	// Return the number of models in the model list
	int nModels() const;
	// Add a new model to the workspace
	Model* addModel();
	// Remove specified model from the list
	void removeModel(Model*);
	// Find model by name
	Model *findModel(const char *name) const;


	/*
	// Filters
	*/
	private:
	// How many filters had errors on startup
	int nFiltersFailed_;
	// Filenames (including paths) of filters that failed to load
	List<Dnchar> failedFilters_;
	// Parse directory index and load filters
	int parseFilterDir(const char *path);
	// Set export partners for import filters
	void partnerFilters();
	// List of Filter Forests
	List<Forest> filterForests_;
	// Reflists of file filters of different types
	Reflist<Tree,int> filters_[FilterData::nFilterTypes];

	public:
	// Load filters
	void openFilters();
	// Load filter from specified filename
	bool openFilter(const char *filename);
	// Register a filter of a given type
	void registerFilter(Tree *filter, FilterData::FilterType ft);
	// Whether filters loaded succesfully on startup
	int nFiltersFailed() const;
	// Return first item in failed filter list
	Dnchar *failedFilters() const;
	// Reload filters
	int reloadFilters();
	// Probe file for its format
	Tree *probeFile(const char *filename, FilterData::FilterType);
	// Find filter of specified type with nickname provided
	Tree *findFilter(FilterData::FilterType ft, const char *nickname) const;
	// Find filter by description
	Tree *findFilterByDescription(FilterData::FilterType ft, const char *description) const;
	// Return first filter in list (of a given type)
	Refitem<Tree,int> *filters(FilterData::FilterType ft) const;
	// Return nth filter in list (of a given type)
	Refitem<Tree,int> *filter(FilterData::FilterType ft, int index);
	// Return number of filters of a given type
	int nFilters(FilterData::FilterType ft) const;
	// Return pointer to list of filters of given type
	Reflist<Tree,int> *filterList(FilterData::FilterType ft);
	// Print list of valid filter nicknames
	void printValidNicknames(FilterData::FilterType ft);


	/*
	// Forcefields
	*/
	private:
	// List of loaded forcefields
	List<Forcefield> forcefields_;
	// Default forcefield to use when no other has been applied
	Forcefield *defaultForcefield_;

	public:
	// Add a new forcefield
	Forcefield *addForcefield(const char *name = NULL);
	// Load the specified forcefield
	Forcefield *loadForcefield(const char *filename);
	// Find forcefield by name
	Forcefield *findForcefield(const char *name) const;
	// Set the default forcefield
	void setDefaultForcefield(Forcefield *ff);
	// Get the current default forcefield
	Forcefield *defaultForcefield() const;
	// Return the first ff in the list
	Forcefield *forcefields() const;
	// Return the nth forcefield in the list
	Forcefield *forcefield(int n);
	// Return the number of loaded forcefields
	int nForcefields() const;
	// Set active forcefield
	void setCurrentForcefield(Forcefield *ff);
	// Set active forcefield by ID
	void setCurrentForcefield(int id);
	// Return the active forcefield
	Forcefield *currentForcefield() const;
	// Return ID of current forcefield
	int currentForcefieldId() const;
	// Remove specified forcefield
	void removeForcefield(Forcefield*);
	// Remove FF references from the model list
	void dereferenceForcefield(Forcefield*);


	/*
	// Clipboards
	*/
	private:
	// Grid 'clipboard'
	Grid* gridClipboard_;

	public:
	// User clipboard
	Clipboard *userClipboard;
	// Copy specified grid
	void copyGrid(Grid *g);
	// Return grid on clipboard
	Grid *gridClipboard();
	

	/*
	// Program locations
	*/
	private:
	// Location of user's home directory
	Dnchar homeDir_;
	// Current working directory
	Dnchar workDir_;
	// Data directory
	Dnchar dataDir_;
	// Whether data dir has been set
	bool dataDirSet_;
	// Name of user's 'aten' directory within their homedir
	Dnchar atenDir_;

	public:
	// Set location of users's home directory
	void setHomeDir(const char *path);
	// Return the current home directory location
	const char *homeDir() const;
	// Set working directory
	void setWorkDir(const char *path);
	// Return the current working directory
	const char *workDir() const;
	// Set data directory
	void setDataDir(const char *path);
	// Return the data directory path
	const char *dataDir() const;
	// Return whether the data dir has already been set
	bool dataDirSet() const;
	// Return the aten directory name
	const char *atenDir() const;


	/*
	// Program Modes
	*/
	private:
	// Current mode of program operation
	ProgramMode programMode_;

	public:
	// Sets the current program mode
	void setProgramMode(ProgramMode pm);
	// Return the current program mode
	ProgramMode programMode() const;


	/*
	// Scripts
	*/
	private:
	// List of loaded scripts
	List<Forest> scripts_;

	public:
	// Add script to list
	Forest *addScript();
	// Remove specified script
	void removeScript(Forest *script);
	// Return number of loaded scripts
	int nScripts();
	// Return first script in list
	Forest *scripts();
	// Return n'th script in list
	Forest *script(int n);


	/*
	// Program Control / Settings (not Prefs)
	*/
	private:
	// Selected drawing element
	short int sketchElement_;
	// Whether type export conversion is enabled
	bool typeExportMapping_;

	public:
	// Set current drawing element
	void setSketchElement(short int el);
	// Return current drawing element
	short int sketchElement() const;
	// Element map name conversions to apply on load
	List< NameMap<int> > typeImportMap;
	// Type map name conversions to apply on save
	KVMap typeExportMap;
	// Set whether type export conversion is enabled
	void setTypeExportMapping(bool b);
	// Return whether type export conversion is enabled
	bool typeExportMapping() const;
	// Convert supplied type name according to export type map
	const char *typeExportConvert(const char *s) const;


	/*
	// Progress Indicators
	*/
	public:
	// Initialise a progress indicator
	void initialiseProgress(const char *jobtitle, int totalsteps);
	// Update the number of steps (returns if the dialog was canceled)
	bool updateProgress(int currentstep = -1);
	// Terminate the current progress
	void cancelProgress();


	/*
	// CLI
	*/
	private:
	// Print usage information
	void printUsage() const;
	// Variable list holding vars set from CLI
	VariableList passedValues_;
	// Add passed value
	bool addPassedValue(VTypes::DataType dt, const char *name, const char *value);

	public:
	// Parse early command line options, before filter / prefs load
	bool parseCliEarly(int, char**);
	// Parse command line options (after filter / prefs load
	int parseCli(int, char**);
	// Find passed value
	Variable *findPassedValue(const char *name) const;


	/*
	// Single-shot program modes
	*/
	private:
	// Model format in which to export models
	Tree *exportFilter_;
	// Cached commands to use in batch processing mode
	List<Forest> batchCommands_;

	public:
	// Set format to use in export
	void setExportFilter(Tree *f);
	// Export all currently loaded models in the referenced format
	void exportModels();
	// Add set of batch commands
	Forest *addBatchCommand();
	// Run all stored commands on all loaded models
	void processModels();
	// Save all models under their original names
	void saveModels();


	/*
	// Commands
	*/
	public:
	// Command Definitions
	Command commands;
	// Preferences file commands
	Forest prefsCommands;

	/*
	// Fragment Library
	*/
	private:
	// Models making up fragment library
	List<Model> fragmentModels_;
	// Groups of fragments within the library
	List<FragmentGroup> fragmentGroups_;
	// Search for name fragment group
	FragmentGroup *findFragmentGroup(const char *name);
	// Internal count for naming new fragments
	int fragmentModelId_;
	// Parse fragment directory
	bool parseFragmentDir(const char *path, const char *groupname);

	public:
	// Add new fragment model from specified model's current selection
	void addFragmentFromSelection(Model *source, const char *parentgroup);
	// Load fragment library
	void openFragments();
	// Return first fragment library
	FragmentGroup *fragmentGroups();
};

extern Aten aten;

#endif
