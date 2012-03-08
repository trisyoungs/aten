/*
	*** Aten's master structure
	*** src/main/aten.h
	Copyright T. Youngs 2007-2012

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

#include "base/dnchar.h"
#include "base/kvmap.h"
#include "model/bundle.h"
#include "model/fragment.h"
#include "templates/list.h"
#include "parser/program.h"
#include "parser/variablelist.h"
#include "methods/partition.h"

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
	enum ProgramMode { CommandMode, InteractiveMode, GuiMode, ExportMode, BatchMode, ProcessMode, BatchExportMode, NoMode };
	// Target list for model creation
	enum TargetModelList { MainModelList, FragmentLibraryList, WorkingModelList };
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
	// Temporary, working model list
	List<Model> workingModels_;
	// Current target list for new generation of models
	Aten::TargetModelList targetModelList_;
	// Reflist of visible models
	Reflist<Model,int> visibleModels_;
	
	public:
	// Set usage of working model list
	void setUseWorkingList(bool b);
	// Return list of working models
	Model *workingModels() const;
	// Sets the current active model for editing
	void setCurrentModel(Model *m, bool deselectOthers = FALSE);
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
	void removeModel(Model *m);
	// Close specified model, saving first if requested
	bool closeModel(Model *m);
	// Find model by name
	Model *findModel(const char *name) const;
	// Set visible flag for specified model
	void setModelVisible(Model *m, bool visible);
	// Return number of visible models
	int nVisibleModels();
	// Return reflist of visible models
	Refitem<Model, int> *visibleModels();
	// Return n'th visible model
	Model *visibleModel(int id);
	// Log specified change(s) in all models
	void globalLogChange(Log::LogType);


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
	// List of Filter programs
	List<Program> filterPrograms_;
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
	// Global Function Includes and Partitioning Schemes
	*/
	private:
	// Program containing all globally-defined include functions
	Program includeFunctions_;
	// How many include files had errors on startup
	int nIncludesFailed_;
	// Filenames (including paths) of includes that failed to load
	List<Dnchar> failedIncludes_;
	// Parse directory index and load includes
	int parseIncludeDir(const char *path);
	// Programs containing partitioning schemes
	List<PartitioningScheme> partitioningSchemes_;
	// How many partitioning files had errors on startup
	int nPartitioningSchemesFailed_;
	// Filenames (including paths) of partitions that failed to load
	List<Dnchar> failedPartitioningSchemes_;
	// Parse directory index and load includes
	int parsePartitionsDir(const char *path);
	
	public:
	// Load global include functions
	void openIncludes();
	// Load include from specified filename
	bool openInclude(const char *filename);
	// Whether includes loaded succesfully on startup
	int nIncludesFailed() const;
	// Return first item in failed includes list
	Dnchar *failedIncludes() const;
	// Find global include function by name
	Tree *findIncludeFunction(const char *name);
	// Load global partition functions
	void openPartitions();
	// Load partition from specified filename
	bool openPartition(const char *filename);
	// Whether partitions loaded succesfully on startup
	int nPartitioningSchemesFailed() const;
	// Return first item in failed partitions list
	Dnchar *failedPartitioningSchemes() const;
	// Find named partitioning scheme
	PartitioningScheme *findPartitioningScheme(const char *name);
	// Return number of partitioning schemes in the list
	int nPartitioningSchemes();
	// Return first partitioning scheme in the list
	PartitioningScheme *partitioningSchemes();
	// Return nth partitioning scheme in the list
	PartitioningScheme *partitioningSchemes(int index);
	// Copy specified partitioning scheme and add it to the list
	void addPartitioningScheme(PartitioningScheme &scheme);


	/*
	// Forcefields
	*/
	private:
	// List of loaded forcefields
	List<Forcefield> forcefields_;

	public:
	// Add a new forcefield
	Forcefield *addForcefield(const char *name = NULL);
	// Load the specified forcefield
	Forcefield *loadForcefield(const char *filename);
	// Find forcefield by name
	Forcefield *findForcefield(const char *name) const;
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
	// Program Locations
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
	List<Program> scripts_;

	public:
	// Add script to list
	Program *addScript();
	// Remove specified script
	void removeScript(Program *script);
	// Return number of loaded scripts
	int nScripts();
	// Return first script in list
	Program *scripts();
	// Return n'th script in list
	Program *script(int n);


	/*
	// Program Control / Settings (not Prefs)
	*/
	private:
	// Whether type export conversion is enabled
	bool typeExportMapping_;
	// Whether saveImage redirect is active (for scripted movie making)
	bool redirectedImagesActive_;
	// Format string (containing only a single variable integer) for image redirect filenames
	Dnchar redirectedImageFormat_;
	// Current count for image file (increased each time redirectedImageFilename() is called)
	int redirectedImageCount_;
	// Maximum allowed images to be saved before 'saveimage' command raises an error
	int maxRedirectedImages_;
	

	public:
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
	// Return whether saveImage redirect is active (for scripted movie making)
	bool redirectedImagesActive();
	// Initialise image redirection
	void initialiseImageRedirect(const char *filenameFormat, int maxFrames);
	// Return next filename for image redirection
	const char *nextRedirectedFilename();
	// Cancel image redirection, returning number of images saved
	int cancelImageRedirect();


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
	List<Program> batchCommands_;

	public:
	// Set format to use in export
	void setExportFilter(Tree *f);
	// Export all currently loaded models in the referenced format
	void exportModels();
	// Add set of batch commands
	Program *addBatchCommand();
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
