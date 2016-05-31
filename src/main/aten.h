/*
	*** Aten's master structure
	*** src/main/aten.h
	Copyright T. Youngs 2007-2016

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

#include <QDir>
#include "base/kvmap.h"
#include "base/prefs.h"
#include "base/encoderdefinition.h"
#include "model/bundle.h"
#include "model/fragment.h"
#include "model/fragmentgroup.h"
#include "templates/list.h"
#include "parser/program.h"
#include "parser/variablelist.h"
#include "methods/partitioningscheme.h"
#include "gui/useractions.h"
#include "plugins/pluginstore.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class AtenWindow;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
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

	private:
	// Pointer to AtenWindow
	AtenWindow* atenWindow_;

	public:
	// Set pointer to AtenWindow
	void setAtenWindow(AtenWindow* atenWindow);
	// Return pointer to AtenWindow
	AtenWindow* atenWindow();


	/*
	 * Models
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
	// RefList of visible models
	RefList<Model,int> visibleModels_;
	
	public:
	// Set usage of working model list
	void setUseWorkingList(bool b);
	// Return list of working models
	Model* workingModels() const;
	// Return first item in the model list
	Model* models() const;
	// Return nth item in the model list
	Model* model(int n);
	// Return whether model exists
	bool isModel(Model* model) const;
	// Return the current model's index in the model list
	int currentModelId() const;
	// Return index of specified model
	int modelIndex(Model* model) const;
	// Return the number of models in the model list
	int nModels() const;
	// Add a new model to the workspace
	Model* addModel();
	// Remove specified model from the list
	void removeModel(Model* xmodel);
	// Find model by name
	Model* findModel(QString name) const;
	// Set visible flag for specified model
	void setModelVisible(Model* m, bool visible);
	// Set the specified model to be the only one visible
	void setSingleModelVisible(Model* m);
	// Return number of visible models
	int nVisibleModels();
	// Return reflist of visible models
	RefListItem<Model, int>* visibleModels();
	// Return n'th visible model
	Model* visibleModel(int id);
	// Log specified change(s) in all models
	void globalLogChange(Log::LogType);
	// Load model (if it is not loaded already)
	bool loadModel(QString fileName, Tree* filter = NULL);
	// Open model (if it is not loaded already)
	bool openModel(QString fileName, IOPluginInterface* plugin = NULL);


	/*
	 * Filters
	 */
	private:
	// Filenames (including paths) of filters that failed to load
	QStringList failedFilters_;
	// Set export partners for import filters
	void partnerFilters();
	// List of Filter programs
	List<Program> filterPrograms_;
	// RefLists of file filters of different types
	RefList<Tree,int> filters_[FilterData::nFilterTypes];
	// Filter strings for filter file dialogs
	QString fileDialogFilters_[FilterData::nFilterTypes];
	// Filter strings for bitmap file dialogs
	QString bitmapFileDialogFilters_;

	private:
	// Search specified directory for filters
	int searchFilterDir(QDir path);

	public:
	// Load filters
	void loadFilters();
	// Load filter from specified filename
	bool loadFilter(QString filename);
	// Create filter strings for file dialogs
	void createFileDialogFilters();
	// Register a filter of a given type
	void registerFilter(Tree* filter, FilterData::FilterType filterType);
	// Return current number of filter programs
	int nFilterPrograms() const;
	// Return first item in failed filter list
	QStringList failedFilters() const;
	// Reload filters
	int reloadFilters();
	// Probe file for its format
	Tree* probeFile(QString filename, FilterData::FilterType filterType);
	// Find filter of specified type with nickname provided
	Tree* findFilter(FilterData::FilterType filterType, QString nickname) const;
	// Find filter by description
	Tree* findFilterByDescription(FilterData::FilterType filterType, QString description) const;
	// Find filter by id
	Tree* findFilterByID(FilterData::FilterType filterType, int id) const;
	// Return first filter in list (of a given type)
	RefListItem<Tree,int>* filters(FilterData::FilterType filterType) const;
	// Return nth filter in list (of a given type)
	RefListItem<Tree,int>* filter(FilterData::FilterType filterType, int index);
	// Return number of filters of a given type
	int nFilters(FilterData::FilterType filterType) const;
	// Return pointer to list of filters of given type
	RefList<Tree,int>* filterList(FilterData::FilterType filterType);
	// Print list of valid filter nicknames
	void printValidNicknames(FilterData::FilterType filterType);
	// Return filter strings for file dialogs
	const QString& fileDialogFilters(FilterData::FilterType filterType) const;
	// Return filter strings for bitmap file dialogs
	const QString& bitmapFileDialogFilters() const;


	/*
	 * Global Function Includes
	 */
	private:
	// Program containing all globally-defined include functions
	Program includeFunctions_;
	// How many include files had errors on startup
	int nIncludesFailed_;
	// Filenames (including paths) of includes that failed to load
	QStringList failedIncludes_;

	private:
	// Search directory for includes
	int searchIncludeDir(QDir path);

	public:
	// Load global include functions
	void loadIncludes();
	// Load include from specified filename
	bool loadInclude(QString fileName, QString name);
	// Whether includes loaded succesfully on startup
	int nIncludesFailed() const;
	// Return first item in failed includes list
	QStringList failedIncludes() const;
	// Find global include function by name
	Tree* findIncludeFunction(QString name);


	/*
	 * Partitioning Schemes
	 */
	private:
	// Programs containing partitioning schemes
	List<PartitioningScheme> partitioningSchemes_;
	// How many partitioning files had errors on startup
	int nPartitioningSchemesFailed_;
	// Filenames (including paths) of partitions that failed to load
	QStringList failedPartitioningSchemes_;
	// Partitioning scheme for Pores tool
	PartitioningScheme poresPartitioningScheme_;

	private:
	// Search specified directory for partitions
	int searchPartitionsDir(QDir path);

	public:
	// Load global partition functions
	void loadPartitions();
	// Load partition from specified filename
	bool loadPartition(QString fileName, QString name);
	// Whether partitions loaded succesfully on startup
	int nPartitioningSchemesFailed() const;
	// Return first item in failed partitions list
	QStringList failedPartitioningSchemes() const;
	// Find named partitioning scheme
	PartitioningScheme* findPartitioningScheme(QString name);
	// Return number of partitioning schemes in the list
	int nPartitioningSchemes();
	// Return first partitioning scheme in the list
	PartitioningScheme* partitioningSchemes();
	// Return nth partitioning scheme in the list
	PartitioningScheme* partitioningSchemes(int index);
	// Copy specified partitioning scheme and add it to the list
	void addPartitioningScheme(PartitioningScheme& scheme);
	// Return partitioning scheme for Pores tool
	PartitioningScheme& poresPartitioningScheme();


	/*
	 * Forcefields
	 */
	private:
	// List of loaded forcefields
	List<Forcefield> forcefields_;
	// Combination rules
	CombinationRules combinationRules_;

	public:
	// Add a new forcefield
	Forcefield* addForcefield(QString name = QString());
	// Take ownership of the specified forcefield
	void ownForcefield(Forcefield* ff);
	// Load the specified forcefield
	Forcefield* loadForcefield(QString filename);
	// Find forcefield by name
	Forcefield* findForcefield(QString name) const;
	// Return the first ff in the list
	Forcefield* forcefields() const;
	// Return the nth forcefield in the list
	Forcefield* forcefield(int n);
	// Return the number of loaded forcefields
	int nForcefields() const;
	// Set active forcefield
	void setCurrentForcefield(Forcefield* ff);
	// Set active forcefield by ID
	void setCurrentForcefield(int id);
	// Return the active forcefield
	Forcefield* currentForcefield() const;
	// Return ID of current forcefield
	int currentForcefieldId() const;
	// Remove specified forcefield
	void removeForcefield(Forcefield*);
	// Remove FF references from the model list
	void dereferenceForcefield(Forcefield*);
	// Return combination rules
	CombinationRules& combinationRules();


	/*
	 * Clipboards
	 */
	private:
	// Grid 'clipboard'
	Grid* gridClipboard_;

	public:
	// User clipboard
	Clipboard* userClipboard;
	// Copy specified grid
	void copyGrid(Grid* g);
	// Return grid on clipboard
	Grid* gridClipboard();
	

	/*
	 * Program Locations
	 */
	private:
	// Location of user's home directory
	QDir homeDir_;
	// Current working directory
	QDir workDir_;
	// Data directory
	QDir dataDir_;
	// Plugin directory
	QDir pluginDir_;
	// Name of user's 'aten' directory within their homedir
	QString atenDirName_;

	public:
	// Return the current home directory location
	QDir homeDir() const;
	// Return the current working directory
	QDir workDir() const;
	// Return the data directory path
	QDir dataDir() const;
	// Return the plugin directory path
	QDir pluginDir() const;
	// Return full path of file in Aten's data directory
	QString dataDirectoryFile(QString filename);
	// Return full path of file in user's Aten directory
	QString atenDirectoryFile(QString filename);
	// Set/get necessary directories
	void setDirectories();


	/*
	 * Session
	 */
	private:
	// Name of current session file, if any
	QString sessionFilename_;

	public:
	// Return name of current session file, if any
	QString sessionFilename();
	// Clear current session (remove all user data)
	void clearSession();
	// Load session from filename specified
	bool loadSession(QString filename);
	// Save session under specified filename
	bool saveSession(QString filename);


	/*
	 * Scripts
	 */
	private:
	// List of loaded scripts
	List<Program> scripts_;

	public:
	// Add script
	Program* addScript();
	// Remove specified script
	void removeScript(Program* script);
	// Return number of loaded scripts
	int nScripts();
	// Return first script in list
	Program* scripts();
	// Return n'th script in list
	Program* script(int n);


	/*
	 * CLI
	 */
	private:
	// Print usage information
	void printUsage() const;
	// Variable list holding vars set from CLI
	VariableList passedValues_;
	// Add passed value
	bool addPassedValue(VTypes::DataType dt, QString name, QString value);

	public:
	// Parse early command line options, before filter / prefs load
	bool parseCliEarly(int, char**);
	// Parse command line options (after filter / prefs load
	int parseCli(int, char**);
	// Find passed value
	Variable* findPassedValue(QString name) const;


	/*
	 * Program Modes / Control
	 */
	private:
	// Current mode of program operation
	ProgramMode programMode_;
	// Model format in which to export models
	Tree* exportFilter_;
	// Cached commands to use in batch processing mode
	List<Program> batchCommands_;
	// Whether type export conversion is enabled
	bool typeExportMapping_;

	public:
	// Return the current program mode
	ProgramMode programMode() const;
	// Set format to use in export
	void setExportFilter(Tree* f);
	// Export all currently loaded models in the referenced format
	void exportModels();
	// Add set of batch commands
	Program* addBatchCommand();
	// Run all stored commands on all loaded models
	void processModels();
	// Save all models under their original names
	void saveModels();
	// Type map name conversions to apply on save
	KVMap typeExportMap;
	// Set whether type export conversion is enabled
	void setTypeExportMapping(bool b);
	// Return whether type export conversion is enabled
	bool typeExportMapping() const;
	// Convert supplied type name according to export type map
	QString typeExportConvert(QString typeName) const;


	/*
	 * Commands
	 */
	private:
	// Command Definitions
	Commands commands_;

	public:
	// Return specified command keyword
	const char* commandKeyword(Commands::Function func);
	// Return specified command arguments
	const char* commandArguments(Commands::Function func);
	// Return specified return-value datatype
	VTypes::DataType commandReturnType(Commands::Function func);
	// Return whether specified command takes any arguments
	bool commandHasArguments(Commands::Function func);
	// Return specified command argument names
	const char* commandArgText(Commands::Function func);
	// Return specified command syntax
	const char* commandSyntax(Commands::Function func);
	// Execute specified command
	bool callCommand(Commands::Function cf, CommandNode* node, ReturnValue& rv);
	// Execute specified TreeNode command with specified bundle
	bool callCommand(Commands::Function cf, TreeNode* node, ReturnValue& rv, Bundle& bundle);


	/*
	 * Fragment Library
	 */
	private:
	// Models making up fragment library
	List<Model> fragments_;
	// Groups of fragments within the library
	List<FragmentGroup> fragmentGroups_;
	// Internal count for naming new fragments
	int fragmentModelId_;
	// Current fragment for drawing
	Fragment* currentFragment_;
	// Bond id for current fragment drawing
	int fragmentBondId_;

	private:
	// Search specified directory for fragments
	bool searchFragmentDir(QDir path, QString groupName);
	// Search for named fragment group
	FragmentGroup* findFragmentGroup(QString name);

	public:
	// Load fragment library
	void loadFragments();
	// Add new fragment model from specified model's current selection
	void addFragmentFromSelection(Model* source, QString parentGroup);
	// Return first fragment group
	FragmentGroup* fragmentGroups();
	// Return number of fragments available
	int nFragments();
	// Set current fragment for drawing
	void setCurrentFragment(Fragment* fragment);
	// Return current fragment for drawing
	Fragment* currentFragment();
	// Increment bond id value
	void increaseFragmentBondId();
	// Return bondId (as reference so it can be reset by associated Fragment routines)
	int& fragmentBondId();
	// Update all fragment icons
	void updateFragmentIcons();


	/*
	 * Encoder Definitions
	 */
	private:
	// List of encoder definitions
	List<EncoderDefinition> encoders_;

	public:
	// Load encoder definitions
	void loadEncoderDefinitions();
	// Return list of encoder definitions
	EncoderDefinition* encoders();


	/*
	 * Current Objects
	 */
	private:
	// Current object Bundle
	Bundle current_;

	public:
	// Return current object Bundle
	Bundle& current();
	// Sets the current active model for editing
	void setCurrentModel(Model* model);
	// Return current active model for editing
	Model* currentModel() const;
	// Return current active model for editing, accounting for trajectory frames
	Model* currentModelOrFrame() const;
	// Set current grid for editing
	void setCurrentGrid(Grid* grid);
	// Return current grid for editing
	bool currentGrid(Grid*& grid) const;


	/*
	 * Prefs
	 */
	private:
	// Dump element information to specified LineParser
	void dumpElementInfo(LineParser& parser);
	// Dump preferences values to specified LineParser
	void dumpPreferences(LineParser& parser);

	public:
	// Load user preferences file
	bool loadPrefs();
	// Save user preferences file
	bool savePrefs(QString fileName);


	/*
	 * Plugins
	 */
	private:
	// Plugin store
	PluginStore pluginStore_;
	// How many plugins had errors when loaded
	int nPluginsFailed_;
	// Filenames (including paths) of plugins that failed to load
	QStringList failedPlugins_;

	private:
	// Search specified directory for plugins
	int searchPluginsDir(QDir path);
	// Load specified plugin and register its functions
	bool loadPlugin(QString fileName);

	public:
	// Load plugins
	void loadPlugins();
	// Return plugin store reference
	const PluginStore& pluginStore();
};

ATEN_END_NAMESPACE

#endif
