/*
	*** Aten's master structure
	*** src/base/aten.h
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

#ifndef ATEN_ATEN_H
#define ATEN_ATEN_H

#include "classes/bundle.h"
#include "classes/cell.h"
#include "parse/filter.h"
#include "command/commandlist.h"
#include "base/cli.h"
#include "templates/namemap.h"

#define ATENVERSION "0.99"
#define ATENREVISION "577"
#define ATENDATE "Tue 26 Aug - 16:01"
#define ATENURL "http://aten.googlecode.com/svn/trunk"

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
	enum ProgramMode { CommandMode, InteractiveMode, GuiMode, BatchExportMode, NoMode };
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

	public:
	// Sets the current active model for editing
	void setCurrentModel(Model*);
	// Return current active model for editing
	Model *currentModel() const;
	// Return first item in the model list
	Model *models() const;
	// Return nth item in the model list
	Model *model(int n);
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
	// Import / Export
	*/
	private:
	// Parse filter index and load filters
	bool parseFilterIndex(const char *path, ifstream *indexfile);
	// Load filter(s) from specified file
	bool loadFilter(const char *filename);
	// Set export partners for import filters
	void partnerFilters();
	// List of file filters 
	List<Filter> filters_[Filter::nFilterTypes];

	public:
	// Load filters
	bool openFilters();
	// Reload filters
	bool reloadFilters();
	// Probe file for its format
	Filter *probeFile(const char *filename, Filter::FilterType);
	// Find filter of specified type with nickname provided
	Filter *findFilter(Filter::FilterType ft, const char *nickname) const;
	// Return first filter in list (of a given type)
	Filter *filters(Filter::FilterType ft) const;


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
	Forcefield *addForcefield();
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

	public:
	// Set location of users's home directory
	void setHomeDir(const char *path);
	// Return the current home directory location
	const char *homeDir();
	// Set working directory
	void setWorkDir(const char *path);
	// Return the current working directory
	const char *workDir();
	// Set data directory
	void setDataDir(const char *path);
	// Return the data directory path
	const char *dataDir();


	/*
	// Program Modes
	*/
	private:
	// Initialise command function pointers
	void initCommands();
	// Current mode of program operation
	ProgramMode programMode_;

	public:
	// Sets the current program mode
	void setProgramMode(ProgramMode pm);
	// Return the current program mode
	ProgramMode programMode();
	// Cached scripts
	List<CommandList> scripts;
	// Script to store temporary typed commands
	CommandList tempScript;
	// Interactive mode command list
	CommandList interactiveScript;


	/*
	// Building
	*/
	private:
	// Selected drawing element
	short int sketchElement_;

	public:
	// Set current drawing element
	void setSketchElement(short int el);
	// Return current drawing element
	short int sketchElement();


	/*
	// Progress Indicators
	*/
	public:
	// Initialise a progress indicator
	void initialiseProgress(const char *jobtitle, int totalsteps);
	// Update the number of steps (returns if the dialog was canceled)
	bool updateProgress(int currentstep);
	// Terminate the current progress
	void cancelProgress();


	/*
	// CLI
	*/
	private:
	// Print usage information
	void printUsage() const;

	public:
	// Parse early command line options, before filter / prefs load
	bool parseCliEarly(int, char**);
	// Parse command line options (after filter / prefs load
	int parseCli(int, char**);
	// Element map name conversions to apply on load
	List< Namemap<int> > typeMap;


	/*
	// Single-shot program modes
	*/
	private:
	// Model format in which to export models
	Filter *exportFilter_;

	public:
	// Set format to use in export
	void setExportFilter(Filter *f);
	// Export all currently loaded models in the referenced format
	void exportModels();
};

extern Aten aten;

#endif
