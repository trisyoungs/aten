/*
	*** Master structure
	*** src/base/master.h
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

#ifndef ATEN_MASTER_H
#define ATEN_MASTER_H

#include "classes/bundle.h"
#include "classes/cell.h"
#include "parse/filter.h"
#include "command/commandlist.h"
#include "base/cli.h"
#include "templates/namemap.h"

// Forward Declarations
class Generator;
class Spacegroup;
class Model;
class Forcefield;
class Grid;
class Clipboard;

// Master
class Master
{
	public:
	// Constructor / Destructor
	Master();
	~Master();
	// Program mode enum
	enum ProgramMode { CommandMode, InteractiveMode, GuiMode, NoMode };
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
	// Load filter(s) from specified file
	bool loadFilter(const char *filename);
	// Set export partners for import filters
	void partnerFilters();
	// List of file filters 
	List<Filter> filters_[FT_NITEMS];

	public:
	// Load filters from specified location
	bool openFilters(const char* dir, bool isdatadir);
	// Probe file for its format
	Filter *probeFile(const char *filename, FilterType);
	// Find filter of specified type with nickname provided
	Filter *findFilter(FilterType ft, const char *nickname) const;
	// Return first filter in list (of a given type)
	Filter *filters(FilterType ft) const;

	/*
	// Forcefields
	*/
	private:
	// List of loaded forcefields
	List<Forcefield> forcefields_;
	// Default forcefield to use when no other has been applied
	Forcefield *defaultForcefield_;

	public:
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
	// Load the specified forcefield
	Forcefield *loadForcefield(const char*);
	// Find forcefield by name
	Forcefield *findForcefield(const char*) const;
	// Set the default forcefield
	void setDefaultForcefield(Forcefield *ff);
	// Get the current default forcefield
	Forcefield *defaultForcefield() const;

	/*
	// Volumetric Grid Data
	*/
	private:
	// Currently loaded grids
	List<Grid> grids_;

	public:
	// Return list of surfaces
	Grid *grids() const;
	// Return number of surfaces loaded
	int nGrids() const;
	// Return specified surface
	Grid *grid(int id);
	// Add new surface
	Grid *addGrid();
	// Remove surface
	void removeGrid(Grid *s);

	/*
	// Spacegroups
	*/
	public:
	// Spacegroup generators
	static Generator generators[];
	// Spacegroup definitions
	static Spacegroup spacegroups[];
	// Searches for the named spacegroup
	int findSpacegroupByName(const char *name) const;
	// Returns cell type of specified spacegroup id
	CellType spacegroupCellType(int sg) const;

	/*
	// Clipboard
	*/
	public:
	// User clipboard
	Clipboard *userClipboard;

	/*
	// Locations
	*/
	public:
	// Location of user's home directory
	Dnchar homeDir;
	// Current working directory
	Dnchar workDir;
	// Data directory
	Dnchar dataDir;

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
	void setProgramMode(ProgramMode pm) { programMode_ = pm; }
	// Return the current program mode
	ProgramMode programMode() { return programMode_; }
	// Cached scripts
	List<CommandList> scripts;
	// Cached commands
	List<CommandList> commands;
	// Script to store temporary typed commands
	CommandList tempScript;
	// Interactive mode command list
	CommandList interactiveScript;

	/*
	// Building
	*/
	private:
	// Selected drawing element
	int sketchElement_;

	public:
	// Set current drawing element
	void setSketchElement(int el) { sketchElement_ = el; }
	// Return current drawing element
	int sketchElement() { return sketchElement_; }

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
	// Prepare CLI debug options
	void debugCli(int, char**);
	// Parse command line options
	int parseCli(int, char**);
	// Element map name conversions to apply on load
	List< Namemap<int> > typeMap;
};

extern Master master;

#endif
