/*
	*** File filter definition
	*** src/file/filter.h
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

#ifndef H_FILTER_H
#define H_FILTER_H

#include "file/filtercommands.h"
#include "templates/command.h"

// Filter Types
enum filter_type { FT_MODEL_IMPORT, FT_MODEL_EXPORT, FT_TRAJECTORY_IMPORT, FT_TRAJECTORY_EXPORT, FT_FIELD_IMPORT, FT_FIELD_EXPORT, FT_SURFACE_IMPORT, FT_SURFACE_EXPORT, FT_NITEMS };
const char *text_from_FT(filter_type);
filter_type FT_from_text(const char*);

// Forward Declarations
class model;
class surface;
class pattern;
class unitcell;
class ffbound;

// Model file import/export filter
class filter
{
	public:
	// Constructor / Destructor
	filter();
	~filter();
	// List pointers
	filter *prev, *next;
	// Print information on filter
	void print();
	// Load filter commands from file
	bool load(ifstream&);

	/*
	// Properties
	*/
	private:
	// Type of data the filter describes
	filter_type type;
	// Long name of the filter
	dnchar name;
	// Nickname for the filter
	dnchar nickname;
	// File extension(s)
	dnchar extension;
	// File filter glob (for gui)
	dnchar glob;
	// Partner filter nickname
	dnchar partnernick;
	// Partner filter
	filter *partner;
	// Filter description
	dnchar description;
	// Filename alias list
	dnchar exactnames;
	// Whether the file has an associated extension
	bool has_extension;
	// Whether separate zmapping has been defined
	bool has_zmapping;
	// Type of element mapping to use
	zmap_type zmapping;

	public:
	// Return the descriptive name of the filter
	const char *get_name() { return name.get(); }
	// Return the short nickname of the filter
	const char *get_nickname() { return nickname.get(); }
	// Return the file extension
	const char *get_extension() { return extension.get(); }
	// Return the aliases list
	const char *get_exactnames() { return exactnames.get(); }
	// Return whether filter has an extension
	bool get_has_extension() { return has_extension; }
	// Return the partner nickname set in the filter
	const char *get_partnernick() { return partnernick.get(); }
	// Return whether partnernick has been set
	bool partnernick_set() { return (partnernick.empty() ? FALSE : TRUE); }
	// Set the partner filter
	void set_partner(filter *f) { partner = f; }
	// Return the partner filter
	filter *get_partner() { return partner; }
	// Return the file filter
	const char *get_glob() { return glob.get(); }
	// Set the type of filter
	void set_type(filter_type ft) { type = ft; }
	// Return the type of filter
	filter_type get_type() { return type; }
	// Return the long description of the filter (including glob)
	const char *get_description() { return description.get(); }

	/*
	// Command actions
	*/
	private:
	// Command list
	command_list<filter_command> commands;
	// Execute variable commands (mainly model-based)
	bool do_variables(command_node<filter_command>*&);
	// Execute read and write commands
	bool do_readwrite(command_node<filter_command>*&);
	// Execute model actions
	bool do_actions(command_node<filter_command>*&);
	// Execute surface-related actions
	bool do_surface(command_node<filter_command>*&);

	/*
	// Storage targets
	*/
	private:
	// Active model in filter
	model *activemodel;
	// Active cell in filter
	unitcell *activecell;
	// Active surface in filter
	surface *activesurface;
	// Set target to model
	void set_target(model*);

	/*
	// File	
	*/
	private:
	// Pointer to input file
	ifstream *inputfile;
	// Pointer to output file
	ofstream *outputfile;

	public:
	// Open input file
	bool set_input(const char *filename);
	// Set file pointer
	bool set_input(ifstream *file);
	// Set output file
	bool set_output(const char *filename);
	// Close input/output file(s)
	void close_files();
	// Import commands into the structure from file supplied
	bool open(const char *filterfile);
	// Import model file
	model *import_model(const char *modelfile);
	// Perform post-load actions on new models
	void finalise_model_import(const char *modelfile);
	// Export model to filename (in model)
	void export_model(model *source);
	// Export forcefield spec to file
	void export_field(model *source, const char *filename);
	// Open trajectory file
	bool read_trajectory(model *dest, bool isheader);
	// Import surface file
	void import_surface(const char *surfacefile);
};

#endif
