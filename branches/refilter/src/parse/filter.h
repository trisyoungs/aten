/*
	*** File filter definition
	*** src/parse/filter.h
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

#include "command/commandlist.h"
#include "base/prefs.h"
#include <fstream>

// Filter Types
enum filter_type { FT_MODEL_IMPORT, FT_TRAJECTORY_IMPORT, FT_FIELD_IMPORT, FT_GRID_IMPORT, FT_MODEL_EXPORT,  FT_TRAJECTORY_EXPORT, FT_FIELD_EXPORT, FT_GRID_EXPORT, FT_NITEMS };
const char *text_from_FT(filter_type);
filter_type FT_from_text(const char*);

// Filter commands
enum filter_command { FC_NAME, FC_NICKNAME, FC_EXTENSION, FC_GLOB, FC_EXACT, FC_ZMAP, FC_ID, FC_NITEMS };
filter_command FC_from_text(const char*);
const char *text_from_FC(filter_command);

// Forward Declarations
class model;
class grid;
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
	// Filter ID
	int id;
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
	// Return the ID of the filter
	int get_id() { return id; }
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
	// Set the partner filter
	void set_partner(filter *f) { partner = f; }
	// Return the partner filter
	filter *get_partner() { return partner; }
	// Return the file filter
	const char *get_glob() { return glob.get(); }
	// Set the type of filter
	void set_type(filter_type ft);
	// Return the type of filter
	filter_type get_type() { return type; }
	// Return the long description of the filter (including glob)
	const char *get_description() { return description.get(); }

	/*
	// Command actions
	*/
	private:
	// Command list
	commandlist commands;
	// Reset all targets
	void reset_targets();

	public:
	// Execute filter
	bool execute(const char *filename, ifstream *sourcefile = NULL, bool trajheader = FALSE, model *altmodel = NULL);
};

#endif
