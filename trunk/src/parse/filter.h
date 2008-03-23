/*
	*** File filter definition
	*** src/parse/filter.h
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

#ifndef ATEN_FILTER_H
#define ATEN_FILTER_H

#include "command/commandlist.h"
#include "base/prefs.h"
#include <fstream>

// Filter Types
enum FilterType { FT_MODEL_IMPORT, FT_TRAJECTORY_IMPORT, FT_EXPRESSION_IMPORT, FT_GRID_IMPORT, FT_MODEL_EXPORT,  FT_TRAJECTORY_EXPORT, FT_EXPRESSION_EXPORT, FT_GRID_EXPORT, FT_NITEMS };
const char *text_from_FT(FilterType);
FilterType FT_from_text(const char*);

// Filter commands
enum FilterCommmand { FC_NAME, FC_NICKNAME, FC_EXTENSION, FC_GLOB, FC_EXACT, FC_ZMAP, FC_ID, FC_NITEMS };
FilterCommmand FC_from_text(const char*);
const char *text_from_FC(FilterCommmand);

// Forward Declarations
class Model;
class Grid;
class Pattern;
class Cell;
class ForcefieldBound;

// Model file import/export filter
class Filter
{
	public:
	// Constructor
	Filter();
	// List pointers
	Filter *prev, *next;
	// Print information on filter
	void print();
	// Load filter commands from file
	bool load(ifstream&);

	/*
	// Properties
	*/
	private:
	// Filter ID
	int id_;
	// Type of data the filter describes
	FilterType type_;
	// Long name of the filter
	Dnchar name_;
	// Nickname for the filter
	Dnchar nickname_;
	// File extension(s)
	Dnchar extension_;
	// File filter glob (for gui)
	Dnchar glob_;
	// Partner filter
	Filter *partner_;
	// Filter description
	Dnchar description_;
	// Filename alias list
	Dnchar exactNames_;
	// Whether the file has an associated extension
	bool hasExtension_;
	// Whether separate zmapping has been defined
	bool hasZmapping_;
	// Type of element mapping to use
	ZmapType zmapping_;

	public:
	// Return the ID of the filter
	int id();
	// Return the descriptive name of the filter
	const char *name();
	// Return the short nickname of the filter
	const char *nickname();
	// Return the file extension
	const char *extension();
	// Return the aliases list
	const char *exactNames();
	// Return whether filter has an extension
	bool hasExtension();
	// Set the partner filter
	void setPartner(Filter *f);
	// Return the partner filter
	Filter *partner();
	// Return the file filter
	const char *glob();
	// Set the type of filter
	void setType(FilterType ft);
	// Return the type of filter
	FilterType type();
	// Return the long description of the filter (including glob)
	const char *description();

	/*
	// Command actions
	*/
	private:
	// Command list
	CommandList commands_;
	// Reset all targets
	void resetTargets();

	public:
	// Execute filter
	bool execute(const char *filename, ifstream *trajfile = NULL, bool trajheader = FALSE, Model *altmodel = NULL);
};

#endif
