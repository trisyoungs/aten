/*
	*** Line / variable formatting
	*** src/parse/format.h
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

#ifndef ATEN_FORMAT_H
#define ATEN_FORMAT_H

#include "templates/list.h"
#include "classes/variables.h"

// Format node
class format_node
{
	public:
	// Constructor / Destructor
	format_node();
	~format_node();
	// List pointers
	format_node *next, *prev;

	/*
	// Format Node
	*/
	private:
	// Associated variable
	variable *v;
	// Field length (0 for unspecified)
	int length;
	// Field precision (0 for unspecified)
	int precision;

	public:
	// Set format node data
	bool set(const char *s, variable_list &vars);
	// Get format node variable
	variable *get_variable() { return v; }
	// Get field length
	int get_length() { return length; }
	// Get field precision
	int get_precision() { return precision; }
};

// Format
class format
{
	public:
	// Constructor / Destructor
	format();
	~format();

	/*
	// Node List
	*/
	private:
	// Head of format node list
	list<format_node> nodes;
	// Create a format assuming delimited formatting nodes
	bool create_delimited(const char *s, variable_list &vars);
	// Create a format not using delimited formatting nodes
	bool create_exact(const char *s, variable_list &vars);

	public:
	// Returns first node
	format_node* get_nodes() { return nodes.first(); }
	// Create format nodes from a supplied formatting string
	bool create(const char *s, variable_list &vars, bool delimited)
		{ return (delimited ? create_delimited(s, vars) : create_exact(s, vars)); }
	// Create a formatted string from the format data
	const char *create_string();
};

#endif
