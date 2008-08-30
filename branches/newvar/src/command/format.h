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
#include "parse/variablelist.h"

// Format node
class FormatNode
{
	public:
	// Constructor
	FormatNode();
	// List pointers
	FormatNode *next, *prev;

	/*
	// Format Node
	*/
	private:
	// Associated variable
	Variable *variable_;
	// Field length (0 for unspecified)
	int length_;
	// Field precision (0 for unspecified)
	int precision_;
	// Whether to pad integers with zeros
	bool zeroPadInteger_;

	public:
	// Set format node data
	bool set(const char *s, VariableList &vars);
	// Get format node variable
	Variable *variable();
	// Get field length
	int length();
	// Get field precision
	int precision();
	// Whether to zero-pad integers
	bool zeroPadInteger();
};

// Format
class Format
{
	public:
	// Destructor
	~Format();

	/*
	// Node List
	*/
	private:
	// Head of format node list
	List<FormatNode> nodes_;
	// Create a format assuming delimited formatting nodes
	bool createDelimited(const char *s, VariableList &vars);
	// Create a format not using delimited formatting nodes
	bool createExact(const char *s, VariableList &vars);

	public:
	// Returns first node
	FormatNode* nodes();
	// Create format nodes from a supplied formatting string
	bool create(const char *s, VariableList &vars, bool delimited);
	// Create a formatted string from the format data
	const char *createString();
};

#endif
