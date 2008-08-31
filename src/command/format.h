/*
	*** Line / variable formatter
	*** src/command/format.h
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
#include "command/variablelist.h"
#include "command/formatnode.h"
#include "base/parser.h"

// Format
class Format : public Parser
{
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

	/*
	// File parsing (using global parser object)
	*/
	private:
	// Gets all arguments from string by format
	void getAllArgsFormatted();

	public:
	// Parse file with format
	int getArgsFormatted(ifstream*, int);
	// Parse file with format
	void getArgsFormatted(const char*, int);
};

#endif
