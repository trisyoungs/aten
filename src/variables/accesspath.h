/*
	*** Variable Access Path
	*** src/variables/accesspath.h
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

#ifndef ATEN_ACCESSPATH_H
#define ATEN_ACCESSPATH_H

#include "variables/accessstep.h"
#include "variables/variable.h"
#include "base/vtypes.h"
#include "base/parser.h"
#include "base/dnchar.h"
#include "templates/list.h"

// Forward declarations
class VariableList;

// Variable Access Path
class AccessPath : public Variable
{
	public:
	// Constructor
	AccessPath();
	// List pointers
	AccessPath *prev, *next;

	private:
	// Variable 'path'
	List<AccessStep> path_;
	// Eventual return result of path
	VTypes::DataType returnType_;
	// Original path text
	Dnchar originalPath_;
	// Walk path and get final target variable
	Variable *walk();

	public:
	// Set path from character constant
	bool setPath(const char *path, VariableList *sourcevars, Parser::ArgumentForm pathtype = Parser::UnknownForm);
	// Set single-node path from target variable
	void setPath(Variable *v);
	// Get return type of path
	VTypes::DataType returnType();
	// Return original path as text
	const char *originalPath();
	// Set value of variable (char)
	bool set(const char*, int index = -1);
	// Set value of variable (int)
	bool set(int i, int index = -1);
	// Set value of variable (double)
	bool set(double d, int index = -1);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, int index = -1);
	// Get value of variable as character string
	const char *asCharacter(int index = -1);
	// Get value of variable as integer
	int asInteger(int index = -1);
	// Get value of variable as double
	double asDouble(int index = -1);
	// Get value of variable as float
	float asFloat(int index = -1);
	// Get value of variable as a boolean
	bool asBool(int index = -1);
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type, int index = -1);
	// Step variable
	bool step(int delta, int index = -1);
};

#endif
