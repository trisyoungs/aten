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
#include "base/vtypes.h"
#include "base/parser.h"
#include "base/dnchar.h"
#include "templates/list.h"

// Forward declarations
class VariableList;
class Variable;

// Variable Access Path
class AccessPath
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
	bool setPath(const char *path, VariableList *sourcevars, Parser::ArgumentForm pathtype);
	// Set single-node path from target variable
	void setPath(Variable *v);
	// Get return type of path
	VTypes::DataType returnType();
	// Return original path as text
	const char *originalPath();
	// Get return value as integer
	int asInteger();
	// Get return value as double
	double asDouble();
	// Get return value as float
	float asFloat();
	// Get return value as character
	const char *asCharacter();
	// Get return value as bool
	bool asBool();
	// Get return value as pointer
	void *asPointer(VTypes::DataType dt);
	// Increase variable by integer amount
	bool increase(int i);
	// Decrease variable by integer amount
	bool decrease(int i);
	// Set variable target from integer
	bool set(int i);
	// Set variable target from double
	bool set(double d);
	// Set variable target from character
	bool set(const char *s);
	// Set variable target from pointer
	bool set(void *ptr, VTypes::DataType dt); 
};

#endif
