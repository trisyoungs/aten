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
#include "variables/integer.h"
#include "variables/character.h"
#include "variables/real.h"
#include "variables/pointer.h"
#include "base/vtypes.h"
#include "base/parser.h"
#include "base/dnchar.h"
#include "templates/list.h"

// Forward declarations
class VariableList;
class ReturnValue;

// Variable Access Path
class AccessPath : public Variable
{
	public:
	// Constructor / Destructor
	AccessPath();
	~AccessPath();
	// List pointers
	AccessPath *prev, *next;

	private:
	// Variable 'path'
	List<AccessStep> path_;
	// Walk path and get/set/step final target variable
	bool walk(ReturnValue &rv, Variable *srcvar, VTypes::DataType dt, int step);
	// Variable to contain return value
	Variable *resultVariable_;

	public:
	// Set path from character constant
	bool setPath(const char *path, bool isArrayIndex = FALSE);
	// Set value of variable (char)
	bool set(const char*, Variable *index = NULL);
	// Set value of variable (int)
	bool set(int i, Variable *index = NULL);
	// Set value of variable (double)
	bool set(double d, Variable *index = NULL);
	// Set value of variable (pointer)
	bool set(void *ptr, VTypes::DataType type, Variable *index = NULL);
	// Get value of variable as character string
	const char *asCharacter(Variable *index = NULL);
	// Get value of variable as integer
	int asInteger(Variable *index = NULL);
	// Get value of variable as double
	double asDouble(Variable *index = NULL);
	// Get value of variable as float
	float asFloat(Variable *index = NULL);
	// Get value of variable as a boolean
	bool asBool(Variable *index = NULL);
	// Get value of variable as pointer of specified type
	void *asPointer(VTypes::DataType type, Variable *index = NULL);
	// Step variable
	bool step(int delta, Variable *index = NULL);
};

#endif
