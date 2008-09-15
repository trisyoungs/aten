/*
	*** Variable Access Path
	*** src/variables/accesspath.cpp
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

#include "variables/accesspath.h"
#include "variables/accessstep.h"
#include "variables/modelaccess.h"
#include "variables/variablelist.h"
#include "base/messenger.h"
#include "base/sysfunc.h"

// Constructor
AccessPath::AccessPath()
{
	// Private variables
	returnType_ = VTypes::NoData;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Walk path to retrieve end variable
Variable *AccessPath::walk()
{
	msg.enter("AccessPath::walk");
	Variable *result = NULL;
	AccessStep *step = NULL;
	// DataType of the most recently stored pointer, and the pointer itself
	VTypes::DataType ptrType = VTypes::NoData;
	void *ptr = NULL;
	// Get first node in path, its type, and its value
	step = path_.first();
	// Go through remaining nodes in the list one by one, calling the relevant static member functions in access-enabled objects
	for (step = path_.first(); step != NULL; step = step->next)
	{
		// If a previous ptrType was set, use this to determine the accessor set to search.
		// Otherwise, store the return result / 
// 		if (ptrType == VTypes::NoData
		// Check return type of step
		

		// Prepare for next step
		ptrType = step->returnType();
	}
	msg.exit("AccessPath::walk");
}

// Set (create) access path from text path
bool AccessPath::setPath(const char *path)
{
	msg.enter("AccessPath::set");
	static char opath[512];
	Dnchar bit;
	AccessStep *step;
	char *c;
	VTypes::DataType lastType = VTypes::NoData;
	VariableList *pathvars;
	bool success;
	// Make sure the parent variable list has been set...
	if (parent_ == NULL)
	{
		printf("Internal error - parent VariableList has not been set in AccessPath.\n");
		msg.exit("AccessPath::set");
		return FALSE;
	}
	// Store original path
	name_ = path;
	// Take a copy of the original path to work on
	strcpy(opath, path);
	c = opath;
	while (c != '\0')
	{
		// Get section of path existing before the next '.'
		bit = beforeChar(opath, '.');
		// If this is the first added node then the variable must exist in the local VariableList.
		// Otherwise, the DataType set in 'lastType' determines which structure's VariableList to use
		switch (lastType)
		{
			case (VTypes::NoData):
				pathvars = parent_;
				break;
			case (VTypes::ModelData):
				pathvars = modelAccessors.accessors();
				break;
		}
		// Add the new path step
		step = path_.add();
		success = step->setTarget(bit.get(), parent_, pathvars);
	}
	msg.exit("AccessPath::set");
	return success;
}

// Get return value as integer
int AccessPath::asInteger(int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0;
	return v->asInteger();
}

// Get return value as double
double AccessPath::asDouble(int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0.0;
	return v->asDouble();
}

// Get return value as float
float AccessPath::asFloat(int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return 0.0f;
	return v->asFloat();
}

// Get return value as character
const char *AccessPath::asCharacter(int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return "NULL";
	return v->asCharacter();
}

// Get return value as bool
bool AccessPath::asBool(int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->asBool();
}

// Get return value as pointer
void *AccessPath::asPointer(VTypes::DataType dt, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return NULL;
	return v->asPointer(dt);
}

// Increase variable by integer amount
bool AccessPath::step(int delta, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->step(delta);
}

// Set variable target from integer
bool AccessPath::set(int i, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(i);
}

// Set variable target from double
bool AccessPath::set(double d, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(d);
}

// Set variable target from character
bool AccessPath::set(const char *s, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(s);
}

// Set variable target from pointer
bool AccessPath::set(void *ptr, VTypes::DataType dt, int index)
{
	// Retrieve the target variable
	Variable *v = walk();
	if (v == NULL) return FALSE;
	return v->set(ptr, dt);
}
