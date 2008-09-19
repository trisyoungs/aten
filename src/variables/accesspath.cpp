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
#include "variables/returnvalue.h"
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
	AccessStep *step = NULL;
	int arrayindex;
	static ReturnValue result;
	result.reset();
	bool failed = FALSE;
	// DataType of the most recently 'got' value
	VTypes::DataType lastType = VTypes::NoData;
	// Go through remaining nodes in the list one by one, calling the relevant static member functions in access-enabled objects
	for (step = path_.first(); step != NULL; step = step->next)
	{
		// If a previous ptrType was set, use this to determine the accessor set to search. Otherwise, get the value stored in the variable.
		switch (lastType)
		{
			case (VTypes::NoData):
				result.set(step);
				break;
			case (VTypes::IntegerData):
			case (VTypes::RealData):
			case (VTypes::CharacterData):
			case (VTypes::ExpressionData):
				msg.print("AccessPath '%s' is trying to access a subvariable of a non-class type (%s).\n", name_.get(), step->targetName());
				failed = TRUE;
				result.reset();
				break;
			// For pointer types, get return value from static VAccess classes
			case (VTypes::ModelData):
				if (!modelAccessors.findAccessor(result.asPointer(), step->target(), result)) failed = TRUE;
				break;
			case (VTypes::AtomData):
				break;
		}
		// Prepare for next step
		lastType = step->returnType();
	}
	msg.exit("AccessPath::walk");
	return result.value();
}

// Set (create) access path from text path
bool AccessPath::setPath(const char *path)
{
	msg.enter("AccessPath::set");
	static char opath[512];
	Dnchar bit;
	AccessStep *step;
	int n;
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
	while (*c != '\0')
	{
		// Get section of path existing before the next '.'
		bit = beforeChar(c, '.');
		// Check for an empty string bit - caused by '..'
		if (bit.empty())
		{
			msg.print("Empty section found in variable path.\n");
			msg.exit("AccessPath::set");
			return FALSE;
		}
		// If this is the first added node then the variable must exist in the local VariableList.
		// Otherwise, the DataType set in 'lastType' determines which structure's VariableList to us5e
		switch (lastType)
		{
			case (VTypes::NoData):
				pathvars = parent_;
				break;
			case (VTypes::ModelData):
				pathvars = modelAccessors.accessors();
				break;
		}
		printf("Last variable type was '%s'.\n", VTypes::dataType(lastType));
		// Add the new path step
		step = path_.add();
		success = step->setTarget(bit.get(), parent_, pathvars);
		if (!success) break;
		// Increase the char pointer
		for (n=0; n<bit.length(); n++) c ++;
		// If we're on a '.', skip on a further character
		if (*c == '.') c++;
		// Store lasttype
		lastType = step->returnType();
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
