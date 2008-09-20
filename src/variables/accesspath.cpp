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
	resultVariable_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
AccessPath::~AccessPath()
{
	if (resultVariable_ != NULL) delete resultVariable_;
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
				if (!modelAccessors.retrieve(result.asPointer(), step->variableId(), result)) failed = TRUE;
				break;
			case (VTypes::AtomData):
				break;
		}
		if (failed) break;
		// Prepare for next step
		lastType = step->returnType();
	}
	// Put value now stored in the ReturnValue structure in the local Variable
	switch (dataType_)
	{
		case (VTypes::IntegerData):
			resultVariable_->set(result.value()->asInteger());
			break;
		case (VTypes::RealData):
			resultVariable_->set(result.value()->asDouble());
			break;
		case (VTypes::CharacterData):
			resultVariable_->set(result.value()->asCharacter());
			break;
		default:
			resultVariable_->set(result.value()->asPointer(dataType_), dataType_);
			break;
	}
	msg.exit("AccessPath::walk");
	return resultVariable_;
}

// Walk path and set final target variable
bool AccessPath::walkAndSet(Variable *srcvar, VTypes::DataType dt)
{
	msg.enter("AccessPath::walkAndSet");
	AccessStep *step = NULL;
	int arrayindex;
	static ReturnValue result;
	result.reset();
	bool success = TRUE;
	// DataType of the most recently 'got' value
	VTypes::DataType lastType = VTypes::NoData;
	// Go through remaining nodes in the list one by one, calling the relevant static member functions in access-enabled objects
	for (step = path_.first(); step != NULL; step = step->next)
	{
		// If a previous ptrType was set, use this to determine the accessor set to search. Otherwise, get the value stored in the variable.
		switch (lastType)
		{
			case (VTypes::NoData):
				step->setTargetValue(srcvar);
				break;
			case (VTypes::IntegerData):
			case (VTypes::RealData):
			case (VTypes::CharacterData):
			case (VTypes::ExpressionData):
				msg.print("AccessPath '%s' is trying to access a subvariable of a non-class type (%s).\n", name_.get(), step->targetName());
				success = FALSE;
				result.reset();
				break;
			// For pointer types, get/set return value from static VAccess classes
			case (VTypes::ModelData):
				// If this is not the last step, retrieve. Otherwise, set.
				if (step->next == NULL) 
				{
					if (!modelAccessors.set(result.asPointer(), step->variableId(), srcvar)) success = FALSE;
				}
				else
				{
					if (!modelAccessors.retrieve(result.asPointer(), step->variableId(), result)) success = FALSE;
				}
				break;
			case (VTypes::AtomData):
				break;
		}
		if (!success) break;
		// Prepare for next step
		lastType = step->returnType();
	}
	msg.exit("AccessPath::walkAndSet");
	return success;
}

// Set (create) access path from text path
bool AccessPath::setPath(const char *path)
{
	msg.enter("AccessPath::setPath");
	static char opath[512];
	Dnchar bit;
	AccessStep *step;
	int n;
	char *c;
	VTypes::DataType lastType = VTypes::NoData;
	bool success;
	// Make sure the parent variable list has been set...
	if (parent_ == NULL)
	{
		printf("Internal error - parent VariableList has not been set in AccessPath.\n");
		msg.exit("AccessPath::setPath");
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
			msg.exit("AccessPath::setPath");
			return FALSE;
		}
		// If this is the first added node then the variable must exist in the local VariableList.
		step = path_.add();
		// Otherwise, the DataType set in 'lastType' determines which structure's VariableList to use
		switch (lastType)
		{
			case (VTypes::NoData):
				success = step->setTarget(bit.get(), parent_, parent_);
				break;
			case (VTypes::ModelData):
				success = step->setTarget(bit.get(), parent_, modelAccessors.accessors());
				if (success) step->setVariableId(modelAccessors.accessorId(step->target()));
				break;
		}
		if (!success) break;
		// Increase the char pointer
		for (n=0; n<bit.length(); n++) c ++;
		// If we're on a '.', skip on a further character
		if (*c == '.') c++;
		// Store lasttype
		lastType = step->returnType();
	}
	// Set the return type of the path as the type of the last step, and create a suitable return variable
	if (success)
	{
		dataType_ = step->returnType();
		switch (dataType_)
		{
			case (VTypes::IntegerData):
				resultVariable_ = new IntegerVariable;
				break;
			case (VTypes::RealData):
				resultVariable_ = new RealVariable;
				break;
			case (VTypes::CharacterData):
				resultVariable_ = new CharacterVariable;
				break;
			default:
				resultVariable_ = new PointerVariable(dataType_);
				break;
		}
	}
	msg.exit("AccessPath::setPath");
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
	printf("Oh no. Don't think so!\n");
	return FALSE;
}

// Set variable target from integer
bool AccessPath::set(int i, int index)
{
	static IntegerVariable ivar;
	ivar.set(i);
	return walkAndSet(&ivar, VTypes::IntegerData);
}

// Set variable target from double
bool AccessPath::set(double d, int index)
{
	static RealVariable dvar;
	dvar.set(d);
	return walkAndSet(&dvar, VTypes::RealData);
}

// Set variable target from character
bool AccessPath::set(const char *s, int index)
{
	static CharacterVariable cvar;
	cvar.set(s);
	return walkAndSet(&cvar, VTypes::CharacterData);
}

// Set variable target from pointer
bool AccessPath::set(void *ptr, VTypes::DataType dt, int index)
{
	static PointerVariable pvar;
	pvar.reset(ptr, dt);
	return walkAndSet(&pvar, dt);
}
