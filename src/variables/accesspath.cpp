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
#include "variables/atomaccess.h"
#include "variables/cellaccess.h"
#include "variables/modelaccess.h"
#include "variables/prefsaccess.h"
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

// Walk path to retrieve/set/step end variable
bool AccessPath::walk(ReturnValue &rv, Variable *srcvar, VTypes::DataType dt, int delta)
{
	msg.enter("AccessPath::walk");
	AccessStep *step = NULL;
	int arrayindex;
	VAccess *accesslist;
	rv.reset();
	bool result = TRUE;
	// DataType of the most recently 'got' value
	VTypes::DataType lastType = VTypes::NoData;
	// Go through nodes in the list one by one, calling the relevant static member functions in access-enabled objects
	for (step = path_.first(); step != NULL; step = step->next)
	{
		// If a previous ptrType was set, use this to determine the accessor set to search. Otherwise, get the value stored in the variable.
//  		printf("Current step = %s\n", step->target()->name());

		// If no previous data set (i.e. this is the first step) don't use an access list
		if (lastType == VTypes::NoData)
		{
			if (step->next == NULL)
			{
				if (srcvar != NULL) step->setTargetVariable(srcvar);
				else if (delta != 0) step->stepTargetVariable(delta);
				else rv.set(step);
			}
			else rv.set(step);
		}
		else if (VTypes::isPointer(lastType))
		{
			// Get pointer to accesslist
			switch (lastType)
			{
				case (VTypes::ModelData):
					accesslist = &modelAccessors;
					break;
				case (VTypes::CellData):
					accesslist = &cellAccessors;
					break;
				case (VTypes::AtomData):
					accesslist = &atomAccessors;
					break;
				default:
					printf("Subvariable setting within pointers of type '%s' is not implemented.\n", VTypes::dataType(lastType));
					accesslist = NULL;
					break;
			}
			if (accesslist == NULL)
			{
				result = FALSE;
				break;
			}
			// If this is not the last step, retrieve. Otherwise, set or step.
			if (step->next == NULL) 
			{
				if (srcvar != NULL) result = accesslist->set(rv.asPointer(), step, srcvar);
				else if (delta != 0)
				{
					msg.print("Subvariables of pointer classes cannot be stepped.\n");
					result = FALSE;
				}
				else result = accesslist->retrieve(rv.asPointer(), step, rv);
			}
			else result = accesslist->retrieve(rv.asPointer(), step, rv);
		}
		else
		{
			msg.print("AccessPath '%s' is trying to access a subvariable of a non-class type (%s).\n", name_.get(), step->targetName());
			result = FALSE;
			rv.reset();
			break;
		}
		if (!result) break;
		// Prepare for next step
		lastType = step->type();
	}
	msg.exit("AccessPath::walk");
	return result;
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
			case (VTypes::CellData):
				success = step->setTarget(bit.get(), parent_, cellAccessors.accessors());
				if (success) step->setVariableId(cellAccessors.accessorId(step->target()));
				break;
			case (VTypes::AtomData):
				success = step->setTarget(bit.get(), parent_, atomAccessors.accessors());
				if (success) step->setVariableId(atomAccessors.accessorId(step->target()));
				break;
			case (VTypes::PrefsData):
				success = step->setTarget(bit.get(), parent_, prefsAccessors.accessors());
				if (success) step->setVariableId(prefsAccessors.accessorId(step->target()));
				break;
			default:
				printf("This variable type (%s) has not been implemented in AccessPath::setPath.\n", VTypes::dataType(lastType));
				success = FALSE;
				break;
		}
		if (!success)
		{
			msg.print("Unable to resolve path '%s'.\n", path);
			break;
		}
		// Increase the char pointer
		for (n=0; n<bit.length(); n++) c ++;
		// If we're on a '.', skip on a further character
		if (*c == '.') c++;
		// Store lasttype
		lastType = step->type();
	}
	// Set the return type of the path as the type of the last step, and create a suitable return variable
	if (success)
	{
		dataType_ = step->type();
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
int AccessPath::asInteger(Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, 0))
	{
		if (!resultVariable_->set(rv.value()->asInteger())) return 0;
		return resultVariable_->asInteger();
	}
	else return 0;
}

// Get return value as double
double AccessPath::asDouble(Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, 0))
	{
		if (!resultVariable_->set(rv.value()->asDouble())) return 0.0;
		return resultVariable_->asDouble();
	}
	else return 0.0;
}

// Get return value as character
const char *AccessPath::asCharacter(Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, 0))
	{
		if (!resultVariable_->set(rv.value()->asCharacter())) return "NULL";
		return resultVariable_->asCharacter();
	}
	else return "NULL";
}

// Get return value as bool
bool AccessPath::asBool(Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, 0))
	{
		if (!resultVariable_->set(rv.value()->asCharacter())) return FALSE;
		return resultVariable_->asBool();
	}
	else return FALSE;
}

// Get return value as pointer
void *AccessPath::asPointer(VTypes::DataType dt, Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, 0))
	{
		if (!resultVariable_->set(rv.value()->asPointer(dt), dt)) return NULL;
		return resultVariable_->asPointer(dt);
	}
	else return NULL;
}

// Increase variable by integer amount
bool AccessPath::step(int delta, Variable *index)
{
	ReturnValue rv;
	if (walk(rv, NULL, VTypes::NoData, delta)) return TRUE;
	else return FALSE;
}

// Set variable target from integer
bool AccessPath::set(int i, Variable *index)
{
	static IntegerVariable ivar;
	ivar.set(i);
	ReturnValue rv;
	return walk(rv, &ivar, VTypes::IntegerData, 0);
}

// Set variable target from double
bool AccessPath::set(double d, Variable *index)
{
	static RealVariable dvar;
	dvar.set(d);
	ReturnValue rv;
	return walk(rv, &dvar, VTypes::RealData, 0);
}

// Set variable target from character
bool AccessPath::set(const char *s, Variable *index)
{
	static CharacterVariable cvar;
	cvar.set(s);
	ReturnValue rv;
	return walk(rv, &cvar, VTypes::CharacterData, 0);
}

// Set variable target from pointer
bool AccessPath::set(void *ptr, VTypes::DataType dt, Variable *index)
{
	static PointerVariable pvar;
	pvar.reset(ptr, dt);
	ReturnValue rv;
	return walk(rv, &pvar, dt, 0);
}
