/*
	*** Variable Access Class
	*** src/variables/vaccess.cpp
	Copyright T. Youngs 2007-2009

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

#include "variables/vaccess.h"
#include "variables/accessstep.h"
#include "base/messenger.h"

// Constructor
VAccess::VAccess()
{
	// Clear default variables in VariableList
	accessors_.clear();
}

// Return address of VariableList
VariableList *VAccess::accessors()
{
	return &accessors_;
}

// Add new accessor
Variable *VAccess::addAccessor(const char *name, VTypes::DataType dt, bool readonly, int arraysize)
{
	msg.enter("VAccess::addAccessor");
	Variable *result = accessors_.addVariable(name, dt, arraysize);
	if (readonly) result->setReadOnly();
	msg.exit("VAccess::addAccessor");
	return result;
}

// Add new accessor
Variable *VAccess::addListAccessor(const char *name, VTypes::DataType dt)
{
	msg.enter("VAccess::addListAccessor");
	Variable *result = accessors_.addVariable(name, dt);
	result->setReadOnly();
	result->setListArray();
	msg.exit("VAccess::addListAccessor");
	return result;
}

// Return 'id' (position in list) of supplied accessor
int VAccess::accessorId(Variable *accessor)
{
	return accessors_.variableId(accessor);
}

// Check array index in supplied step against target member
bool VAccess::checkIndex(int &index, AccessStep *indexsource, Variable *member)
{
	msg.enter("VAccess::checkIndex");
	if (indexsource == NULL)
	{
		printf("NULL index source given to VAccess::checkIndex.\n");
		msg.exit("VAccess::checkIndex");
		return FALSE;
	}
	if (member == NULL)
	{
		printf("NULL member pointer given to VAccess::checkIndex.\n");
		msg.exit("VAccess::checkIndex");
		return FALSE;
	}
	if (indexsource->hasArrayIndex())
	{
		if (member->isArray())
		{
			// Get index and do simple lower-limit check
			index = indexsource->arrayIndex();
			if ((index < 1) || (index > (member->isListArray() ? index : member->arraySize()) ))
			{
				msg.print("Array index '%i' given to member '%s' is out of bounds (current valid limits are 1 to %i).\n", index, member->name(), member->arraySize());
				msg.exit("VAccess::checkIndex");
				return FALSE;
			}
		}
		else
		{
			msg.print("Array index given to member '%s', but this member is not an array.\n", member->name());
			msg.exit("VAccess::checkIndex");
			return FALSE;
		}
	}
	else
	{
		if (member->isArray())
		{
			printf("Array index required for member '%s'.\n", member->name());
			msg.exit("VAccess::checkIndex");
			return FALSE;
		}
	}
	msg.exit("VAccess::checkIndex");
	return TRUE;
}