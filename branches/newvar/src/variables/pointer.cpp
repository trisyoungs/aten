/*
	*** Pointer Variable
	*** src/variables/pointer.cpp
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

#include "variables/pointer.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
PointerVariable::PointerVariable(VTypes::DataType ptrtype)
{
	dataType_ = ptrtype;
	ptrData_ = NULL;
	ptrArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set value of variable (pointer)
bool PointerVariable::set(void *ptr, VTypes::DataType type, int index)
{
	if (type != dataType_)
	{
		msg.print("A Pointer variable of type '%s' (%s) cannot be set from a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
		return FALSE;
	}
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	bool outofbounds = FALSE;
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		ptrData_ = ptr;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
		else ptrArrayData_[index-1] = ptr;
	}
	return TRUE;
}

// Get value of variable as pointer of specified type
void *PointerVariable::asPointer(VTypes::DataType type, int index)
{
	if (type != dataType_) printf("Error - a Pointer variable of type '%s' (%s) is being requested as a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
	void *result = NULL;
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		result = ptrData_;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
		else result = ptrArrayData_[index-1];
	}
	return result;
}

// Step variable
bool PointerVariable::step(int i, int index)
{
	printf("More work needed here...\n");
	return FALSE;
}
