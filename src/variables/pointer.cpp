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
#include <stdio.h>
#include <stdlib.h>

// Constructor
PointerVariable::PointerVariable(VTypes::DataType ptrtype)
{
	dataType_ = ptrtype;
	arrayType_ = VTypes::NoArray;
	ptrData_ = NULL;
	arrayPtrData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool PointerVariable::set(const char *s, int index)
{
	msg.print("A Pointer variable cannot be set from a character.\n");
	return FALSE;
}

// Set value of variable (int)
bool PointerVariable::set(int i, int index)
{
	msg.print("A Pointer variable cannot be set from an integer.\n");
	return FALSE;
}

// Set value of variable (double)
bool PointerVariable::set(double d, int index)
{
	msg.print("A Pointer variable cannot be set from a double.\n");
	return FALSE;
}

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
		if (arrayType_ != VTypes::NoArray)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		ptrData_ = ptr;
	}
	else
	{
		if (arrayType_ == VTypes::NoArray)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		// Get array and set value...
		if (arrayType_ == VTypes::NormalArray)
		{
			if (index > arraySize_) outofbounds = TRUE;
			else arrayPtrData_[index-1] = ptr;
		}
		else msg.print("This array element cannot be set.\n");
		if (outofbounds)
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
	}
	return TRUE;
}

// Get value of variable as character string
const char *PointerVariable::asCharacter(int index)
{
	printf("A Pointer variable cannot be returned as a character.\n");
	return "NULL";
}

// Get value of variable as integer
int PointerVariable::asInteger(int index)
{
	printf("A Pointer variable cannot be returned as an integer.\n");
	return 0;
}

// Get value of variable as double
double PointerVariable::asDouble(int index)
{
	printf("A Pointer variable cannot be returned as a double.\n");
	return 0.0;
}

// Get value of variable as float
float PointerVariable::asFloat(int index)
{
	printf("A Pointer variable cannot be returned as a float.\n");
	return 0.0f;
}

// Get value of variable as a boolean
bool PointerVariable::asBool(int index)
{
	printf("A Pointer variable cannot be returned as a boolean.\n");
	return FALSE;
}

// Get value of variable as pointer of specified type
void *PointerVariable::asPointer(VTypes::DataType type, int index)
{
	if (type != dataType_) printf("Error - a Pointer variable of type '%s' (%s) is being requested as a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
	void *result = NULL;
	bool outofbounds = FALSE;
	// Check array index given
	if (index == -1)
	{
		if (arrayType_ != VTypes::NoArray)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		ptrData_ = ptr;
	}
	else
	{
		if (arrayType_ == VTypes::NoArray)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		// Get array and value...
		switch (arrayType_)
		{
			case (VTypes::NormalArray):
				if (index > arraySize_) outofbounds = TRUE;
				else result = arrayPtrData_[index-1];
				break;
			case (VTypes::ListArray):
				// Cast pointer into a List and retrieve value
				switch (dataType_)
				{
					case (VTypes::ModelData):
						if (index > ((List<Model>*) ptrData_).nItems()) outofbounds = TRUE;
						else result = ((List<Model>*) ptrData_)[index-1];
						break;
					case (VTypes::AtomData):
						if (index > ((List<Atom>*) ptrData_).nItems()) outofbounds = TRUE;
						else result = ((List<Atom>*) ptrData_)[index-1];
						break;
					default:
						printf("NOT DONE YET!\n");
						break;
				}
		}
		if (outofbounds) msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
	}
	return result;
}

// Integer increase
bool PointerVariable::increase(int i, int index)
{
	printf("More work needed here...\n");
	return FALSE;
}

// Integer decrease
bool PointerVariable::decrease(int i, int index)
{
	printf("More work needed here...\n");
	return FALSE;
}

