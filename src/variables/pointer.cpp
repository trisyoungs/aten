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
	data = NULL;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool PointerVariable::set(const char *s)
{
	printf("A Pointer variable cannot be set from a character.\n");
	return FALSE;
}

// Set value of variable (int)
bool PointerVariable::set(int i)
{
	printf("A Pointer variable cannot be set from an integer.\n");
	return FALSE;
}

// Set value of variable (double)
bool PointerVariable::set(double d)
{
	printf("A Pointer variable cannot be set from a double.\n");
	return FALSE;
}

// Set value of variable (pointer)
bool PointerVariable::set(void *ptr, VTypes::DataType type)
{
	if (type != dataType_)
	{
		printf("A Pointer variable of type '%s' (%s) cannot be set from a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
		return FALSE;
	}
	else data = ptr;
	return TRUE;
}

// Get value of variable as character string
const char *PointerVariable::asCharacter()
{
	printf("A Pointer variable cannot be returned as a character.\n");
	return "NULL";
}

// Get value of variable as integer
int PointerVariable::asInteger()
{
	printf("A Pointer variable cannot be returned as an integer.\n");
	return 0;
}

// Get value of variable as double
double PointerVariable::asDouble()
{
	printf("A Pointer variable cannot be returned as a double.\n");
	return 0.0;
}

// Get value of variable as float
float PointerVariable::asFloat()
{
	printf("A Pointer variable cannot be returned as a float.\n");
	return 0.0f;
}

// Get value of variable as a boolean
bool PointerVariable::asBool()
{
	printf("A Pointer variable cannot be returned as a boolean.\n");
	return FALSE;
}

// Get value of variable as pointer of specified type
void *PointerVariable::asPointer(VTypes::DataType type)
{
	if (type != dataType_) printf("Error - a Pointer variable of type '%s' (%s) is being requested as a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
	return data;
}

// Integer increase
bool PointerVariable::increase(int i)
{
	printf("More work needed here...\n");
	return FALSE;
}

// Integer decrease
bool PointerVariable::decrease(int i)
{
	printf("More work needed here...\n");
	return FALSE;
}

