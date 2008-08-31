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
PointerVariable::PointerVariable()
{
	dataType_ = VTypes::NoData;
	listType_ = VTypes::NoArray;
	data = NULL;
}

/*
// Set / Get
*/

// Set value of variable (char)
void PointerVariable::set(const char *s)
{
	printf("A pointer variable cannot be set from a character.\n");
}

// Set value of variable (int)
void PointerVariable::set(int i)
{
	printf("A pointer variable cannot be set from an integer.\n");
}

// Set value of variable (double)
void PointerVariable::set(double d)
{
	printf("A pointer variable cannot be set from a double.\n");
}

// Set value of variable (pointer)
void PointerVariable::set(void *ptr, VTypes::DataType type)
{
	if (type != dataType_) printf("A Pointer variable of type XXX"
	else data = ptr;
}

// Get value of variable as character string
const char *PointerVariable::asCharacter()
{
	printf("A pointer variable cannot be returned as a character.\n");
}

// Get value of variable as integer
int PointerVariable::asInteger()
{
	printf("A pointer variable cannot be returned as an integer.\n");
}

// Get value of variable as double
double PointerVariable::asDouble()
{
	printf("A pointer variable cannot be returned as a double.\n");
}

// Get value of variable as float
float PointerVariable::asFloat()
{
	printf("A pointer variable cannot be returned as a float.\n");
}

// Get value of variable as a boolean
bool PointerVariable::asBool()
{
	printf("A pointer variable cannot be returned as a boolean.\n");
}

// Get value of variable as pointer of specified type
void *PointerVariable::asPointer(VTypes::DataType type)
{
	if (type != dataType_) printf("Error  - a Pointer variable of type %s is being requested as a %s.\n",sdsd);
	return data;
}

// Integer increase
void PointerVariable::increase(int i)
{
	printf("More work needed here...\n");
}

// Integer decrease
void PointerVariable::decrease(int i)
{
	printf("More work needed here...\n");
}

