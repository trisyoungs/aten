/*
	*** Reference Variable
	*** src/variables/reference.cpp
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

#include "variables/reference.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
ReferenceVariable::ReferenceVariable()
{
	dataType_ = VTypes::ReferenceData;
	listType_ = VTypes::NoArray;
	data = NULL;
}

/*
// Set / Get
*/

// Set value of variable (char)
void ReferenceVariable::set(const char *s)
{
	printf("A Reference variable cannot be set from a character.\n");
}

// Set value of variable (int)
void ReferenceVariable::set(int i)
{
	printf("A Reference variable cannot be set from an integer.\n");
}

// Set value of variable (double)
void ReferenceVariable::set(double d)
{
	printf("A Reference variable cannot be set from a double.\n");
}

// Set value of variable (pointer)
void ReferenceVariable::set(void *ptr, VTypes::DataType type)
{
	printf("A Reference variable cannot be set from a double.\n");
}

// Get value of variable as character string
const char *ReferenceVariable::asCharacter()
{
	printf("A Reference variable cannot be returned as a character.\n");
	return "NULL";
}

// Get value of variable as integer
int ReferenceVariable::asInteger()
{
	printf("A Reference variable cannot be returned as an integer.\n");
	return 0;
}

// Get value of variable as double
double ReferenceVariable::asDouble()
{
	printf("A Reference variable cannot be returned as a double.\n");
	return 0.0;
}

// Get value of variable as float
float ReferenceVariable::asFloat()
{
	printf("A Reference variable cannot be returned as a float.\n");
	return 0.0f;
}

// Get value of variable as a boolean
bool ReferenceVariable::asBool()
{
	printf("A Reference variable cannot be returned as a boolean.\n");
	return FALSE;
}

// Get value of variable as pointer of specified type
void *ReferenceVariable::asPointer(VTypes::DataType type)
{
	printf("A Reference variable cannot be returned as a pointer.\n");
	return NULL;
}

// Integer increase
void ReferenceVariable::increase(int i)
{
	printf("A Reference variable cannot be increased.\n");
}

// Integer decrease
void ReferenceVariable::decrease(int i)
{
	printf("A Reference variable cannot be decreased.\n");
}

