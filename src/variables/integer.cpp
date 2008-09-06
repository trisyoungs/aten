/*
	*** Integer Variable
	*** src/variables/integer.cpp
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

#include "variables/integer.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
IntegerVariable::IntegerVariable()
{
	dataType_ = VTypes::IntegerData;
	arrayType_ = VTypes::NoArray;
	data = 0;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool IntegerVariable::set(const char *s)
{
	data = atoi(s);
	return TRUE;
}

// Set value of variable (int)
bool IntegerVariable::set(int i)
{
	data = i;
	return TRUE;
}

// Set value of variable (double)
bool IntegerVariable::set(double d)
{
	data = (int) d;
	return TRUE;
}

// Set value of variable (pointer)
bool IntegerVariable::set(void *ptr, VTypes::DataType type)
{
	printf("An Integer variable cannot be set from a pointer.\n");
	return FALSE;
}

// Get value of variable as character string
const char *IntegerVariable::asCharacter()
{
	return itoa(data);
}

// Get value of variable as integer
int IntegerVariable::asInteger()
{
	return data;
}

// Get value of variable as double
double IntegerVariable::asDouble()
{
	return (double) data;
}

// Get value of variable as float
float IntegerVariable::asFloat()
{
	return (float) data;
}

// Get value of variable as a boolean
bool IntegerVariable::asBool()
{
	return (data < 1 ? FALSE : TRUE);
}

// Get value of variable as pointer of specified type
void *IntegerVariable::asPointer(VTypes::DataType type)
{
	printf("An Integer variable cannot be returned as a pointer.\n");
	return NULL;
}

// Integer increase
bool IntegerVariable::increase(int i)
{
	data += i;
	return TRUE;
}

// Integer decrease
bool IntegerVariable::decrease(int i)
{
	data -= i;
	return TRUE;
}

