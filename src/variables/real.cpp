/*
	*** Double Variable
	*** src/variables/real.cpp
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

#include "variables/real.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor / Destructor
RealVariable::RealVariable()
{
	dataType_ = VTypes::RealData;
	arrayType_ = VTypes::NoArray;
	data = 0.0;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool RealVariable::set(const char *s)
{
	data = atof(s);
	return TRUE;
}

// Set value of variable (int)
bool RealVariable::set(int i)
{
	data = i;
	return TRUE;
}

// Set value of variable (double)
bool RealVariable::set(double d)
{
	data = d;
	return TRUE;
}

// Set value of variable (pointer)
bool RealVariable::set(void *ptr, VTypes::DataType type)
{
	printf("A Double variable cannot be set from a pointer.\n");
	return FALSE;
}

// Get value of variable as character string
const char *RealVariable::asCharacter()
{
	return ftoa(data);
}

// Get value of variable as integer
int RealVariable::asInteger()
{
	return (int) data;
}

// Get value of variable as double
double RealVariable::asDouble()
{
	return data;
}

// Get value of variable as float
float RealVariable::asFloat()
{
	return (float) data;
}

// Get value of variable as a boolean
bool RealVariable::asBool()
{
	return (data <= 0 ? FALSE : TRUE);
}

// Get value of variable as pointer of specified type
void *RealVariable::asPointer(VTypes::DataType type)
{
	printf("A Double variable cannot be returned as a pointer.\n");
	return NULL;
}

// Integer increase
bool RealVariable::increase(int i)
{
	data += i;
	return TRUE;
}

// Integer decrease
bool RealVariable::decrease(int i)
{
	data -= i;
	return TRUE;
}

