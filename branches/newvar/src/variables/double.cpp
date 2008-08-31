/*
	*** Double Variable
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

// Constructor / Destructor
DoubleVariable::DoubleVariable()
{
	dataType_ = VObject::DoubleVariable;
	listType_ = VObject::NoList;
	data = 0.0;
}

/*
// Set / Get
*/

// Set value of variable (char)
void DoubleVariable::set(const char *s)
{
	data = atof(s);
}

// Set value of variable (int)
void DoubleVariable::set(int i)
{
	data = i;
}

// Set value of variable (double)
void DoubleVariable::set(double d)
{
	data = d;
}

// Set value of variable (pointer)
void DoubleVariable::set(void *ptr, VObject::DataType type)
{
	printf("A Double variable cannot be set from a pointer.\n");
}

// Get value of variable as character string
const char *DoubleVariable::asCharacter()
{
	return ftoa(data);
}

// Get value of variable as integer
int DoubleVariable::asDouble()
{
	return (int) data;
}

// Get value of variable as double
double DoubleVariable::asDouble()
{
	return data;
}

// Get value of variable as float
float DoubleVariable::asFloat()
{
	return (float) data;
}

// Get value of variable as a boolean
bool DoubleVariable::asBool()
{
	return (data <= 0 ? FALSE : TRUE);
}

// Get value of variable as pointer of specified type
void *DoubleVariable::asPointer(VObject::DataType type)
{
	printf("A Double variable cannot be returned as a pointer.\n");
	return NULL;
}

// Double increase
void DoubleVariable::increase(int i)
{
	data += i;
}

// Double decrease
void DoubleVariable::decrease(int)
{
	data -= i;
}

