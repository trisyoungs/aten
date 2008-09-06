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
	doubleData_ = 0.0;
	doubleArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set value of variable (char)
bool RealVariable::set(const char *s, int index)
{
	return (set(atof(s), index));
}

// Set value of variable (int)
bool RealVariable::set(int i, int index)
{
	return set( (double) i, index);
}

// Set value of variable (double)
bool RealVariable::set(double d, int index)
{
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
		realData_ = d;
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
			else arrayRealData_[index-1] = d;
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

// Set value of variable (pointer)
bool RealVariable::set(void *ptr, VTypes::DataType type, int index)
{
	printf("A Double variable cannot be set from a pointer.\n");
	return FALSE;
}

// Get value of variable as character string
const char *RealVariable::asCharacter(int index)
{
	return ftoa(data);
}

// Get value of variable as integer
int RealVariable::asInteger(int index)
{
	return (int) data;
}

// Get value of variable as double
double RealVariable::asDouble(int index)
{
	return data;
}

// Get value of variable as float
float RealVariable::asFloat(int index)
{
	return (float) data;
}

// Get value of variable as a boolean
bool RealVariable::asBool(int index)
{
	return (data <= 0 ? FALSE : TRUE);
}

// Get value of variable as pointer of specified type
void *RealVariable::asPointer(VTypes::DataType type, int index)
{
	printf("A Double variable cannot be returned as a pointer.\n");
	return NULL;
}

// Integer increase
bool RealVariable::increase(int i, int index)
{
	data += i;
	return TRUE;
}

// Integer decrease
bool RealVariable::decrease(int i, int index)
{
	data -= i;
	return TRUE;
}

