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
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor / Destructor
RealVariable::RealVariable()
{
	dataType_ = VTypes::RealData;
	realData_ = 0.0;
	realArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set size of array
bool RealVariable::setArraySize(int size)
{
	if (arraySize_ != -1) msg.print("Warning - Real variable '%s' already has an array.\n", name_.get());
	realArrayData_ = new double[size];
	arraySize_ = size;
}

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
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		realData_ = d;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		// Get array and set value...
		if ((index > arraySize_) || (index < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", index, name_.get());
			return FALSE;
		}
		else realArrayData_[index-1] = d;
	}
	return TRUE;
}

// Get value of variable as character string
const char *RealVariable::asCharacter(int index)
{
	return ftoa(asDouble(index));
}

// Get value of variable as integer
int RealVariable::asInteger(int index)
{
	return (int) asDouble(index);
}

// Get value of variable as double
double RealVariable::asDouble(int index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return 0.0;
	}
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		return realData_;
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
		return realArrayData_[index-1];
	}
}

// Get value of variable as a boolean
bool RealVariable::asBool(int index)
{
	return (asDouble(index) <= 0 ? FALSE : TRUE);
}

// Step variable
bool RealVariable::step(int delta, int index)
{
	// Check array index given
	if (index == -1)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		realData_ += delta;
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
		realArrayData_[index-1] += delta;
	}
	return TRUE;
}
