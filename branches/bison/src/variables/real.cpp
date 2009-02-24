/*
	*** Double Variable
	*** src/variables/real.cpp
	Copyright T. Youngs 2007-2009

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

// Destructor
RealVariable::~RealVariable()
{
	if (realArrayData_ != NULL) delete[] realArrayData_;
}

/*
// Set / Get
*/

// Set size of array
bool RealVariable::setArraySize(int size)
{
	if (arraySize_ != -1)
	{
		msg.print("Real variable '%s' already has an array.\n", name_.get());
		return FALSE;
	}
	realArrayData_ = new double[size];
	arraySize_ = size;
	return TRUE;
}

// Set value of variable (char)
bool RealVariable::set(const char *s, Variable *index)
{
	return (set(atof(s), index));
}

// Set value of variable (int)
bool RealVariable::set(int i, Variable *index)
{
	return set( (double) i, index);
}

// Set value of variable (double)
bool RealVariable::set(double d, Variable *index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1) for (int n=0; n<arraySize_; n++) realArrayData_[n] = d;
		else realData_ = d;
// 		if (arraySize_ != -1)
// 		{
// 			msg.print("No array index given to array '%s'.\n", name_.get());
// 			return FALSE;
// 		}
// 		realData_ = d;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		// Get array index and set value...
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return FALSE;
		}
		else realArrayData_[n-1] = d;
	}
	return TRUE;
}

// Get value of variable as character string
const char *RealVariable::asCharacter(Variable *index)
{
	return ftoa(asDouble(index));
}

// Get value of variable as integer
int RealVariable::asInteger(Variable *index)
{
	return (int) asDouble(index);
}

// Get value of variable as double
double RealVariable::asDouble(Variable *index)
{
	// Check array index given
	if (index == NULL)
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
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return FALSE;
		}
		return realArrayData_[n-1];
	}
}

// Get value of variable as a boolean
bool RealVariable::asBool(Variable *index)
{
	return (asDouble(index) <= 0 ? FALSE : TRUE);
}

// Step variable
bool RealVariable::step(int delta, Variable *index)
{
	// Check array index given
	if (index == NULL)
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
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return FALSE;
		}
		realArrayData_[n-1] += delta;
	}
	return TRUE;
}

// Clears value of variable
bool RealVariable::reset(Variable *index)
{
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		realData_ = 0.0;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return FALSE;
		}
		realArrayData_[n-1] = 0.0;
	}
	return TRUE;
}
