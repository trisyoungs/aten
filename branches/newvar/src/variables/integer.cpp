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
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
IntegerVariable::IntegerVariable()
{
	// Private variables
	dataType_ = VTypes::IntegerData;
	integerData_ = 0;
	integerArrayData_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set size of array
bool IntegerVariable::setArraySize(int size)
{
	if (arraySize_ != -1)
	{
		msg.print("Integer variable '%s' already has an array.\n", name_.get());
		return FALSE;
	}
	integerArrayData_ = new int[size];
	arraySize_ = size;
	return TRUE;
}

// Set value of variable (char)
bool IntegerVariable::set(const char *s, Variable *index)
{
	return set( atoi(s) );
}

// Set value of variable (int)
bool IntegerVariable::set(int i, Variable *index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	bool outofbounds = FALSE;
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		integerData_ = i;
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
		else integerArrayData_[n-1] = i;
	}
	return TRUE;
}

// Set value of variable (double)
bool IntegerVariable::set(double d, Variable *index)
{
	return set( (int) d);
}

// Get value of variable as character string
const char *IntegerVariable::asCharacter(Variable *index)
{
	return itoa(asInteger(index));
}

// Get value of variable as integer
int IntegerVariable::asInteger(Variable *index)
{
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		return integerData_;
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
		return integerArrayData_[n-1];
	}
}

// Get value of variable as double
double IntegerVariable::asDouble(Variable *index)
{
	return (double) asInteger(index);
}

// Get value of variable as a boolean
bool IntegerVariable::asBool(Variable *index)
{
	return (asInteger(index) < 1 ? FALSE : TRUE);
}

// Step variable
bool IntegerVariable::step(int delta, Variable *index)
{
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		integerData_ += delta;
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
		integerArrayData_[n-1] += delta;
	}
	return TRUE;
}
