/*
	*** Special Variable
	*** src/variables/special.cpp
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

#include "variables/special.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor / Destructor
SpecialVariable::SpecialVariable()
{
	dataType_ = VTypes::RealData;
	realData_ = 0.0;
	realArrayData_ = NULL;
	arraySize_ = -1;
}

// Destructor
SpecialVariable::~SpecialVariable()
{
	if (realArrayData_ != NULL) delete[] realArrayData_;
}

/*
// Set / Get
*/

// Set type of special variable
void SpecialVariable::setSpecialData(SpecialData sd)
{
}

// Get value of variable as character string
const char *SpecialVariable::asCharacter(Variable *index)
{
	return ftoa(asDouble(index));
}

// Get value of variable as integer
int SpecialVariable::asInteger(Variable *index)
{
	return (int) asDouble(index);
}

// Get value of variable as double
double SpecialVariable::asDouble(Variable *index)
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
bool SpecialVariable::asBool(Variable *index)
{
	return (asDouble(index) <= 0 ? FALSE : TRUE);
}
