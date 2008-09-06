/*
	*** Variable Access Step
	*** src/variables/accessstep.cpp
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

#include "variables/accessstep.h"
#include "base/constants.h"
#include <stdlib.h>

// Constructor
AccessStep::AccessStep()
{
	// Private variables
	target_ = NULL;
	arrayIndex_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set target variable
void AccessStep::setTarget(Variable *var)
{
}

// Create arrayindex 'branch'
bool AccessStep::setArrayIndex(const char *path, VariableList *sourcevars)
{
}

// Get return value as integer
int AccessStep::asInteger()
{
}

// Get return value as double
int AccessStep::asDouble()
{
}

// Get return value as bool
int AccessStep::asBool()
{
}

// Get return value as pointer
int AccessStep::asPointer(VTypes::DataType dt)
{
}
