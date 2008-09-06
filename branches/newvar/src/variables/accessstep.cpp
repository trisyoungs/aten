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

#include "variables/accesspath.h"
#include "variables/accessstep.h"
#include "variables/variable.h"
#include "base/constants.h"
#include "base/messenger.h"
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
	target_ = var;
}

// Create arrayindex 'branch'
bool AccessStep::setArrayIndex(const char *path, VariableList *sourcevars)
{
	// Check existing pointer...
	if (arrayIndex_ != NULL) msg.print("AccessStep already has an array index set.\n");
	arrayIndex_ = new AccessPath;
	if (!arrayIndex_->setPath(path, sourcevars)) return FALSE;
	else return TRUE;
}

// Get return value as integer
int AccessStep::asInteger()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as an integer.\n");
	else return target_->asInteger();
	return 0;
}

// Get return value as double
double AccessStep::asDouble()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a double.\n");
	else return target_->asDouble();
	return 0.0;
}

// Get return value as bool
bool AccessStep::asBool()
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a bool.\n");
	else return target_->asBool();
	return FALSE;
}

// Get return value as pointer
void * AccessStep::asPointer(VTypes::DataType dt)
{
	if (target_ == NULL) msg.print("AccessStep has no target variable to return as a pointer.\n");
	else return target_->asPointer(dt);
	return 0;
}
