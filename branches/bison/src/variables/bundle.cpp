/*
	*** Bundle-based Variable
	*** src/variables/bundle.cpp
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

#include "variables/bundle.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include "base/bundle.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
BundleVariable::BundleVariable(VTypes::DataType ptrtype)
{
	// Private variables
	dataType_ = ptrtype;
	data_ = NULL;
	arraySize_ = -1;
}

/*
// Set / Get
*/

// Set size of array
bool BundleVariable::setArraySize(int size)
{
	msg.print("A BundlePointer cannot be declared as an array.\n");
	return FALSE;
}

// Set value of variable (pointer to pointer variable)
bool BundleVariable::set(void *bundle, VTypes::DataType type, Variable *index)
{
	if (type != dataType_)
	{
		msg.print("A BundleVariable of type '%s' (%s) cannot be set from a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
		return FALSE;
	}
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	// Check array index given
	if (index != NULL)
	msg.print("Array index given to BundleVariable '%s'.\n", name_.get());
	data_ = (Bundle*) bundle;
	return TRUE;
}

// Get value of variable as pointer of specified type
void *BundleVariable::asPointer(VTypes::DataType type, Variable *index)
{
	if (type != dataType_) printf("Error - a member of type '%s' is being requested from a BundleVariable of type '%s' (%s)\n", VTypes::dataType(type), name(), VTypes::dataType(dataType_));
	void *result = NULL;
	// Check array index given
	if (index != NULL)
	{
		msg.print("Array index given to BundleVariable '%s'.\n", name_.get());
		return NULL;
	}
	switch (dataType_)
	{
		case (VTypes::ModelData):
			result = data_->m;
			break;
		case (VTypes::AtomData):
			result = data_->i;
			break;
		default:
			msg.print("No pointer of type '%s' exists in a Bundle.\n", VTypes::dataType(type));
	}
	return result;
}

// Step variable
bool BundleVariable::step(int i, Variable *index)
{
	printf("More work needed here...\n");
	return FALSE;
}
