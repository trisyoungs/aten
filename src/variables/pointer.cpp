/*
	*** Pointer Variable
	*** src/variables/pointer.cpp
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

#include "variables/pointer.h"
#include "model/model.h"
#include "base/atom.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/pattern.h"
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
PointerVariable::PointerVariable(VTypes::DataType ptrtype)
{
	// Private variables
	dataType_ = ptrtype;
	ptrData_ = NULL;
	ptrArrayData_ = NULL;
	arraySize_ = -1;
}

// Destructor
PointerVariable::~PointerVariable()
{
	if (ptrArrayData_ != NULL) delete[] ptrArrayData_;
}

/*
// Set / Get
*/

// Set size of array
bool PointerVariable::setArraySize(int size)
{
	if (arraySize_ != -1)
	{
		msg.print("Pointer variable '%s' already has an array.\n", name_.get());
		return FALSE;
	}
	ptrArrayData_ = new void*[size];
	arraySize_ = size;
	return TRUE; 
}

// Set value of variable (pointer)
bool PointerVariable::set(void *ptr, VTypes::DataType type, Variable *index)
{
	if (type != dataType_)
	{
		msg.print("A Pointer variable of type '%s' (%s) cannot be set from a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
		return FALSE;
	}
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1) for (int n=0; n<arraySize_; n++) ptrArrayData_[n] = ptr;
		else ptrData_ = ptr;
/*		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		ptrData_ = ptr;*/
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
		else ptrArrayData_[n-1] = ptr;
	}
	return TRUE;
}

// Reset (non-array) variable with type and pointer provided
bool PointerVariable::reset(void *ptr, VTypes::DataType type)
{
	dataType_ = type;
	ptrData_ = ptr;
	return TRUE;
}

// Get value of variable as integer
int PointerVariable::asInteger(Variable *index)
{
	int result = 0;
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		result = ptrData_ != NULL;
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
		else result = ptrArrayData_[n-1] != NULL;
	}
	return result;
}

// Get value of variable as vector
Vec3<double> PointerVariable::asVector(Variable *index)
{
	// Check type of pointer
	if (dataType_ != VTypes::VectorData)
	{
		msg.print("Pointer variable of type '%s' cannot be returned as a vector.\n", VTypes::dataType(dataType_));
		return Vec3<double>();
	}
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return Vec3<double>();
		}
		return *((Vec3<double>*) ptrData_);
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return Vec3<double>();
		}
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return Vec3<double>();
		}
		else return *((Vec3<double>*) ptrArrayData_[n-1]);
	}
}

// Get value of variable as pointer of specified type
void *PointerVariable::asPointer(VTypes::DataType type, Variable *index)
{
	if (type != dataType_) printf("Error - a Pointer variable of type '%s' (%s) is being requested as a pointer of type '%s'\n", VTypes::dataType(dataType_), name(), VTypes::dataType(type));
	void *result = NULL;
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		result = ptrData_;
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
		else result = ptrArrayData_[n-1];
	}
	return result;
}

// Step variable
bool PointerVariable::step(int i, Variable *index)
{
	// Check array index given
	int n;
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return FALSE;
		}
		n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return FALSE;
		}
	}
	// Now get the variable and step it.
	int step;
	Atom *atomptr;
	Pattern *patternptr;
	Model *modelptr;
	switch (dataType_)
	{
		case (VTypes::AtomData):
			atomptr = (Atom*) (index == NULL ? ptrData_ : ptrArrayData_[n-1]);
			for (step=0; ((step<i) && (atomptr != NULL)); step++) atomptr = atomptr->next;
			index == NULL ? ptrData_ = atomptr : ptrArrayData_[n-1] = atomptr;
			break;
		case (VTypes::PatternData):
			patternptr = (Pattern*) (index == NULL ? ptrData_ : ptrArrayData_[n-1]);
			for (step=0; ((step<i) && (patternptr != NULL)); step++) patternptr = patternptr->next;
			index == NULL ? ptrData_ = patternptr : ptrArrayData_[n-1] = patternptr;
			break;
		case (VTypes::ModelData):
			modelptr = (Model*) (index == NULL ? ptrData_ : ptrArrayData_[n-1]);
			for (step=0; ((step<i) && (modelptr != NULL)); step++) modelptr = modelptr->next;
			index == NULL ? ptrData_ = modelptr : ptrArrayData_[n-1] = modelptr;
			break;
		default:
			printf("Don't know how to step a pointer variable of type '%s'.\n", VTypes::dataType(dataType_));
			return FALSE;
			break;
	}
	return TRUE;
}

// Clears value of variable
bool PointerVariable::reset(Variable *index)
{
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		ptrData_ = NULL;
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
		ptrArrayData_[n-1] = NULL;
	}
	return TRUE;
}
