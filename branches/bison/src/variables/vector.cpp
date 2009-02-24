/*
	*** Vector Variable
	*** src/variables/vector.cpp
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

#include "variables/vector.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "base/messenger.h"
#include <stdio.h>
#include <stdlib.h>

// Constructor
VectorVariable::VectorVariable()
{
	// Private variables
	dataType_ = VTypes::VectorData;
	vectorData_.zero();
	vectorArrayData_ = NULL;
	arraySize_ = -1;
}

// Destructor
VectorVariable::~VectorVariable()
{
	if (vectorArrayData_ != NULL) delete[] vectorArrayData_;
}

/*
// Set / Get
*/

// Set size of array
bool VectorVariable::setArraySize(int size)
{
	if (arraySize_ != -1)
	{
		msg.print("Vector variable '%s' already has an array.\n", name_.get());
		return FALSE;
	}
	vectorArrayData_ = new Vec3<double>[size];
	arraySize_ = size;
	return TRUE;
}

// Set value of variable (vector)
bool VectorVariable::set(Vec3<double> v, Variable *index)
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
		if (arraySize_ != -1) for (int n=0; n<arraySize_; n++) vectorArrayData_[n] = v;
		else vectorData_ = v;
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
		else vectorArrayData_[n-1] = v;
	}
	return TRUE;
}

// Set value of variable (pointer)
bool VectorVariable::set(void *ptr, VTypes::DataType type, Variable *index)
{
	// Check read/write status
	if (readOnly_)
	{
		msg.print("Variable '%s' is read-only.\n", name_.get());
		return FALSE;
	}
	bool outofbounds = FALSE;
	// Check data type provided
	if (type != VTypes::VectorData)
	{
		msg.print("Variable '%s' cannot be set from a pointer that isn't a pointer to another vector.\n", name_.get());
		return FALSE;
	}
	// Cast pointer
	Vec3<double> *v1 = (Vec3<double>*) ptr;
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1) for (int n=0; n<arraySize_; n++) vectorArrayData_[n] = *v1;
		else vectorData_ = *v1;
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
		else vectorArrayData_[n-1] = *v1;
	}
	return TRUE;
}

// Set value of variable (int)
bool VectorVariable::set(int i, Variable *index)
{
	Vec3<double> v(i,i,i);
	return set(v, index);
}

// Set value of variable (double)
bool VectorVariable::set(double d, Variable *index)
{
	Vec3<double> v(d,d,d);
	return set(v, index);
}

// Set value of variable (char)
bool VectorVariable::set(const char *s, Variable *index)
{
	double d = atof(s);
	Vec3<double> v(d,d,d);
	return set(v, index);
}

// Get value of variable as vector
void *VectorVariable::asPointer(VTypes::DataType type, Variable *index)
{
	// Check type of variable requested
	if (type != VTypes::VectorData)
	{
		msg.print("The variable '%s' is a vector, and cannot be returned as a pointer to anything else (%s).\n", name_.get(), VTypes::dataType(type));
		return NULL;
	}
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return NULL;
		}
		return &vectorData_;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return NULL;
		}
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return NULL;
		}
		return &vectorArrayData_[n-1];
	}
}

// Get value of variable as vector
Vec3<double> VectorVariable::asVector(Variable *index)
{
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return NULL;
		}
		return vectorData_;
	}
	else
	{
		if (arraySize_ == -1)
		{
			msg.print("Array index given to variable '%s'.\n", name_.get());
			return NULL;
		}
		int n = index->asInteger();
		if ((n > arraySize_) || (n < 1))
		{
			msg.print("Array index %i is out of bounds for array '%s'.\n", n, name_.get());
			return NULL;
		}
		return vectorArrayData_[n-1];
	}
}

// Clears value of variable
bool VectorVariable::reset(Variable *index)
{
	printf("resetting vectorvariable.\n");    //TGAY
	// Check array index given
	if (index == NULL)
	{
		if (arraySize_ != -1)
		{
			msg.print("No array index given to array '%s'.\n", name_.get());
			return FALSE;
		}
		vectorData_.zero();
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
		vectorArrayData_[n-1].zero();
	}
	return TRUE;
}
