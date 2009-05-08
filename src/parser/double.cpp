/*
	*** Double Variable and Array
	*** src/parser/double.cpp
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

#include "parser/double.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
DoubleVariable::DoubleVariable(double d, bool constant) : doubleData_(d)
{
	// Private variables
	returnType_ = VTypes::DoubleData;
	readOnly_ = constant;
}

// Destructor
DoubleVariable::~DoubleVariable()
{
}

/*
// Set / Get
*/

// Set value of variable (real)
bool DoubleVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a double) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	doubleData_ = rv.asDouble(success);
	return success;
}

// Reset variable
void DoubleVariable::reset()
{
	doubleData_ = 0.0;
}

// Return value of node
bool DoubleVariable::execute(ReturnValue &rv)
{
	rv.set(doubleData_);
	return TRUE;
}

// Print node contents
void DoubleVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s%f (constant value)\n", tab, doubleData_);
	else printf("[V]%s%f (variable, name=%s)\n", tab, doubleData_, name_.get());
	delete[] tab;
}

/*
// Variable Array
*/

// Constructor
DoubleArrayVariable::DoubleArrayVariable(TreeNode *sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
{
	// Private variables
	returnType_ = VTypes::DoubleData;
	doubleArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
}

// Destructor
DoubleArrayVariable::~DoubleArrayVariable()
{
	if (doubleArrayData_ != NULL) delete[] doubleArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool DoubleArrayVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a double array) cannot be assigned to.\n");
		return FALSE;
	}
	if (doubleArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	bool success = FALSE;
	// Is the supplied ReturnValue an array?
	if (rv.type() == VTypes::VectorData)
	{
		if (arraySize_ == 3)
		{
			Vec3<double>v = rv.asVector(success);
			doubleArrayData_[0] = v.x;
			doubleArrayData_[1] = v.y;
			doubleArrayData_[2] = v.z;
		}
		else msg.print("Error setting variable '%s': Array size must be 3 in order to set from a vector.\n", name_.get());
	}
	else if (rv.arraySize() == -1) for (int i=0; i<arraySize_; i++) doubleArrayData_[i] = rv.asDouble(success);
	else
	{
		if (rv.arraySize() != arraySize_) msg.print("Error setting variable '%s': Array sizes do not conform.\n", name_.get());
		else for (int i=0; i<arraySize_; i++) doubleArrayData_[i] = rv.asDouble(i, success);
	}
	return success;
}

// Set array element from returnvalue node
bool DoubleArrayVariable::setAsArray(ReturnValue &rv, int arrayindex)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case an integer array?) cannot be assigned to.\n");
		return FALSE;
	}
	if (doubleArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Check index
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Index %i out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	// Set individual element
	doubleArrayData_[arrayindex] = rv.asDouble();
	return TRUE;
}

// Reset variable
void DoubleArrayVariable::reset()
{
	if (doubleArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return;
	}
	// Loop over array elements and set them - for constant arrays only change non-constant subvalues
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
		{
			count++;
			if (ri->item->readOnly()) continue;
			if (!ri->item->execute(value)) doubleArrayData_[count] = 0;
			else doubleArrayData_[count] = value.asDouble();
		}
	}
	else for (int i=0; i<arraySize_; i++) doubleArrayData_[i] = 0;
}

// Return value of node
bool DoubleArrayVariable::execute(ReturnValue &rv)
{
	if (doubleArrayData_ == NULL)
	{
		if (!readOnly_)
		{
			printf("Internal Error: Array '%s' has not been initialised and can't be executed.\n", name_.get());
			return FALSE;
		}
		if (!initialise())
		{
			printf("Internal Error: Array '%s' failed to initialise and so can't be executed.\n", name_.get());
			return FALSE;
		}
	}
	else if (readOnly_) reset();
	rv.set(VTypes::DoubleData, doubleArrayData_, arraySize_);
	return TRUE;
}

// Return value of node as array
bool DoubleArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	rv.set( doubleArrayData_[arrayindex] );
	return TRUE;
}

// Print node contents
void DoubleArrayVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", tab, name_.get(), arraySize_);
	delete[] tab;
}


// Initialise array
bool DoubleArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find size for double array '%s'.\n", name_.get());
		return FALSE;
	}
	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (doubleArrayData_ != NULL)) { delete[] doubleArrayData_; doubleArrayData_ = NULL; }
	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (doubleArrayData_ == NULL)) doubleArrayData_ = new double[arraySize_];
	// In the case of constant arrays, use the argument list of the TreeNode to set the array elements
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) return FALSE;
			doubleArrayData_[count++] = value.asDouble();
		}
	}
	else if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (!set(rv)) return FALSE;
		}
		else return FALSE;
	}
	return TRUE;
}
