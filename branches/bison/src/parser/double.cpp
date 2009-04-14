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
		msg.print("A constant value (in this case an integer array) cannot be assigned to.\n");
		return FALSE;
	}
	if (doubleArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) doubleArrayData_[i] = rv.asDouble();
	return TRUE;
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
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) doubleArrayData_[i] = 0;
}

// Return value of node
bool DoubleArrayVariable::execute(ReturnValue &rv)
{
	msg.print("A whole array ('%s') cannot be passed as a value.\n", name_.get());
	return FALSE;
}

// Return value of node as array
bool DoubleArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 1) || (arrayindex > arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex, name_.get());
		return FALSE;
	}
	rv.set( doubleArrayData_[arrayindex-1] );
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
	// If the array is already allocated, free it.
	if (doubleArrayData_ != NULL) printf("Array exists already...\n");	
	if (doubleArrayData_ != NULL) delete[] doubleArrayData_;
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find array size for '%s'.\n", name_.get());
		return FALSE;
	}
	// Create new array
	arraySize_ = newsize.asInteger();
	if (arraySize_ > 0) doubleArrayData_ = new double[arraySize_];
	if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (set(rv)) return TRUE;
			else
			{
				msg.print("Error: Variable %s is of type '%s', and cannot be initialised from a value of type '%s'.\n", name_.get(), VTypes::dataType(returnType_), VTypes::dataType(rv.type()));
				return FALSE;
			}
		}
		return FALSE;
	}
	return TRUE;
}
