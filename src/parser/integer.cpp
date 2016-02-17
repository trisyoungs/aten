/*
	*** Integer Variable and Array
	*** src/parser/integer.cpp
	Copyright T. Youngs 2007-2016

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

#include "parser/integer.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
IntegerVariable::IntegerVariable(int i, bool constant) : integerData_(i)
{
	// Private variables
	returnType_ = VTypes::IntegerData;
	readOnly_ = constant;
}

// Destructor
IntegerVariable::~IntegerVariable()
{
}

/*
// Set / Get
*/

// Set from returnvalue node
bool IntegerVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case an integer) cannot be assigned to.");
		return false;
	}
	bool success;
	integerData_ = rv.asInteger(success);
	return success;
}

// Reset variable
void IntegerVariable::reset()
{
	integerData_ = 0;
}


// Return value of node
bool IntegerVariable::execute(ReturnValue& rv)
{
	rv.set(integerData_);
	return true;
}

// Print node contents
void IntegerVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	if (readOnly_) printf("[C]%s%i (constant value)\n", qPrintable(tab), integerData_);
	else printf("[V]%s%i (variable, name=%s)\n", qPrintable(tab), integerData_, qPrintable(name_));
}

/*
 * Variable Array
 */

// Constructor
IntegerArrayVariable::IntegerArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::IntegerData;
	integerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Destructor
IntegerArrayVariable::~IntegerArrayVariable()
{
	if (integerArrayData_ != NULL) delete[] integerArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool IntegerArrayVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case an int array) cannot be assigned to.");
		return false;
	}
	if (integerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return false;
	}
	bool success = false;
	// Is the supplied ReturnValue an array or a vector?
	if (rv.type() == VTypes::VectorData)
	{
		if (arraySize_ == 3)
		{
			Vec3<double>v = rv.asVector(success);
			integerArrayData_[0] = v.x;
			integerArrayData_[1] = v.y;
			integerArrayData_[2] = v.z;
		}
		else Messenger::print("Error setting variable '%s': Array size must be 3 in order to set from a vector.", qPrintable(name_));
	}
	else if (rv.arraySize() == -1) for (int i=0; i<arraySize_; i++) integerArrayData_[i] = rv.asDouble(success);
	else
	{
		if (rv.arraySize() != arraySize_) Messenger::print("Error setting variable '%s': Array sizes do not conform.\n", qPrintable(name_));
		else for (int i=0; i<arraySize_; i++) integerArrayData_[i] = rv.asInteger(i, success);
	}
	return success;
}

// Set array element from returnvalue node
bool IntegerArrayVariable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case an integer array?) cannot be assigned to.");
		return false;
	}
	if (integerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return false;
	}
	// Check index
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Index %i out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	// Set individual element
	integerArrayData_[arrayIndex] = rv.asInteger();
	return true;
}

// Reset variable
void IntegerArrayVariable::reset()
{
	if (integerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return;
	}
	// Loop over array elements and set them - for constant arrays only change non-constant subvalues
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (RefListItem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) integerArrayData_[count++] = 0;
			else integerArrayData_[count++] = value.asInteger();
		}
	}
	else for (int i=0; i<arraySize_; i++) integerArrayData_[i] = 0;
}

// Return value of node
bool IntegerArrayVariable::execute(ReturnValue& rv)
{
	if (integerArrayData_ == NULL)
	{
		if (!readOnly_)
		{
			printf("Internal Error: Array '%s' has not been initialised and can't be executed.\n", qPrintable(name_));
			return false;
		}
		if (!initialise())
		{
			printf("Internal Error: Array '%s' failed to initialise and so can't be executed.\n", qPrintable(name_));
			return false;
		}
	}
	else if (readOnly_) reset();
	rv.setArray(VTypes::IntegerData, integerArrayData_, arraySize_);
	return true;
}

// Return value of node as array
bool IntegerArrayVariable::executeAsArray(ReturnValue& rv, int arrayIndex)
{
	// Check bounds
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Error: Array index %i is out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	rv.set( integerArrayData_[arrayIndex] );
	return true;
}

// Print node contents
void IntegerArrayVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", qPrintable(tab), qPrintable(name_), arraySize_);
}

// Return array pointer
int *IntegerArrayVariable::arrayData()
{
	return integerArrayData_;
}

// Initialise array
bool IntegerArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		Messenger::print("Failed to find size for int array '%s'.", qPrintable(name_));
		return false;
	}
	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (integerArrayData_ != NULL)) { delete[] integerArrayData_; integerArrayData_ = NULL; }
	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (integerArrayData_ == NULL)) integerArrayData_ = new int[arraySize_];
	// In the case of constant arrays, use the argument list of the TreeNode to set the array elements
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (RefListItem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) return false;
			integerArrayData_[count++] = value.asInteger();
		}
	}
	else if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (!set(rv)) return false;
		}
		else return false;
	}
	return true;
}
