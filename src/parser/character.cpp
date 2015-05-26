/*
	*** String (Character) Variable
	*** src/parser/character.cpp
	Copyright T. Youngs 2007-2015

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

#include "parser/character.h"
#include "math/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
StringVariable::StringVariable()
{
	// Private variables
	returnType_ = VTypes::StringData;
	readOnly_ = false;
}


StringVariable::StringVariable(QString s, bool constant) : stringData_(s)
{
	// Private variables
	returnType_ = VTypes::StringData;
	readOnly_ = constant;
}

// Destructor
StringVariable::~StringVariable()
{
}

/*
// Set / Get
*/

// Set value of node from returnvalue
bool StringVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case a character) cannot be assigned to.");
		return false;
	}
	bool success;
	stringData_ = rv.asString(success);
	return success;
}

// Reset node
void StringVariable::reset()
{
	stringData_.clear();
}

// Return value of node
bool StringVariable::execute(ReturnValue& rv)
{
	rv.set(stringData_);
	return true;
}

/*
 * Variable Data
 */

// Print node contents
void StringVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	if (readOnly_) printf("[C]%s\"%s\" (constant value)\n", qPrintable(tab), qPrintable(stringData_));
	else printf("[V]%s\"%s\" (variable, name=%s)\n", qPrintable(tab), qPrintable(stringData_), qPrintable(name_));
}


/*
 * Variable Array
 */

// Constructor
StringArrayVariable::StringArrayVariable(TreeNode* sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
{
	// Private variables
	returnType_ = VTypes::StringData;
	stringArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
}

// Destructor
StringArrayVariable::~StringArrayVariable()
{
	if (stringArrayData_ != NULL) delete[] stringArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool StringArrayVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case an integer array) cannot be assigned to.");
		return false;
	}
	if (stringArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return false;
	}
	// Is the supplied ReturnValue an array?
	if (rv.arraySize() == -1) for (int i=0; i<arraySize_; i++) stringArrayData_[i] = rv.asString();
	else
	{
		if (rv.arraySize() != arraySize_)
		{
			Messenger::print("Error setting variable '%s': Array sizes do not conform.", qPrintable(name_));
			return false;
		}
		bool success;
		for (int i=0; i<arraySize_; i++) stringArrayData_[i] = rv.asString(i, success);
		if (!success) return false;
	}
	return true;
}

// Set array element from returnvalue node
bool StringArrayVariable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case an integer array?) cannot be assigned to.");
		return false;
	}
	if (stringArrayData_ == NULL)
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
	stringArrayData_[arrayIndex] = rv.asString();
	return true;
}

// Reset variable
void StringArrayVariable::reset()
{
	if (stringArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return;
	}
	// Loop over array elements and set them - for constant arrays only change non-constant subvalues
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (Refitem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) stringArrayData_[count++].clear();
			else stringArrayData_[count++] = value.asString();
		}
	}
	else for (int i=0; i<arraySize_; i++) stringArrayData_[i].clear();
}

// Return value of node
bool StringArrayVariable::execute(ReturnValue& rv)
{
	if (stringArrayData_ == NULL)
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
	rv.setArray(VTypes::StringData, stringArrayData_, arraySize_);
	return true;
}

// Return value of node as array
bool StringArrayVariable::executeAsArray(ReturnValue& rv, int arrayIndex)
{
	// Check bounds
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Error: Array index %i is out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	rv.set( stringArrayData_[arrayIndex] );
	return true;
}

// Print node contents
void StringArrayVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", qPrintable(tab), qPrintable(name_), arraySize_);
}

// Initialise array
bool StringArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		Messenger::print("Failed to find size for string array '%s'.", qPrintable(name_));
		return false;
	}

	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (stringArrayData_ != NULL))
	{
		delete[] stringArrayData_;
		stringArrayData_ = NULL;
	}

	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (stringArrayData_ == NULL)) stringArrayData_ = new QString[arraySize_];

	// In the case of constant arrays, use the argument list of the TreeNode to set the array elements
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (Refitem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) return false;
			stringArrayData_[count++] = value.asString();
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
