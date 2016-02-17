/*
	*** Pointer Variable and Array Base
	*** src/parser/pvariable.cpp
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

#include "parser/pvariable.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

/*
// Set / Get
*/

// Set value of variable
bool PointerVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case %s) cannot be assigned to.", VTypes::aDataType(returnType_));
		return false;
	}
	bool success;
	pointerData_ = rv.asPointer(returnType_, success);
	refitemData_ = rv.refPointer();
	return success;
}

// Reset variable
void PointerVariable::reset()
{
	pointerData_ = NULL;
	refitemData_ = NULL;
}

// Return value of node
bool PointerVariable::execute(ReturnValue& rv)
{
	rv.set(returnType_, pointerData_, refitemData_);
	return true;
}

// Print node contents
void PointerVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	if (readOnly_) printf("%s%p (%s) (constant value)\n", qPrintable(tab), pointerData_, VTypes::dataType(returnType_));
	else printf("%s%p (%s) (variable, name=%s)\n", qPrintable(tab), pointerData_, VTypes::dataType(returnType_), qPrintable(name_));
}

/*
 * Variable Array
 */

// Destructor
PointerArrayVariable::~PointerArrayVariable()
{
	if (pointerArrayData_ != NULL) delete[] pointerArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool PointerArrayVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case %s array) cannot be assigned to.", VTypes::aDataType(returnType_));
		return false;
	}
	if (pointerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return false;
	}
	// Is the supplied ReturnValue an array?
	if (rv.arraySize() == -1) for (int i=0; i<arraySize_; i++) pointerArrayData_[i] = rv.asPointer(returnType_);
	else
	{
		if (rv.arraySize() != arraySize_)
		{
			Messenger::print("Error setting variable '%s': Array sizes do not conform.", qPrintable(name_));
			return false;
		}
		bool success;
		for (int i=0; i<arraySize_; i++) pointerArrayData_[i] = rv.asPointer(i, returnType_, success);
		if (!success) return false;
	}
	return true;
}

// Set array element from returnvalue node
bool PointerArrayVariable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case %s array?) cannot be assigned to.", VTypes::aDataType(returnType_));
		return false;
	}
	if (pointerArrayData_ == NULL)
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
	pointerArrayData_[arrayIndex] = rv.asPointer(returnType_);
	return true;
}

// Reset variable
void PointerArrayVariable::reset()
{
	if (pointerArrayData_ == NULL)
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
			if (!ri->item->execute(value)) pointerArrayData_[count++] = 0;
			else pointerArrayData_[count++] = value.asPointer(returnType_);
		}
	}
	else for (int i=0; i<arraySize_; i++) pointerArrayData_[i] = 0;
}

// Return value of node
bool PointerArrayVariable::execute(ReturnValue& rv)
{
	if (pointerArrayData_ == NULL)
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
	rv.setArray(returnType_, pointerArrayData_, arraySize_);
	return true;
}

// Return value of node as array
bool PointerArrayVariable::executeAsArray(ReturnValue& rv, int arrayIndex)
{
	// Check bounds
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Error: Array index %i is out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	rv.set( returnType_, pointerArrayData_[arrayIndex] );
// 	printf("Executed :: '%s'\n", rv.info());
	return true;
}

// Print node contents
void PointerArrayVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[V]%s (%s array, name=%s, current size=%i)\n", qPrintable(tab), VTypes::dataType(returnType_), qPrintable(name_), arraySize_);
}

// Initialise array
bool PointerArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		Messenger::print("Failed to find size for %s array '%s'.", VTypes::aDataType(returnType_), qPrintable(name_));
		return false;
	}

	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (pointerArrayData_ != NULL))
	{
		delete[] pointerArrayData_;
		pointerArrayData_ = NULL;
	}

	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (pointerArrayData_ == NULL)) pointerArrayData_ = new void*[arraySize_];
	// In the case of constant arrays, use the argument list of the TreeNode to set the array elements
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (RefListItem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) return false;
			pointerArrayData_[count++] = value.asPointer(returnType_);
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

