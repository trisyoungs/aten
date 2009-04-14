/*
	*** Pointer Variable and Array Base
	*** src/parser/pvariable.cpp
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

#include "parser/pvariable.h"

/*
// Variable
*/

/*
// Set / Get
*/

// Set value of variable
bool PointerVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case %s) cannot be assigned to.\n", VTypes::aDataType(returnType_));
		return FALSE;
	}
	bool success;
	pointerData_ = rv.asPointer(returnType_, success);
	return success;
}

// Reset variable
void PointerVariable::reset()
{
	pointerData_ = NULL;
}

// Return value of node
bool PointerVariable::execute(ReturnValue &rv)
{
	rv.set(returnType_, pointerData_);
	return TRUE;
}

// Print node contents
void PointerVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("%s%li (%s) (constant value)\n", tab, pointerData_, VTypes::dataType(returnType_));
	else printf("%s%li (%s) (variable, name=%s)\n", tab, pointerData_, VTypes::dataType(returnType_), name_.get());
	delete[] tab;
}

/*
// Variable Array
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
bool PointerArrayVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case %s array) cannot be assigned to.\n", VTypes::aDataType(returnType_));
		return FALSE;
	}
	if (pointerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) pointerArrayData_[i] = rv.asPointer(returnType_);
	return TRUE;
}

// Set array element from returnvalue node
bool PointerArrayVariable::setAsArray(ReturnValue &rv, int arrayindex)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case %s array?) cannot be assigned to.\n", VTypes::aDataType(returnType_));
		return FALSE;
	}
	if (pointerArrayData_ == NULL)
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
	pointerArrayData_[arrayindex] = rv.asPointer(returnType_);
	return TRUE;
}

// Reset variable
void PointerArrayVariable::reset()
{
	if (pointerArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) pointerArrayData_[i] = 0;
}

// Return value of node
bool PointerArrayVariable::execute(ReturnValue &rv)
{
	msg.print("A whole vector array ('%s') cannot be passed as value.\n", name_.get());
	return FALSE;
}

// Return value of node as array
bool PointerArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	rv.set( returnType_, pointerArrayData_[arrayindex] );
	printf("Executed :: '%s'\n", rv.info());
	return TRUE;
}

// Print node contents
void PointerArrayVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s (%s array, name=%s, current size=%i)\n", tab, VTypes::dataType(returnType_), name_.get(), arraySize_);
	delete[] tab;
}

// Initialise array
bool PointerArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// If the array is already allocated, free it.
	if (pointerArrayData_ != NULL) printf("Array exists already...\n");	
	if (pointerArrayData_ != NULL) delete[] pointerArrayData_;
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find array size for '%s'.\n", name_.get());
		return FALSE;
	}
	// Create new array
	arraySize_ = newsize.asInteger();
	if (arraySize_ > 0) pointerArrayData_ = new void*[arraySize_];
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

