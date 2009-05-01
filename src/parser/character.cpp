/*
	*** String (Character) Variable
	*** src/parser/character.cpp
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

#include "parser/character.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
StringVariable::StringVariable()
{
	// Private variables
	returnType_ = VTypes::StringData;
	readOnly_ = FALSE;
}


StringVariable::StringVariable(const char *s, bool constant) : stringData_(s)
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
bool StringVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a character) cannot be assigned to.\n");
		return FALSE;
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
bool StringVariable::execute(ReturnValue &rv)
{
	rv.set(stringData_.get());
	return TRUE;
}

// Print node contents
void StringVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s\"%s\" (constant value)\n", tab, stringData_.get());
	else printf("[V]%s\"%s\" (variable, name=%s)\n", tab, stringData_.get(), name_.get());
	delete[] tab;
}


/*
// Variable Array
*/

// Constructor
StringArrayVariable::StringArrayVariable(TreeNode *sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
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
bool StringArrayVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case an integer array) cannot be assigned to.\n");
		return FALSE;
	}
	if (stringArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Is the supplied ReturnValue an array?
	if (rv.arraySize() == -1) for (int i=0; i<arraySize_; i++) stringArrayData_[i] = rv.asString();
	else
	{
		if (rv.arraySize() != arraySize_)
		{
			msg.print("Error setting variable '%s': Array sizes do not conform.\n", name_.get());
			return FALSE;
		}
		bool success;
		for (int i=0; i<arraySize_; i++) stringArrayData_[i] = rv.elementAsString(i, success);
		if (!success) return FALSE;
	}
	return TRUE;
}

// Set array element from returnvalue node
bool StringArrayVariable::setAsArray(ReturnValue &rv, int arrayindex)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case an integer array?) cannot be assigned to.\n");
		return FALSE;
	}
	if (stringArrayData_ == NULL)
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
	stringArrayData_[arrayindex] = rv.asString();
	return TRUE;
}

// Reset variable
void StringArrayVariable::reset()
{
	if (stringArrayData_ == NULL)
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
			if (!ri->item->execute(value)) stringArrayData_[count].clear();
			else stringArrayData_[count] = value.asString();
		}
	}
	else for (int i=0; i<arraySize_; i++) stringArrayData_[i].clear();
}

// Return value of node
bool StringArrayVariable::execute(ReturnValue &rv)
{
	msg.print("A whole array ('%s') cannot be passed as a value.\n", name_.get());
	return FALSE;
}

// Return value of node as array
bool StringArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	rv.set( stringArrayData_[arrayindex].get() );
	return TRUE;
}

// Print node contents
void StringArrayVariable::nodePrint(int offset, const char *prefix)
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
bool StringArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find size for string array '%s'.\n", name_.get());
		return FALSE;
	}
	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (stringArrayData_ != NULL)) { delete[] stringArrayData_; stringArrayData_ = NULL; }
	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (stringArrayData_ == NULL)) stringArrayData_ = new Dnchar[arraySize_];
	// In the case of constant arrays, use the argument list of the TreeNode to set the array elements
	if (readOnly_)
	{
		int count = 0;
		ReturnValue value;
		for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
		{
			if (!ri->item->execute(value)) return FALSE;
			stringArrayData_[count++] = value.asString();
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
