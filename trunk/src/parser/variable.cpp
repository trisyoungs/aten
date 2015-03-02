/*
	*** Variable
	*** src/parser/variable.cpp
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

#include "parser/variable.h"
#include "parser/returnvalue.h"
#include "parser/accessor.h"

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
Variable::Variable() : TreeNode()
{
	// Private variables
	name_.set("unnamedvariable");
	initialValue_ = NULL;
	nodeType_ = TreeNode::VarNode;
}

// Destructor (virtual)
Variable::~Variable()
{
}

/*
 * Variable Character
 */

// Set name of variable
void Variable::setName(const char* s)
{
	name_.set(s);
}

// Get name of variable
const char* Variable::name() const
{
	return name_.get();
}

// Initialise variable
bool Variable::initialise()
{
	if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (set(rv)) return TRUE;
			else
			{
				Messenger::print("Error: Failed to initialise variable '%s'.\n", name_.get());
				return FALSE;
			}
		}
		return FALSE;
	}
	return TRUE;
}

// Set initial value expression
bool Variable::setInitialValue(TreeNode* node)
{
	initialValue_ = node;
	if (initialValue_ == NULL) return TRUE;
	// Check return types (again, int and double are interchangeable)
	VTypes::DataType dt = node->returnType();
	switch (returnType_)
	{
		case (VTypes::IntegerData):
		case (VTypes::DoubleData):
			if ((dt != VTypes::IntegerData) && (dt != VTypes::DoubleData))
			{
				Messenger::print("Error: Initial value for '%s' is of an incompatible type (%s).\n", name_.get(), VTypes::dataType(dt));
				return FALSE;
			}
			if ((returnType_ == VTypes::IntegerData) && (dt == VTypes::DoubleData)) Messenger::print("Warning: Initial value for integer variable '%s' is a double and will lose precision.\n", name_.get());
			break;
		case (VTypes::VectorData):
			if ((dt != VTypes::IntegerData) && (dt != VTypes::DoubleData) && (dt != returnType_))
			{
				Messenger::print("Error: Initial value for '%s' is of an incompatible type (%s).\n", name_.get(), VTypes::dataType(dt));
				return FALSE;
			}
			break;
		// Exact match required for everything else (or pointer and integer is ok)
		default:
			if (returnType_ == dt) break;
			if ((dt == VTypes::IntegerData) && (returnType_ > VTypes::VectorData)) break;
			Messenger::print("Error: Initial value for variable '%s' is of an incompatible type (%s).\n", name_.get(), VTypes::dataType(dt));
			return FALSE;
			break;
	}
	return TRUE;
}

// Return TreeNode corresponding to initial value
TreeNode* Variable::initialValue() const
{
	return initialValue_;
}

// Execute as an array
bool Variable::executeAsArray(ReturnValue& rv, int arrayIndex)
{
	// Secondary array 'retrieval' executor
	Messenger::print("Error: Variable '%s' is not an array.\n", name_.get());
	return FALSE;
}

// Set as an array
bool Variable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	// Secondary array 'set' executor
	Messenger::print("Error: Variable '%s' is not an array.\n", name_.get());
	return FALSE;
}

// Search accessors (if any) available for node
StepNode* Variable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	// Default is to return NULL since no accessors are defined
	printf("Error: No accessors are available for a variable of type '%s'.\n", VTypes::dataType(returnType_));
	return NULL;
}

// Search accessor list provided
int Variable::searchAccessor(const char* s, int nAccessors, Accessor *accessors)
{
	// Search for accessor (case-sensitive)
	for (int i = 0; i < nAccessors; ++i) if (strcmp(accessors[i].name,s) == 0) return i;
	return -1;
}

// Search accessor list provided
int Variable::searchAccessor(const char* s, int nAccessors, FunctionAccessor *accessors)
{
	// Search for accessor (case-sensitive)
	for (int i = 0; i < nAccessors; ++i) if (strcmp(accessors[i].name,s) == 0) return i;
	return -1;
}

/*
// Array Variable
*/

// Constructor
ArrayVariable::ArrayVariable()
{
	arraySizeExpression_ = NULL;
	arraySize_ = -1;
}

// Destructor (virtual)
ArrayVariable::~ArrayVariable()
{
}

// Return current array size
int ArrayVariable::arraySize() const
{
	return arraySize_;
}
