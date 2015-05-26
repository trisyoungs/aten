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

// Static members
Aten* Variable::aten_ = NULL;

/*
 * Variable
 */

// Constructor
Variable::Variable() : TreeNode()
{
	// Private variables
	name_ = "unnamedvariable";
	initialValue_ = NULL;
	nodeType_ = TreeNode::VarNode;
}

// Destructor (virtual)
Variable::~Variable()
{
}

/*
 * Link to Aten
 */

// Set pointer to Aten
void Variable::setAten(Aten* aten)
{
	aten_ = aten;
}

/*
 * Variable Character
 */

// Set name of variable
void Variable::setName(QString name)
{
	name_ = name;
}

// Get name of variable
QString Variable::name() const
{
	return name_;
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
			if (set(rv)) return true;
			else
			{
				Messenger::print("Error: Failed to initialise variable '%s'.", qPrintable(name_));
				return false;
			}
		}
		return false;
	}
	return true;
}

// Set initial value expression
bool Variable::setInitialValue(TreeNode* node)
{
	initialValue_ = node;
	if (initialValue_ == NULL) return true;
	// Check return types (again, int and double are interchangeable)
	VTypes::DataType dt = node->returnType();
	switch (returnType_)
	{
		case (VTypes::IntegerData):
		case (VTypes::DoubleData):
			if ((dt != VTypes::IntegerData) && (dt != VTypes::DoubleData))
			{
				Messenger::print("Error: Initial value for '%s' is of an incompatible type (%s).", qPrintable(name_), VTypes::dataType(dt));
				return false;
			}
			if ((returnType_ == VTypes::IntegerData) && (dt == VTypes::DoubleData)) Messenger::print("Warning: Initial value for integer variable '%s' is a double and will lose precision.", qPrintable(name_));
			break;
		case (VTypes::VectorData):
			if ((dt != VTypes::IntegerData) && (dt != VTypes::DoubleData) && (dt != returnType_))
			{
				Messenger::print("Error: Initial value for '%s' is of an incompatible type (%s).", qPrintable(name_), VTypes::dataType(dt));
				return false;
			}
			break;
		// Exact match required for everything else (or pointer and integer is ok)
		default:
			if (returnType_ == dt) break;
			if ((dt == VTypes::IntegerData) && (returnType_ > VTypes::VectorData)) break;
			Messenger::print("Error: Initial value for variable '%s' is of an incompatible type (%s).", qPrintable(name_), VTypes::dataType(dt));
			return false;
			break;
	}
	return true;
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
	Messenger::print("Error: Variable '%s' is not an array.", qPrintable(name_));
	return false;
}

// Set as an array
bool Variable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	// Secondary array 'set' executor
	Messenger::print("Error: Variable '%s' is not an array.", qPrintable(name_));
	return false;
}

// Search accessors (if any) available for node
StepNode* Variable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	// Default is to return NULL since no accessors are defined
	printf("Error: No accessors are available for a variable of type '%s'.\n", VTypes::dataType(returnType_));
	return NULL;
}

// Search accessor list provided
int Variable::searchAccessor(QString name, int nAccessors, Accessor *accessors)
{
	// Search for accessor (case-sensitive)
	for (int i = 0; i < nAccessors; ++i) if (name == accessors[i].name) return i;
	return -1;
}

// Search accessor list provided
int Variable::searchAccessor(QString name, int nAccessors, FunctionAccessor *accessors)
{
	// Search for accessor (case-sensitive)
	for (int i = 0; i < nAccessors; ++i) if (name == accessors[i].name) return i;
	return -1;
}

/*
 * Array Variable
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
