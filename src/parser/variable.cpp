/*
	*** Variable
	*** src/parser/variable.cpp
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

#include "parser/variable.h"
#include "parser/returnvalue.h"
#include <string.h>

// Constructor
Variable::Variable()
{
	// Private variables
	name_.set("unnamed");
	initialValue_ = NULL;
	nodeType_ = TreeNode::VarNode;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor (virtual)
Variable::~Variable()
{
}

// Set name of variable
void Variable::setName(const char* s)
{
	name_.set(s);
}

// Get name of variable
const char *Variable::name()
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
				msg.print("Error: Variable %s is of type '%s', and cannot be initialised from a value of type '%s'.\n", name_.get(), VTypes::dataType(returnType_), VTypes::dataType(rv.type()));
				return FALSE;
			}
		}
		return FALSE;
	}
	return TRUE;
}

// Set initial value expression
void Variable::setInitialValue(TreeNode *node)
{
	initialValue_ = node;
}

// Return TreeNode corresponding to initial value
TreeNode *Variable::initialValue()
{
	return initialValue_;
}

// Execute as an array
bool Variable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Secondary array 'retrieval' executor
	msg.print("Error: Variable '%s' is not an array.\n", name_.get());
	return FALSE;
}

// Set as an array
bool Variable::setAsArray(ReturnValue &rv, int arrayindex)
{
	// Secondary array 'set' executor
	msg.print("Error: Variable '%s' is not an array.\n", name_.get());
	return FALSE;
}

// Search accessors (if any) available for node
StepNode *Variable::findAccessor(const char *s, TreeNode *arrayindex)
{
	// Default is to return NULL since no accessors are defined
	printf("Error: No accessors are available for a variable of type '%s'.\n", VTypes::dataType(returnType_));
	return NULL;
}
