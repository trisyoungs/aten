/*
	*** Variable Node
	*** src/parser/variablenode.cpp
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

#include "parser/variablenode.h"
#include "parser/returnvalue.h"
#include "parser/variable.h"

// Constructor
VariableNode::VariableNode(NuVariable *var) : variable_(var)
{
	// Private variables
	readOnly_ = FALSE;
}

// Destructor
VariableNode::~VariableNode()
{
}

// Set function
void VariableNode::setVariable(NuVariable *variable)
{
	variable_ = variable;
}

// Get function
NuVariable *VariableNode::variable()
{
	return variable_;
}

// Execute command
bool VariableNode::execute(NuReturnValue &rv)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		return FALSE;
	}
	// Call the local variable's execute() function
	printf("Executing variable.\n");
	variable_->execute(rv);
	rv.info();
	printf("Done executing variable.\n");
// 	return variable_->execute(rv);
	return TRUE;
}

// Print node contents
void VariableNode::nodePrint(int offset, const char *prefix)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		return;
	}
	// Call the local variables nodePrint() function
	variable_->nodePrint(offset, prefix);
}

// Set from returnvalue node
bool VariableNode::set(NuReturnValue &rv)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		return FALSE;
	}
	// Call the local variables set() function
	return variable_->set(rv);
}

// Reset node
void VariableNode::reset()
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		return;
	}
	variable_->reset();
}
