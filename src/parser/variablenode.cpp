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
	if (variable_ != NULL) returnType_ = var->returnType();
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

// Return name of variable target
const char *VariableNode::name()
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		return "NULL";
	}
	return variable_->name();
}

// Execute command
bool VariableNode::execute(NuReturnValue &rv)
{
	msg.enter("VariableNode::execute");
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer.\n");
		msg.exit("VariableNode::execute");
		return FALSE;
	}
	// Call the local variable's execute() function
	bool result = variable_->execute(rv);
	rv.info();
	msg.exit("VariableNode::execute");
	return result;
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

// Search accessors (if any) available for linked variable
StepNode *VariableNode::findAccessor(const char *s)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: No variable stored in VariableNode to use for accessor search.\n");
		return NULL;
	}
	return variable_->findAccessor(s);
}
