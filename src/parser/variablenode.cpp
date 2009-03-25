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
#include "parser/stepnode.h"
#include <string.h>

// Constructor
VariableNode::VariableNode(NuVariable *var) : variable_(var)
{
	// Private variables
	readOnly_ = FALSE;
	nodeType_ = TreeNode::VarWrapperNode;
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

// Set array index
void VariableNode::setArrayIndex(TreeNode *index)
{
	arrayIndex_ = index;
}

// Return array index
TreeNode *VariableNode::arrayIndex()
{
	return arrayIndex_;
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

// Finalise path, setting return value and readOnly property from last step node
void VariableNode::finalisePath()
{
	msg.enter("VariableNode::finalisePath");
	msg.print(Messenger::Parse, "There are %i steps in the pathnode...\n", args_.nItems());
	// Return type of last argument is return type of PathNode
	if (args_.last() == NULL) returnType_ = NuVTypes::NoData;
	else
	{
		returnType_ = args_.last()->item->returnType();
		readOnly_ = args_.last()->item->readOnly();
	}
	msg.print(Messenger::Parse, "Return type of VariableNode path is '%s' and read_only status is '%s'\n", NuVTypes::dataType(returnType_), readOnly_ ? "true" : "false");
	msg.exit("VariableNode::finalisePath");
}

/*
// Set / execute
*/

// Execute command
bool VariableNode::execute(NuReturnValue &rv)
{
	msg.enter("VariableNode::execute");
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be executed.\n");
		msg.exit("VariableNode::execute");
		return FALSE;
	}
	// Call the local variable's execute() function to get the base value
	bool result;
	if (arrayIndex_ == NULL) result = variable_->execute(rv);
	else
	{
		NuReturnValue index;
		if (!arrayIndex_->execute(index)) return FALSE;
		result = variable_->executeAsArray(rv, index.asInteger());
	}
	// If a path is present (i.e. there are arguments to the VariableNode, then execute it. Otherwise, just return the variable contents
	// Next, step through accessnodes, passing the returnvalue to each in turn
	if (result) for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
	{
		result = ri->item->execute(rv);
		if (!result) break;
	}
	if (result)
	{
		printf("Final result of path walk / variable retrieval is:\n");
		rv.info();
	}
	else printf("Variable retrieval failed.\n");
	msg.exit("VariableNode::execute");
	return result;
}

// Print node contents
void VariableNode::nodePrint(int offset, const char *prefix)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be printed.\n");
		return;
	}
	// Call the local variables nodePrint() function
	variable_->nodePrint(offset, prefix);
	// If there is a path, print that as well
	if (args_.nItems() != 0)
	{
		// Construct tabbed offset
		offset++;
		char *tab;
		tab = new char[offset+32];
		tab[0] = '\0';
		for (int n=0; n<offset-1; n++) strcat(tab,"\t");
		if (offset > 1) strcat(tab,"   |--> ");
		if (offset == 1) strcat(tab,"\t");
		strcat(tab,prefix);
		printf("[PATH]%s (basevar).", tab);
		for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
		{
			ri->item->nodePrint(offset);
			if (ri->next != NULL) printf(".");
			else printf(" [%s]\n", NuVTypes::dataType(returnType_));
		}
	}
}

// Set from returnvalue node
bool VariableNode::set(NuReturnValue &setrv)
{
	msg.enter("VariableNode::set");
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be set.\n");
		msg.exit("VariableNode::set");
		return FALSE;
	}
	// If there are no path nodes then just set the local variable
	bool result = TRUE;
	NuReturnValue executerv;
	if (args_.nItems() == 0)
	{
		// Call the local variable's set() function
		if (arrayIndex_ == NULL) result = variable_->set(setrv);
		else
		{
			NuReturnValue index;
			if (!arrayIndex_->execute(index)) result = FALSE;
			else result = variable_->setAsArray(setrv, index.asInteger());
		}
	}
	else
	{
		// Call the local variable's execute() function to get the base value
		bool result2;
		if (arrayIndex_ == NULL) result2 = variable_->execute(executerv);
		else
		{
			NuReturnValue index;
			if (!arrayIndex_->execute(index)) result2 =  FALSE;
			else result2 = variable_->executeAsArray(executerv, index.asInteger());
		}
		// Next, step through accessnodes up until the last one, passing the returnvalue to each in turn
		if (result2)
		{
			for (Refitem<TreeNode,int> *ri = args_.first(); ri != args_.last(); ri = ri->next)
			{
				result = ri->item->execute(executerv);
				if (!result) break;
			}
			// For the last accessnode in the list, cast into a StepNode and use the set function
// 		printf("Node type of args_>last() is %i\n", args_.last()->nodeType());
			((StepNode*) args_.last()->item)->set(executerv, setrv);
		}
		else result = FALSE;
	}
	if (!result) printf("Variable set failed.\n");
	msg.exit("VariableNode::set");
	return result;
}

// Initialise node
bool VariableNode::initialise()
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be initialise.\n");
		return FALSE;
	}
	return variable_->initialise();
}

// Search accessors (if any) available for linked variable
StepNode *VariableNode::findAccessor(const char *s, bool array)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: No variable stored in VariableNode to use for accessor search.\n");
		return NULL;
	}
	return variable_->findAccessor(s, array);
}
