/*
	*** Variable Node
	*** src/parser/variablenode.cpp
	Copyright T. Youngs 2007-2010

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
#include "parser/vector.h"
#include <string.h>

// Constructor
VariableNode::VariableNode(Variable *var) : variable_(var)
{
	// Private variables
	readOnly_ = FALSE;
	nodeType_ = TreeNode::VarWrapperNode;
	arrayIndex_ = NULL;
	if (variable_ != NULL) returnType_ = var->returnType();
}

// Destructor
VariableNode::~VariableNode()
{
}

// Set function
void VariableNode::setVariable(Variable *variable)
{
	variable_ = variable;
}

// Get function
Variable *VariableNode::variable()
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
	// Return type of last argument is return type of PathNode
	if (args_.last() == NULL) returnType_ = VTypes::NoData;
	else
	{
		StepNode *step = (StepNode*) args_.last()->item;
		returnType_ = step->returnType();
		readOnly_ = step->readOnly();
		returnsArray_ = ((step->arraySize() > 0) && (step->arrayIndex() == NULL));
	}
}

/*
// Set / execute
*/

// Execute command
bool VariableNode::execute(ReturnValue &rv)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be executed.\n");
		return FALSE;
	}
	// Call the local variable's execute() function to get the base value
	bool result;
	if (arrayIndex_ == NULL) result = variable_->execute(rv);
	else
	{
		ReturnValue index;
		if (!arrayIndex_->execute(index)) return FALSE;
		result = variable_->executeAsArray(rv, index.asInteger()-1);
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
// 		printf("Final result of path walk / variable retrieval is:\n");
// 		rv.info();
	}
	else printf("Variable retrieval failed.\n");
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

		Dnchar tab(offset+32);
		for (int n=0; n<offset-1; n++) tab += '\t';
		if (offset > 1) tab.strcat("   |--> ");
		tab.strcat(prefix);

		printf("[PATH]%s (basevar).", tab.get());
		for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
		{
			ri->item->nodePrint(offset);
			if (ri->next != NULL) printf(".");
			else printf(" [%s]\n", VTypes::dataType(returnType_));
		}
	}
}

// Set from returnvalue node
bool VariableNode::set(ReturnValue &setrv)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be set.\n");
		return FALSE;
	}
	bool result = TRUE;
	ReturnValue executerv, lastresult;
	// If there are no path nodes then just set the local variable
	// Also, if the base variable is of vector type, set it in a special way...
	if (args_.nItems() == 0)
	{
		// Call the local variable's set() function
		if (arrayIndex_ == NULL) result = variable_->set(setrv);
		else
		{
			ReturnValue index;
			if (!arrayIndex_->execute(index)) result = FALSE;
			else result = variable_->setAsArray(setrv, index.asInteger() - 1);
		}
	}
	else if (variable_->returnType() == VTypes::VectorData)
	{
		// Grab accessor ID from last (only) step and use it to set the vector component
		int component = ((StepNode*) args_.last()->item)->accessor();
		if (arrayIndex_ == NULL) result = variable_->execute(executerv);
		else 
		{
			ReturnValue index;
			if (!arrayIndex_->execute(index)) result = FALSE;
			else result = variable_->executeAsArray(executerv, index.asInteger() - 1);
		}
		if (result)
		{
			VectorVariable::setAccessor(component, executerv, setrv, FALSE);
			if (arrayIndex_ == NULL) result = variable_->set(executerv);
			else
			{
				ReturnValue index;
				if (!arrayIndex_->execute(index)) result = FALSE;
				else result = variable_->setAsArray(executerv, index.asInteger() - 1);
			}

// 	OLD CODE:	executerv.set(component, setrv.asDouble());
// 			if (arrayIndex_ == NULL) result = variable_->set(executerv);
// 			else
// 			{
// 				ReturnValue index;
// 				if (!arrayIndex_->execute(index)) result = FALSE;
// 				else result = variable_->setAsArray(executerv, index.asInteger() - 1);
// 			}
		}
	}
	else
	{
		// Call the local variable's execute() function to get the base value
		bool result2;
		if (arrayIndex_ == NULL) result2 = variable_->execute(executerv);
		else
		{
			ReturnValue index;
			if (!arrayIndex_->execute(index)) result2 = FALSE;
			else result2 = variable_->executeAsArray(executerv, index.asInteger()-1);
		}
		// Next, step through accessnodes up until the last one, passing the returnvalue to each in turn.
		if (result2)
		{
			lastresult = executerv;
			for (Refitem<TreeNode,int> *ri = args_.first(); ri != args_.last(); ri = ri->next)
			{
				result = ri->item->execute(executerv);
				if (!result) break;
			}
			// For the last accessnode in the list, cast into a StepNode and use the set function
// 			printf("Node type of args_>last() is %i\n", args_.last()->item->nodeType());
// 			printf("Penultimate result is %s\n", lastresult.info());
			if (!((StepNode*) args_.last()->item)->set(executerv, setrv)) result = FALSE;
			else
			{
// 				printf("Path set result execute = %s\n", executerv.info());
				// If the node prior to the last is a vector, we must do something special!
				Refitem<TreeNode,int> *ri = args_.last()->prev;
				if ((ri != NULL) && (ri->item->returnType() == VTypes::VectorData))
				{
//					StepNode *step = (StepNode*) ri->item;
// 					printf("Previous step type = %s.\n", VTypes::dataType(step->returnType()));
					// We must 'step back' a bit here, taking the current vector result and setting the penultimate step with it
					result = ((StepNode*) ri->item)->set(lastresult,executerv);
				}
			}
		}
		else result = FALSE;
	}
	if (!result) printf("Variable set failed.\n");
	return result;
}

// Initialise node
bool VariableNode::initialise()
{
	if (variable_ == NULL)
	{
		printf("Internal Error: VariableNode contains a NULL Variable pointer and can't be initialised.\n");
		return FALSE;
	}
	return variable_->initialise();
}

// Search accessors (if any) available for linked variable
StepNode *VariableNode::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	if (variable_ == NULL)
	{
		printf("Internal Error: No variable stored in VariableNode to use for accessor search.\n");
		return NULL;
	}
	return variable_->findAccessor(s, arrayindex, arglist);
}
