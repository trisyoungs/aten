/*
	*** Path Node
	*** src/parser/pathnode.cpp
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

#include "parser/pathnode.h"
#include "parser/variablenode.h"
#include <string.h>

// Constructor
PathNode::PathNode(TreeNode *base) : baseVariable_(base)
{
}

// Destructor
PathNode::~PathNode()
{
}

// Finalise path, setting return value and readOnly property from last step node
void PathNode::finalise()
{
	msg.enter("PathNode::finalise");
	printf("there are %i steps in the pathnode...\n", args_.nItems());
	// Return type of last argument is return type of PathNode
	if (args_.last() == NULL) returnType_ = NuVTypes::NoData;
	else
	{
		returnType_ = args_.last()->item->returnType();
		readOnly_ = args_.last()->item->readOnly();
	}
	printf("Return type of PathNode is '%s' and read_only status is '%s'\n", NuVTypes::dataType(returnType_), readOnly_ ? "true" : "false");
	msg.exit("PathNode::finalise");
}

// Execute node
bool PathNode::execute(NuReturnValue &rv)
{
	msg.enter("PathNode::execute");
	bool result;
	// First step - retrieve the base variable result
	result = baseVariable_->execute(rv);
	// Next, step through accessnodes, passing the returnvalue to each in turn
	if (result) for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next)
	{
		result = ri->item->execute(rv);
		if (!result) break;
	}
	if (result)
	{
		printf("Final result of path walk is:\n");
		rv.info();
	}
	else printf("Path walk failed.\n");
	msg.exit("PathNode::execute");
	return result;
}

// Set from returnvalue node
bool PathNode::set(NuReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a PathNode.\n");
	return FALSE;
}

// Reset variable
void PathNode::reset()
{
	printf("XXX RESET PathNode\n");
}

// Print node contents
void PathNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	if (offset == 1) strcat(tab,"\t");
	strcat(tab,prefix);
	// Output node data
	printf("[PN]%s%s (Path) (%i steps)\n", tab, ((VariableNode*) baseVariable_)->name(), args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}

// Search accessors (if any) available for node
StepNode *PathNode::findAccessor(const char *s)
{
	// Call the base variables findAccessor
	if (baseVariable_ == NULL)
	{
		printf("Internal Error: No base variable set in PathNode.\n");
		return NULL;
	}
	return baseVariable_->findAccessor(s);
}