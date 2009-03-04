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
PathNode::PathNode(TreeNode *base, TreeNode *path) : baseVariable_(base)
{
	// Similar to command argument lists, we must reverse and store the provided linked list of path nodes
	if (path == NULL) printf("Internal Error: NULL path passed in construction of PathNode.\n");
	else addArguments(path);

	// Return type of last argument is return type of PathNode
	if (args_.last() == NULL) returnType_ = NuVTypes::NoData;
	else returnType_ = args_.last()->item->returnType();
	printf("Return type of PathNode is '%s'\n", NuVTypes::dataType(returnType_));
}

// Destructor
PathNode::~PathNode()
{
}

// Execute node
bool PathNode::execute(NuReturnValue &rv)
{
	printf("Executing pathnode...\n");
	// First step - retrieve the base variable result
	if (!baseVariable_->execute(rv)) return FALSE;
	// Next, step through accessnodes, passing the returnvalue to each in turn
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) if (!ri->item->execute(rv)) return FALSE;
	return TRUE;
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
	printf("%s%s (Path) (%i steps)\n", tab, ((VariableNode*) baseVariable_)->name(), args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}
