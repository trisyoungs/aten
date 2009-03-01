/*
	*** Tree Node
	*** src/parser/treenode.cpp
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

#include "parser/treenode.h"
#include "parser/tree.h"
#include "variables/accesspath.h"
#include "base/sysfunc.h"
#include "templates/reflist.h"


// Constructors
TreeNode::TreeNode()
{
	// Private variables
	returnType_ = NuVTypes::NoData;
	readOnly_ = TRUE;
	parentScope_ = NULL;
	nextArgument = NULL;
	prevArgument = NULL;
}

// Destructor
TreeNode::~TreeNode()
{
}

// Sets the content type of the variable
void TreeNode::setReturnType(NuVTypes::DataType dt)
{
	returnType_ = dt;
}

// Returns content type of the variable
NuVTypes::DataType TreeNode::returnType()
{
	return returnType_;
}

// Return readonly status
bool TreeNode::readOnly()
{
	return readOnly_;
}

// Set the readonly status of the variable to TRUE
void TreeNode::setReadOnly()
{
	readOnly_ = TRUE;
}

// Return number of arguments currently assigned to node
int TreeNode::nArgs()
{
	return args_.nItems();
}

// Return whether argument i was given
bool TreeNode::hasArg(int i)
{
	return (i < args_.nItems());
}

// Add argument(s) to node
void TreeNode::addArguments(TreeNode *leaf)
{
	/*
	The supplied leaf may be a single node, or it may be a list of nodes.
	In the case of a list, we must walk backwards through the list to get to the beginning since the parser will provide only the last node of the list
	*/
	TreeNode *first;
	for (first = leaf; first != NULL; first = first->prevArgument) if (first->prevArgument == NULL) break;
	for (TreeNode *node = first; node != NULL; node = node->nextArgument) args_.add(node);
}

// Return argument specified
TreeNode *TreeNode::arg(int i)
{
	if ((i < -1) || (i >= args_.nItems()))
	{
		printf("Argument index %i is out of range.\n", i);
		return NULL;
	}
	return args_[i]->item;
}

