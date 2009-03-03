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
#include <string.h>

// Constructor
PathNode::PathNode(int ac, NuVTypes::DataType prevtype) : accessor_(ac), previousType_(prevtype)
{
	// Private variables
	returnType_ = NuVTypes::NoData;
}

// Destructor
PathNode::~PathNode()
{
}

// Execute node
bool PathNode::execute(NuReturnValue &rv)
{
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
// 	printf("%s (Path Node) (%i variables)\n", tab, variables.nVariables());
	int n = 1;
	//printf("%s%s (Command) (%i arguments)\n", tab, NuCommand::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}
