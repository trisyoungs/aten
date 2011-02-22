/*
	*** Command Node
	*** src/parser/commandnode.cpp
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

#include "parser/commandnode.h"
#include "parser/integer.h"
#include "parser/double.h"
#include "parser/variablenode.h"
#include "base/sysfunc.h"
#include <string.h>

// Constructor
CommandNode::CommandNode(Command::Function func) : function_(func)
{
	// Private variables
	nodeType_ = TreeNode::CmdNode;
}

// Destructor
CommandNode::~CommandNode()
{
}

// Get function
Command::Function CommandNode::function()
{
	return function_;
}

// Execute command
bool CommandNode::execute(ReturnValue &rv)
{
	// Execute the command
	return commands.call(function_, this, rv);
}

// Print node contents
void CommandNode::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
// 	printf("Function id = %p\n", function_);
	printf("[CN]%s%s (Command) (%i arguments)\n", tab.get(), Command::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
}

// Set from returnvalue node
bool CommandNode::set(ReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a CommandNode.\n");
	return FALSE;
}

// Initialise node
bool CommandNode::initialise()
{
	printf("Internal Error: A CommandNode cannot be initialised.\n");
	return FALSE;
}
