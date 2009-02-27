/*
	*** Command Node
	*** src/parser/commandnode.cpp
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

#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include <string.h>

// Constructor
NuCommandNode::NuCommandNode(NuCommand::Function func) : function_(func)
{
	// Private variables
}

// Destructor
NuCommandNode::~NuCommandNode()
{
}

// Set function
void NuCommandNode::setFunction(NuCommand::Function cf)
{
	function_ = cf;
}

// Get function
NuCommand::Function NuCommandNode::function()
{
	return function_;
}

// Execute command
bool NuCommandNode::execute(NuReturnValue &rv)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	// Check whether to disregard flow control nodes
	return nucommands.call(function_, this, rv);
}

// Print node contents
void NuCommandNode::nodePrint(int offset, const char *prefix)
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
	printf("%s%s (Command) (%i arguments)\n", tab, NuCommand::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}

// Set from returnvalue node
bool NuCommandNode::set(NuReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a CommandNode.\n");
	return FALSE;
}

// Reset node
void NuCommandNode::reset()
{
	printf("XXX RESET COMMANDNODE.\n");
}
