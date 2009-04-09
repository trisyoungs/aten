/*
	*** Scoped Command Node
	*** src/parser/scopenode.cpp
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

#include "parser/scopenode.h"
#include "main/aten.h"
#include "model/model.h"
#include <string.h>

// Constructor
ScopeNode::ScopeNode(NuCommand::Function func) : NuCommandNode(func)
{
	// Private variables
	nodeType_ = TreeNode::ScopedNode;
}

// Destructor
ScopeNode::~ScopeNode()
{
}

// Add global variables to list
void ScopeNode::createGlobalVariables()
{
	// Add the global Aten variable
	NuVariable *v = variables.create(NuVTypes::AtenData, "aten");
}

// Execute command
bool ScopeNode::execute(NuReturnValue &rv)
{
	// Make sure the current rendersource is up-to-date
	aten.current.rs = (aten.current.m == NULL ? NULL : aten.current.m->renderSource());
	// Reset/initialise all variables in the list
	if (!variables.initialise()) return FALSE;
	// Execute the command
	return aten.commands.call(function_, this, rv);
}

// Set from returnvalue node
bool ScopeNode::set(NuReturnValue &rv)
{
	printf("Internal Error: Trying to 'set' a ScopeNode.\n");
	return FALSE;
}

// Initialise node
bool ScopeNode::initialise()
{
	printf("Internal Error: A ScopeNode cannot be initialised.\n");
	return FALSE;
}

// Print node contents
void ScopeNode::nodePrint(int offset, const char *prefix)
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
	printf("[SN]%s (Scoped Node) (%i variables)\n", tab, variables.nVariables());
	int n = 1;
	for (TreeNode *tn = variables.first(); tn != NULL; tn = tn->next)
	{
		NuVariable *v = (NuVariable*) tn;
		printf("%s --> %3i: %s (%s)\n", tab, n++, v->name(), NuVTypes::dataType(v->returnType()));
		if (v->initialValue() != NULL) v->initialValue()->nodePrint(offset+1, "init: ");
	}
	printf("[SN]%s%s (Command) (%i arguments)\n", tab, NuCommand::data[function_].keyword, args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int> *ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
	delete[] tab;
}
