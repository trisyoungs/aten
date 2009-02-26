/*
	*** Tree
	*** src/parser/tree.cpp
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

#include "parser/tree.h"
#include "parser/treenode.h"
#include "parser/scopenode.h"
#include "parser/commandnode.h"
#include "parser/grammar.h"
#include <stdarg.h>

// YYParse forward
int yyparse();

// Singleton
Tree *Tree::currentTree;

// Constructor
Tree::Tree()
{
	// Private variables
	isFileSource_ = FALSE;
	fileSource_ = NULL;
	stringPos_ = -1;
	stringLength_ = 0;

	// Public variables
	currentTree = NULL;
}

// Destructor
Tree::~Tree()
{
	clear();
}

/*
// Create / Execute
*/

// Clear contents of tree
void Tree::clear()
{
	// Manually delete the nodes owned by this Tree
	for (Refitem<TreeNode,int> *ri = ownedNodes_.first(); ri != NULL; ri = ri->next) delete ri->item;
	ownedNodes_.clear();
	statements_.clear();
}

// Create tree from string
bool Tree::generate(const char *s)
{
	msg.enter("Tree::generate");
	clear();
	// Store this as the current Tree (for Bison) and add a dummy ScopeNode to contain the main variable list
	currentTree = this;
	ScopeNode *root = new ScopeNode(NuCommand::NoFunction);
	ownedNodes_.add(root);
	scopeNodes_.add(root);
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	isFileSource_ = FALSE;
	int result = yyparse();
	currentTree = NULL;
	if (result != 0)
	{
		// Delete any tree node information
		clear();
		msg.print("Failed to parse data.\n");
	}
	else print();
	msg.exit("Tree::generate");
	return (result == 0);
}

// Execute tree
int Tree::execute(NuReturnValue &rv)
{
	msg.enter("Tree::execute");
	int result;
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		result = ri->item->execute(rv);
		if (result != NuCommand::Success) break;
	}
	printf("Final result of tree execution:\n");
	rv.info();
	msg.exit("Tree::execute");
	return result;
}

// Print tree
void Tree::print()
{
	printf("Leaf Structure:\n");
	int n=0;
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		printf("-------------------------------------------------------------\n");
		printf("Statement %i:\n", n);
		ri->item->nodePrint(1);
		n ++;
		printf("-------------------------------------------------------------\n");
	}
}

/*
// Character Stream Retrieval
*/

// Get next character from current input stream
char Tree::getChar()
{
	char c = 0;
	if (isFileSource_)
	{
	}
	else
	{
		// Return current character
		c = stringSource_[stringPos_];
		// Increment string position
		stringPos_++;
	}
	return c;
}

// 'Replace' last character read from current input stream
void Tree::unGetChar()
{
	if (isFileSource_)
	{
	}
	else
	{
		// Decrement string position
		stringPos_--;
	}
}

// Add a node representing a whole statement to the execution list
void Tree::addStatement(TreeNode *leaf)
{
	statements_.add(leaf);
}

// Add command-based leaf node to topmost branch on stack
TreeNode *Tree::addCommandLeaf(NuCommand::Function func, int nargs, ...)
{
	// Create variable argument parser
	va_list vars;
	va_start(vars,nargs);
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(func);
	ownedNodes_.add(leaf);
	// Add arguments
	for (int n=0; n<nargs; n++) leaf->addArgument(va_arg(vars, TreeNode*));
	va_end(vars);
	return leaf;
}

// Add joiner
TreeNode *Tree::addJoiner(TreeNode *node1, TreeNode *node2)
{
	printf("Adding a joiner...\n");
	NuCommandNode *leaf = new NuCommandNode(NuCommand::Joiner);
	ownedNodes_.add(leaf);
	if (node1 != NULL) leaf->addArgument(node1);
	if (node2 != NULL) leaf->addArgument(node2);
	return leaf;
}

// Add variable to topmost scope
void Tree::addVariable(NuVTypes::DataType type, const char *name)
{
	// Create the supplied variable in the list of the topmost scope
	if (!scopeNodes_.last()->item->variables.create(type, name)) printf("ERROR!\n");
}

// Search for variable in current scope
NuVariable *Tree::isVariableInScope(const char *name)
{
	// Search the current ScopeNode list for the variable name requested
	NuVariable *v = NULL;
	for (Refitem<ScopeNode,int> *ri = scopeNodes_.first(); ri != NULL; ri =ri->next)
	{
		v = ri->item->variables.find(name);
		if (v != NULL) break;
	}
	return v;
}
