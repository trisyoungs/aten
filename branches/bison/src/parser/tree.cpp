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
#include "parser/commandnode.h"
#include "parser/grammar.h"

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
	headNode_ = NULL;

	// Public variables
	currentTree = NULL;
}

// Destructor
Tree::~Tree()
{
}

/*
// Create / Execute
*/

// Create tree from string
bool Tree::generate(const char *s)
{
	msg.enter("Tree::generate");
	// Push this tree branch onto the stack
// 	stack_.add(this);
	currentTree = this;
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	isFileSource_ = FALSE;
	int n = yyparse();
	printf("Result of yyparse = %i\n", n);
	msg.exit("Tree::generate");
}

// Execute tree
int Tree::execute(NuReturnValue &rv)
{
	msg.enter("Tree::execute");
	// Walk the individual nodes of the tree, which will set ReturnValue at each stage
	int result;
// 	for (TreeNode *node = nodes_.first(); node != NULL; node = node->TreeNode::next)
// 	{
		result = headNode_->execute(rv); 
// 	}
	// Copy current return value into supplied variable
	rv = returnValue_;
	printf("Final result type of tree execution is '%s'.\n", VTypes::dataType(rv.type()));
	msg.exit("Tree::execute");
	return result;
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

// Add simple leaf node (e.g. constant, variable) to topmost branch on stack
TreeNode *Tree::addLeaf(TreeNode *leaf)
{
// 	Refitem<Tree,int> *ri = stack_.last();
// 	if (ri == NULL) printf("Severe - no topmost branch on stack. A crash is coming!\n");
// 	Tree *topmost = ri->item;
// 	topmost->nodes_.own(leaf);
	nodeList_.add(leaf);
	return leaf;
}

// Add command-based leaf node to topmost branch on stack
TreeNode *Tree::addCommandLeaf(NuCommand::Function func)
{
// 	Refitem<Tree,int> *ri = stack_.last();
// 	if (ri == NULL) printf("Severe - no topmost branch on stack. A crash is coming!\n");
// 	Tree *topmost = ri->item;
	// Create the new command node
// 	printf("COMMAND ADDED.\n");
	NuCommandNode *leaf = new NuCommandNode(func);
	nodeList_.add(leaf);
	return leaf;
}

// Add joiner
TreeNode *Tree::addJoiner(TreeNode *node1, TreeNode *node2)
{
	printf("Adding a joiner...\n");
	NuCommandNode *leaf = new NuCommandNode(NuCommand::Joiner);
	nodeList_.add(leaf);
	if (node1 != NULL) leaf->addArgument(node1);
	if (node2 != NULL) leaf->addArgument(node2);
	return leaf;
}

// Set head node
void Tree::setHeadNode(TreeNode *node)
{
	// Check for previously-set headnode
	if (headNode_ != NULL) printf("Error - headnode has already been set.\n");
	else
	{
		headNode_ = node;
		printf("Head node set to %li\n", headNode_);
	}
}
