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

#include "parser/treenode.h"
#include "parser/scopenode.h"
#include "parser/commandnode.h"
#include "parser/grammar.h"
#include "parser/tree.h"
#include "parser/vector.h"
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
	statements_.add(root);
	// Store the source string
	stringSource_ = s;
	stringPos_ = 0;
	stringLength_ = stringSource_.length();
	isFileSource_ = FALSE;
	nErrors_ = 0;
	int result = yyparse();
	currentTree = NULL;
	if ((result != 0) || (nErrors_ != 0))
	{
		// Delete any tree node information
		clear();
		msg.print("Failed to parse data.\n");
		msg.exit("Tree::generate");
		return FALSE;		
	}
	else print();
	msg.exit("Tree::generate");
	return TRUE;
}

// Execute tree
bool Tree::execute(NuReturnValue &rv)
{
	msg.enter("Tree::execute");
	bool result;
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		result = ri->item->execute(rv);
		if (!result) break;
	}
	printf("Final result of tree execution:\n");
	rv.info();
	msg.exit("Tree::execute");
	return result;
}

// Print tree
void Tree::print()
{
	printf("Leaf Structure (%i statements):\n", statements_.nItems());
	int n=1;
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		printf("-------------------------------------------------------------\n");
		printf("Statement %i:\n", n);
		printf("item pointer is %li\n", ri->item);
		ri->item->nodePrint(1);
		n ++;
	}
	printf("-------------------------------------------------------------\n");
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
		if (stringPos_ == stringLength_) return '\0';
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
	if (leaf == NULL)
	{
		printf("Internal Error: NULL TreeNode passed to Tree::addStatement().\n");
		return;
	}
	printf("Added statement leaf node %li\n", leaf);
	statements_.add(leaf);
}

// Add an operator to the Tree
TreeNode *Tree::addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2)
{
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(func);
	ownedNodes_.add(leaf);
	// Add arguments
	leaf->addArguments(arg1);
	if (arg2 != NULL) leaf->addArguments(arg2);
	// Store return type - if we were passed 1 or 2, store the return type of this argument
	// If we were passed 99, it is a logical operator and should return an integer
	// If we were passed 0, assume its a number and work out which number type we actually return
	if (typearg == 1) leaf->setReturnType(arg1->returnType());
	else if (typearg == 2) leaf->setReturnType(arg2->returnType());
	else if (typearg == 99) leaf->setReturnType(NuVTypes::IntegerData);
	else
	{
		if (arg2 == NULL) leaf->setReturnType(arg1->returnType());
		else if (arg1->returnType() == arg2->returnType()) leaf->setReturnType(arg1->returnType());
		else leaf->setReturnType(NuVTypes::RealData);
	}
	return leaf;
}

// Add command-based leaf node to topmost branch on stack
TreeNode *Tree::addCommandLeaf(NuCommand::Function func, int nargs, ...)
{
	msg.enter("Tree::addCommandLeaf");
	// Create variable argument parser
	va_list vars;
	va_start(vars,nargs);
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(func);
	ownedNodes_.add(leaf);
	// Add arguments
	for (int n=0; n<nargs; n++) leaf->addArguments(va_arg(vars, TreeNode*));
	va_end(vars);
	// Store the function's return type
	leaf->setReturnType(NuCommand::data[func].returnType);
	// Check that the correct arguments were given to the command
	printf("The function leaf is %li, containing funcid %i, and has %i arguments\n", leaf, func, leaf->nArgs());
	const char *c = NuCommand::data[func].arguments;
	char upc;
	int count = 0, ngroup;
	bool optional, requirevar, failed, cluster = FALSE;
	NuVTypes::DataType rtype;
	do
	{
		// Retain last character if this is a repeat
		if (*c != '*')
		{
			// If the character is '^', then we get the next char and set the requirevar flag
			// If it is '[' or ']' then set the cluster flag and get the next char
			requirevar = FALSE;
			if (*c == '^')
			{
				requirevar = TRUE;
				c++;
			}
			else if ((*c == '[') || (*c == ']'))
			{
				cluster = (*c == '[');
				ngroup = 0;
				c++;
			}
			// Get character and convert to upper case if necessary
			if ((*c > 96) && (*c < 123))
			{
				upc = *c - 32;
				optional = TRUE;
			}
			else
			{
				upc = *c;
				optional = FALSE;
			}
		}
		else optional = TRUE;
		printf("The next argument token is '%c'\n", upc);
		// If we have reached the end of the argument specification, do we still have arguments left in the command?
		if (upc == '\0')
		{
			if (leaf->nArgs() > count)
			{
				msg.print("Error: %i extra arguments given to function '%s' (syntax is '%s %s').\n", leaf->nArgs()-count, NuCommand::data[func].keyword, NuCommand::data[func].keyword, NuCommand::data[func].argText);
				nErrors_ ++;
				msg.exit("Tree::addCommandLeaf");
				return leaf;
			}
			else
			{
				msg.enter("Tree::addCommandLeaf");
				return leaf;
			}
		}
		// If we have gone over the number of arguments provided, is this an optional argument?
		if (count >= leaf->nArgs())
		{
			if (!optional)
			{
				msg.print("Error: The function '%s' requires argument %i.\n", NuCommand::data[func].keyword, count+1);
				msg.print("       Command syntax is '%s %s'.\n", NuCommand::data[func].keyword, NuCommand::data[func].argText);
				nErrors_ ++;
				msg.exit("Tree::addCommandLeaf");
				return leaf;
			}
			else if (cluster && (ngroup != 0))
			{
				msg.print("Error: The optional argument %i to function '%s' is part of a group and must be specified.\n", count+1, NuCommand::data[func].keyword);
				msg.print("       Command syntax is '%s %s'.\n", NuCommand::data[func].keyword, NuCommand::data[func].argText);
				nErrors_ ++;
				msg.exit("Tree::addCommandLeaf");
				return leaf;
			}
			else
			{
				msg.exit("Tree::addCommandLeaf");
				return leaf;
			}
		}
		// Check argument type
		rtype = leaf->argType(count);
		failed = FALSE;
		switch (upc)
		{
			// Number		(IntegerData, RealData)
			case ('N'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::RealData))
				{
					msg.print("Argument %i to command '%s' must be a number.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Character		(CharacterData)
			case ('C'):
				if (rtype != NuVTypes::CharacterData)
				{
					msg.print("Argument %i to command '%s' must be a character string.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;	
			// Any Simple		(IntegerData, RealData, CharacterData)
			case ('S'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::RealData) && (rtype != NuVTypes::CharacterData))
				{
					msg.print("Argument %i to command '%s' must be a number or a character string.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Boolean		(Any Except NoData)
			case ('B'):
				if (rtype == NuVTypes::NoData)
				{
					msg.print("Argument %i to command '%s' must return something!\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Atom/Id		(IntegerData, AtomData)
			case ('A'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::AtomData))
				{
					msg.print("Argument %i to command '%s' must be an integer or an atom&.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Model/ID/Name	(ModelData, CharacterData, IntegerData)
			case ('M'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::ModelData) && (rtype != NuVTypes::CharacterData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a model& or a character string.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Pattern/ID/Name	(PatternData, CharacterData, IntegerData)
			case ('P'):
				if ((rtype != NuVTypes::IntegerData) && (rtype != NuVTypes::PatternData) && (rtype != NuVTypes::CharacterData))
				{
					msg.print("Argument %i to command '%s' must be an integer, a pattern& or a character string.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
			// Pointer		(Any pointer (void*) object)
			case ('V'):
				if (rtype < NuVTypes::AtomData)
				{
					msg.print("Argument %i to command '%s' must be a reference of some kind.\n", count+1, NuCommand::data[func].keyword);
					failed = TRUE;
				}
				break;
		}
		// Check for failure
		if (failed)
		{
			msg.exit("Tree::addCommandLeaf");
			nErrors_ ++;
			return leaf;
		}
		if (upc != '*') c++;
		if (cluster) ngroup++;
		count++;
	} while (*c != '\0');
	msg.exit("Tree::addCommandLeaf");
	return leaf;
}

// Add an argument to the most recently pushed function on the stack
TreeNode *Tree::joinArguments(TreeNode *arg1, TreeNode *arg2)
{
	arg1->prevArgument = arg2;
	arg2->nextArgument = arg1;
	printf("Joining arguments %li and %li\n", arg1, arg2);
	return arg1;
}

// Add joiner
TreeNode *Tree::addJoiner(TreeNode *node1, TreeNode *node2)
{
	printf("Adding a statement joiner for %li and %li\n", node1, node2);
	NuCommandNode *leaf = new NuCommandNode(NuCommand::Joiner);
	ownedNodes_.add(leaf);
	if (node1 != NULL) leaf->addArguments(node1);
	if (node2 != NULL) leaf->addArguments(node2);
	return leaf;
}

// Add constane to topmost ScopeNode
void Tree::addConstant(NuVariable *v)
{
	scopeNodes_.last()->item->variables.take(v);
}

// Add variable to topmost scope
bool Tree::addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	printf("Adding a variable called %s\n", name->get());
	// Create the supplied variable in the list of the topmost scope
	if (!scopeNodes_.last()->item->variables.create(type, name->get(), initialValue))
	{
		printf("ERROR!\n");
		return FALSE;
	}
	// Create a placeholder node with no function
	NuCommandNode *leaf = new NuCommandNode(NuCommand::Declarations);
	ownedNodes_.add(leaf);
	return TRUE;
}

// Add constant value
TreeNode *Tree::addVecConstant(NuVTypes::DataType type, TreeNode *value1, TreeNode *value2, TreeNode *value3)
{
	NuVectorVariable *leaf = new NuVectorVariable(value1, value2, value3);
	scopeNodes_.last()->item->variables.take(leaf);
	return leaf;
}

// Search for variable in current scope
NuVariable *Tree::isVariableInScope(const char *name)
{
	// Search the current ScopeNode list for the variable name requested
	NuVariable *v = NULL;
	for (Refitem<ScopeNode,int> *ri = scopeNodes_.last(); ri != NULL; ri =ri->prev)
	{
		v = ri->item->variables.find(name);
		if (v != NULL) break;
	}
	return v;
}
