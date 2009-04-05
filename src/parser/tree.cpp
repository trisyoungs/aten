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
#include "parser/variablenode.h"
#include "parser/stepnode.h"
#include "parser/grammar.h"
#include "parser/parser.h"
#include "parser/tree.h"
#include "parser/vector.h"
#include "parser/character.h"
#include "parser/integer.h"
#include "parser/real.h"
#include "base/sysfunc.h"
#include "main/aten.h"
#include <stdarg.h>

// Constructor
Tree::Tree()
{
	// Private variables
	parent_ = NULL;
	declaredType_ = NuVTypes::NoData;
	declarationAssignment_ = FALSE;
	parser_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;

	// Initialise
	initialise();
}

// Destructor
Tree::~Tree()
{
	clear();
}

// Set parent
void Tree::setParent(Forest *f)
{
	parent_ = f;
}

// Return parent
Forest *Tree::parent()
{
	return parent_;
}

// Return whether this tree is a filter
bool Tree::isFilter()
{
	return (filter.type() != FilterData::nFilterTypes);
}

/*
// Create / Execute
*/

// Add read option
void Tree::addReadOption(LineParser::ParseOption po)
{
	if (!(readOptions_&po)) readOptions_ += po;
}

// Remove read option
void Tree::removeReadOption(LineParser::ParseOption po)
{
	if (readOptions_&po) readOptions_ -= po;
}

// Return read options
int Tree::readOptions()
{
	return readOptions_;
}

// Return parser object pointer
LineParser *Tree::parser()
{
	return parser_;
}


// Clear contents of tree
void Tree::clear()
{
	nodes_.clear();
	statements_.clear();
	scopeStack_.clear();
}

// (Re)Initialise Tree
void Tree::initialise()
{
	msg.enter("Tree::initialise");
	clear();
	// Store this as the current Tree (for Bison) and add a dummy ScopeNode to contain the main variable list
	ScopeNode *root = new ScopeNode(NuCommand::NoFunction);
	root->createGlobalVariables();
	nodes_.own(root);
	scopeStack_.add(root);
	statements_.add(root);
	msg.exit("Tree::initialise");
}

// Execute tree
bool Tree::execute(NuReturnValue &rv)
{
	msg.enter("Tree::execute");
	bool result;
	rv.reset();
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		msg.print(Messenger::Commands, "Executing tree statement %li...\n", ri->item);
// 		ri->item->nodePrint(1);
		result = ri->item->execute(rv);
		if (!result) break;
	}
	if (isFilter()) msg.print("Final result from execution of tree (in forest '%s') is %s\n", parent_->name(), rv.info());
	else msg.print("Final result from execution of %s filter (id = %i) tree '%s' (in forest '%s') is %s\n", FilterData::filterType(filter.type()), filter.id(), filter.name(), parent_->name(), rv.info());
	msg.exit("Tree::execute");
	return result;
}

// Execute tree after opening corresponding input stream
bool Tree::executeRead(LineParser *parser)
{
	msg.enter("Tree::executeRead[LineParser]");
	// Check LineParser
	parser_ = parser;
	if (parser_ == NULL)
	{
		msg.print("Error: NULL parsing source passed.\n");
		msg.exit("Tree::executeRead[LineParser]");
		return FALSE;
	}
	// Execute the commands
	NuReturnValue rv;
	bool result = execute(rv);
	parser_ = NULL;
	msg.exit("Tree::executeRead[LineParser]");
	return result;
}

// Execute, opening specified file as input source (no return value)
bool Tree::executeRead(const char *filename)
{
	msg.enter("Tree::executeRead[filename]");
	// Check for a previous parser pointer
	if (parser_ != NULL) printf("Warning: LineParser already defined in executeRead.\n");
	parser_ = new LineParser(filename);
	if (!parser_->isFileGood())
	{
		msg.exit("Tree::executeRead[filename]");
		return FALSE;
	}
	// Execute the commands
	NuReturnValue rv;
	bool result = execute(rv);
	parser_->closeFile();
	delete parser_;
	parser_ = NULL;
	msg.exit("Tree::executeRead[filename]");
}

// Execute, with specified filename as data target
bool Tree::executeWrite(const char *filename)
{
	msg.enter("Tree::executeWrite[filename]");
	// Check for a previous parser pointer
	if (parser_ != NULL) printf("Warning: LineParser already defined in executeWrite.\n");
	parser_ = new LineParser(filename, TRUE);
	if (!parser_->isFileGood())
	{
		msg.exit("Tree::executeWrite[filename]");
		return FALSE;
	}
	// Execute the commands
	NuReturnValue rv;
	bool result = execute(rv);
	parser_->closeFile();
	delete parser_;
	parser_ = NULL;
	msg.exit("Tree::executeWrite[filename]");
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
// Statements / Commands / Operators
*/

// Add a node representing a whole statement to the execution list
bool Tree::addStatement(TreeNode *leaf)
{
	if (leaf == NULL)
	{
		printf("Internal Error: NULL TreeNode passed to Tree::addStatement().\n");
		return FALSE;
	}
	msg.print(Messenger::Parse, "Added statement node %li\n", leaf);
	leaf->setParent(this);
	statements_.add(leaf);
	return TRUE;
}

// Add an operator to the Tree
TreeNode *Tree::addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2)
{
	msg.enter("Tree::addOperator");
	// Check compatibility between supplied nodes and the operator, since we didn't check the types in the lexer.
	NuVTypes::DataType rtype;
	if (arg2 == NULL) rtype = checkUnaryOperatorTypes(func, arg1->returnType());
	else rtype = checkBinaryOperatorTypes(func, arg1->returnType(), arg2->returnType());
	if (rtype == NuVTypes::NoData) return NULL;
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added operator '%s' (%li)...\n", aten.commands.data[func].keyword, leaf);
	// Add arguments and set parent
	leaf->addArguments(1,arg1);
	leaf->setParent(this);
	if (arg2 != NULL) leaf->addArguments(1,arg2);
	leaf->setReturnType(rtype);
	msg.exit("Tree::addOperator");
	return leaf;
}

// Add 'if' statement
TreeNode *Tree::addIf(TreeNode *condition, TreeNode *expr1, TreeNode *expr2)
{
	msg.enter("Tree::addIf");
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(NuCommand::If);
	nodes_.own(leaf);
	leaf->addArguments(2, condition, expr1);
	if (expr2 != NULL) leaf->addArgument(expr2);
	leaf->setParent(this);
	msg.print(Messenger::Parse, "'If' statement added (%li).\n", leaf);
	msg.exit("Tree::addIf");
	return leaf;
}

// Add 'for' statement
TreeNode *Tree::addFor(TreeNode *init, TreeNode *condition, TreeNode *action, TreeNode *statements)
{
	msg.enter("Tree::addFor");
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(NuCommand::For);
	nodes_.own(leaf);
	leaf->addArguments(4, init, condition, action, statements);
	leaf->setParent(this);
	msg.print(Messenger::Parse, "'For' statement added (%li, with init=%li, condition=%li, action=%li and statements=%li).\n", leaf, init, condition, action, statements);
	msg.exit("Tree::addFor");
	return leaf;
}

// Add function-based leaf node to topmost branch on stack
TreeNode *Tree::addFunction(NuCommand::Function func, TreeNode *arglist)
{
	msg.enter("Tree::addFunction");
	// Create new command node
	NuCommandNode *leaf = new NuCommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added function '%s' (%li)...\n", aten.commands.data[func].keyword, leaf);
	// Add argument list to node and set parent
	leaf->addArgumentList(arglist);
	leaf->setParent(this);
	// Store the function's return type
	leaf->setReturnType(NuCommand::data[func].returnType);
	// Check that the correct arguments were given to the command
	if (!leaf->checkArguments()) leaf = NULL;
	msg.exit("Tree::addFunction");
	return leaf;
}

// Link two arguments together with their member pointers
TreeNode *Tree::joinArguments(TreeNode *arg1, TreeNode *arg2)
{
	arg1->prevArgument = arg2;
	arg2->nextArgument = arg1;
	msg.print(Messenger::Parse, "Joining arguments %li and %li\n", arg1, arg2);
	return arg1;
}

// Join two commands together
TreeNode *Tree::joinCommands(TreeNode *node1, TreeNode *node2)
{
	msg.print(Messenger::Parse, "Joining command nodes %li and %li\n", node1, node2);
	NuCommandNode *leaf = new NuCommandNode(NuCommand::Joiner);
	nodes_.own(leaf);
	leaf->setParent(this);
	if (node1 != NULL) leaf->addArgument(node1);
	if (node2 != NULL) leaf->addArgument(node2);
	return leaf;
}

// Add on a new scope to the stack
TreeNode *Tree::pushScope()
{
	ScopeNode *node = new ScopeNode();
	nodes_.own(node);
	scopeStack_.add(node);
	msg.print(Messenger::Parse, "ScopeNode %li is pushed.\n", node);
	return node;
}

// Pop the topmost scope node
bool Tree::popScope()
{
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No scoped node to pop from stack.\n");
		return FALSE;
	}
	ScopeNode *temp = ri->item;
	scopeStack_.remove(ri);
	msg.print(Messenger::Parse, "ScopeNode %li is popped.\n", temp);
	return TRUE;
}

/*
// Variables / Constants
*/

// Add constant value to tompost scope
TreeNode *Tree::addConstant(NuVTypes::DataType type, Dnchar *token)
{
	NuVariable *result = NULL;
	if (type == NuVTypes::IntegerData)
	{
		NuIntegerVariable *var = new NuIntegerVariable(atoi(token->get()), TRUE);
		nodes_.own(var);
		return var;
	}
	else if (type == NuVTypes::RealData)
	{
		NuRealVariable *var = new NuRealVariable(atof(token->get()), TRUE);
		nodes_.own(var);
		return var;
	}
	else if (type == NuVTypes::StringData)
	{
		StringVariable *var = new StringVariable(token->get(), TRUE);
		nodes_.own(var);
		return var;
	}
	else printf("Internal Error: Don't know how to create a constant of type '%s' for Tree.\n", NuVTypes::dataType(type));
	return NULL;
}

// Set current declared variable type
bool Tree::setDeclaredVariableType(NuVTypes::DataType type)
{
	declaredType_ = type;
	return TRUE;
}

// Set declarations assignment flag
bool Tree::setDeclarationAssignment(bool b)
{
	declarationAssignment_ = b;
	return TRUE;
}

// Add variable to topmost scope
TreeNode *Tree::addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.print(Messenger::Parse, "A new variable '%s' is being created with type %s.\n", name->get(), NuVTypes::dataType(type));
	// Get topmost scopenode
// 	printf("nscope = %i, %li  %li\n", scopeStack_.nItems(), scopeStack_.first(), scopeStack_.last());
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No current scope in which to define variable '%s'.\n", name->get());
		return NULL;
	}
	// Create the supplied variable in the list of the topmost scope
	NuVariable *var = ri->item->variables.create(type, name->get(), initialValue);
	if (!var)
	{
		printf("Internal Error: Failed to create variable '%s' in local scope.\n", name->get());
		return NULL;
	}
	msg.print(Messenger::Parse, "Created variable '%s' in scopenode %li\n", name->get(), ri->item);
	return var;
}

// Add variable to topmost scope using most recently set declared variable type
TreeNode *Tree::addVariable(Dnchar *name, TreeNode *initialValue)
{
	return addVariable(declaredType_, name, initialValue);
}

// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *Tree::addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	msg.print(Messenger::Parse, "A new array variable '%s' is being created with type %s.\n", name->get(), NuVTypes::dataType(declaredType_));
	// Get topmost scopenode
// 	printf("nscope = %i, %li  %li\n", scopeStack_.nItems(), scopeStack_.first(), scopeStack_.last());
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No current scope in which to define array variable '%s'.\n", name->get());
		return NULL;
	}
	// Create the supplied variable in the list of the topmost scope
	NuVariable *var = ri->item->variables.createArray(declaredType_, name->get(), sizeexpr, initialvalue);
	if (!var)
	{
		printf("Internal Error: Failed to create array variable '%s' in local scope.\n", name->get());
		return NULL;
	}
	nodes_.own(var);
	printf("Created array variable '%s' in scopenode %li   %i\n", name->get(), ri->item, scopeStack_.nItems());
	return var;
}

// // Add constant value
// TreeNode *Tree::addVecConstant(NuVTypes::DataType type, TreeNode *value1, TreeNode *value2, TreeNode *value3)
// {
// 	NuVectorVariable *leaf = new NuVectorVariable(value1, value2, value3);
// 	scopeStack_.last()->item->variables.take(leaf);
// 	return leaf;
// }

// Search for variable in current scope
bool Tree::isVariableInScope(const char *name, NuVariable *&result)
{

	// If the declaredVariableType is set then this token has been found in a declaration statement.
	// ---> it must not exist in the local scope
	// In addition, if this is a declaration assignment, then we search as normal
	msg.print(Messenger::Parse, "Searching scope for variable '%s'...\n", name);
	if (declarationAssignment_ || (declaredType_ == NuVTypes::NoData))
	{
// 		printf("kljlk\n");
		// Search the current ScopeNode list for the variable name requested
		result = NULL;
		for (Refitem<ScopeNode,int> *ri = scopeStack_.last(); ri != NULL; ri = ri->prev)
		{
			msg.print(Messenger::Parse," ... scopenode %li...\n", ri->item);
			result = ri->item->variables.find(name);
			if (result != NULL) break;
		}
		// If result is NULL then we must return FALSE since the variable is not in a declaration
		// If the current declared variable type is NuVTypes::NoData then this is not a variable declaration and we must find the variable...
		if (result == NULL) msg.print(Messenger::Parse, "...variable '%s' not found in any scope.\n", name);
// 		{
// 			msg.print("Error: Variable '%s' has not been declared in the current scope.\n", name);
// 			return FALSE;
// 		}
		return TRUE;
	}
	else
	{
// 	printf("kljlk23423432\n");
		// So, we are declaring a variable in the local scope, which may shadow another in the global scope.
		// We are only concerned with whether this variable currently exists in the local scope...
// 	printf("searching scopenode %li...\n", scopeStack_.last());
		msg.print(Messenger::Parse, "Searching topmost scope %li for existence of new variable '%s'...\n", scopeStack_.last()->item, name);
		result = scopeStack_.last()->item->variables.find(name);
		if (result)
		{
			msg.print("Error: Repeat declaration of variable '%s' in the local scope.\n", name);
			return FALSE;
		}
		return TRUE;
	}
	return FALSE;
}

// Wrap named variable (and array index)
TreeNode *Tree::wrapVariable(NuVariable *var, TreeNode *arrayindex)
{
	// If an array index was given, check that the target variable is actually an array....
	if (arrayindex && (var->nodeType() != TreeNode::ArrayVarNode))
	{
		msg.print("Error: Array index given to variable '%s', but it is not an array.\n", var->name());
		return NULL;
	}
	VariableNode *vnode = new VariableNode(var);
	nodes_.own(vnode);
	vnode->setArrayIndex(arrayindex);
	vnode->setParent(this);
	return vnode;
}

/*
// Paths
*/

// Create a new path on the stack
TreeNode *Tree::createPath(TreeNode *node)
{
	msg.enter("Tree::createPath");
	VariableNode *vnode = (VariableNode*) node;
	pathStack_.add(vnode, vnode);
// 	nodes_.own(vnode);	// Should not be called, since the passed *node is already owned by the tree
	msg.print(Messenger::Parse, "A new path has been started, beginning from variable '%s'.\n", vnode->name());
	msg.exit("Tree::createPath");
	return vnode;
}

// Pop topmost path from stack
TreeNode *Tree::finalisePath()
{
	msg.enter("Tree::finalisePath");
	// Finalise the path before we remove it
	Refitem<VariableNode,TreeNode*> *ri = pathStack_.last();
	if (ri == NULL)
	{
		msg.print("Internal Error: No path on stack to finalise.\n");
		return NULL;
	}
	ri->item->finalisePath();
	TreeNode *result = ri->item;
	pathStack_.remove(ri);
	msg.exit("Tree::finalisePath");
	return result;
}

// Expand the topmost path on the stack
bool Tree::expandPath(Dnchar *name, TreeNode *arrayindex)
{
	msg.enter("Tree::expandPath");
	// Get last item on path stack
	Refitem<VariableNode,TreeNode*> *ri = pathStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No path on stack to expand with accessor '%s'.\n", name->get());
		return NULL;
	}
	TreeNode *laststep = ri->data;
	msg.print(Messenger::Parse,"Tree is evaluating accessor '%s' as step %i from the basenode '%s'...\n", name->get(), ri->item->nArgs()+1, ri->item->name());
	// Find next step accessor
	StepNode *result = laststep->findAccessor(name->get(), arrayindex != NULL);
	// If we found a valid accessor, update the pathstack entry
	if (result)
	{
		msg.print(Messenger::Parse,"...OK - matching accessor found: return type is %s\n", NuVTypes::dataType(result->returnType()));
		ri->data = (TreeNode*) result;
		nodes_.own(result);
		// Finalise the path before we remove it
		Refitem<VariableNode,TreeNode*> *ri = pathStack_.last();
		if (ri == NULL)
		{
			msg.print("Internal Error: No path on stack to expand!\n");
			msg.exit("Tree::expandPath");
			return FALSE;
		}
		ri->item->addArgument(result);
	}
	else msg.print("Error: Object of type '%s' has no matching accessor for '%s'.\n", NuVTypes::dataType(laststep->returnType()), name->get());
	msg.exit("Tree::expandPath");
	return result;
}
