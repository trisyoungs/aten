/*
	*** Tree
	*** src/parser/tree.cpp
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

#include "parser/treenode.h"
#include "parser/scopenode.h"
#include "parser/commandnode.h"
#include "parser/usercommandnode.h"
#include "parser/variablenode.h"
#include "parser/stepnode.h"
#include "parser/widgetnode.h"
#include "parser/grammar.h"
#include "parser/parser.h"
#include "parser/tree.h"
#include "parser/character.h"
#include "parser/vector.h"
#include "parser/element.h"
#include "parser/integer.h"
#include "parser/double.h"
#include "model/model.h"
#include "base/sysfunc.h"
#include "classes/prefs.h"
#include "main/aten.h"
#include <stdarg.h>

// Constructor
Tree::Tree()
{
	// Private variables
	parent_ = NULL;
	parser_ = NULL;
	acceptedFail_ = Command::NoFunction;
	name_ = "unnamed";
	type_ = Tree::UnknownTree;
	readOptions_ = LineParser::Defaults;
	mainWidget_ = NULL;

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
Forest *Tree::parent() const
{
	return parent_;
}

// Set type
void Tree::setType(Tree::TreeType type)
{
	type_ = type;
}

// Return type
Tree::TreeType Tree::type() const
{
	return type_;
}

// Set name of tree
void Tree::setName(const char *s)
{
	name_ = s;
}

// Return name of tree
const char *Tree::name() const
{
	return name_.get();
}

// Set return type of tree
void Tree::setReturnType(VTypes::DataType dt)
{
	returnType_ = dt;
}

// Return return-type of tree
VTypes::DataType Tree::returnType() const
{
	return returnType_;
}

// Return whether this tree is a filter
bool Tree::isFilter() const
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
int Tree::readOptions() const
{
	return readOptions_;
}

// Return parser object pointer
LineParser *Tree::parser()
{
	return parser_;
}

// Return whether the LineParser is ready for file reading
bool Tree::isFileGoodForReading() const
{
	return (parser_ == NULL ? FALSE : parser_->isFileGoodForReading());
}

// Return whether the LineParser is ready for file writing
bool Tree::isFileGoodForWriting() const
{
	return (parser_ == NULL ? FALSE : parser_->isFileGoodForWriting());
}

// Clear contents of tree
void Tree::clear()
{
	nodes_.clear();
	statements_.clear();
	scopeStack_.clear();
	widgets_.clear();
// 	if (mainWidget_ != NULL) delete mainWidget_;
// 	mainWidget_ = NULL;
}

// (Re)Initialise Tree
void Tree::initialise()
{
	msg.enter("Tree::initialise");
	clear();
	// Add a dummy ScopeNode to contain the main variable list
	ScopeNode *root = new ScopeNode(Command::NoFunction);
	root->setParent(this);
	root->createGlobalVariables();
	nodes_.own(root);
	scopeStack_.add(root);
	statements_.add(root);
	msg.exit("Tree::initialise");
}

// Set function for accepted fail
void Tree::setAcceptedFail(Command::Function func)
{
	if ((acceptedFail_ != Command::NoFunction) && (func != Command::NoFunction)) printf("Warning: An acceptedFail command is already set...\n");
	acceptedFail_ = func;
}

// Clear accepted fail bit
Command::Function Tree::acceptedFail() const
{
	return acceptedFail_;
}

// Execute tree
bool Tree::execute(ReturnValue &rv)
{
	msg.enter("Tree::execute");
	bool result = FALSE;
	rv.reset();
	ElementMap::ZMapType zm = ElementMap::nZMapTypes;
	acceptedFail_ = Command::NoFunction;
	// Perform any preparatory commands related to filter trees
	if (isFilter())
	{
		// For all filters, store the current zmapping style in Prefs so we can revert to it later
		zm = prefs.zMapType();
		switch (filter.type())
		{
			case (FilterData::ExpressionExport):
				// Turn on export type mapping
				if (aten.typeExportMap.nPairs() != 0) aten.setTypeExportMapping(TRUE);
				// Create expression for model
				if (!aten.current.m->createExpression())
				{
					msg.exit("Tree::execute");
					return FALSE;
				}
				break;
			case (FilterData::ModelExport):
				// Turn on export type mapping
				if (aten.typeExportMap.nPairs() != 0) aten.setTypeExportMapping(TRUE);
				break;
			default:
				break;
		}
	}
	for (Refitem<TreeNode,int> *ri = statements_.first(); ri != NULL; ri = ri->next)
	{
		msg.print(Messenger::Commands, "Executing tree statement %p...\n", ri->item);
// 		ri->item->nodePrint(1);
		result = ri->item->execute(rv);
		// Catch failures arising from 'return' statements
		if (acceptedFail_ == Command::Return)
		{
			msg.print(Messenger::Parse, "Execution of tree ended early because we returned.\n");
			result = TRUE;
			break;
		}
		else if (acceptedFail_ == Command::Quit)
		{
			result = TRUE;
			break;
		}
		if (!result) break;
	}
	// Perform any finalisation commands related to filter trees
	if (isFilter())
	{
		// For all filters, restore the previous zmapping style
		prefs.setZMapType(zm);
		switch (filter.type())
		{
			case (FilterData::ExpressionExport):
			case (FilterData::ModelExport):
				// Turn off export type mapping
				aten.setTypeExportMapping(FALSE);
				break;
			default:
				break;
		}
	}
	if (isFilter()) msg.print(Messenger::Parse, "Final result from execution of %s filter (id = %i) tree '%s' (in forest '%s') is %s\n", FilterData::filterType(filter.type()), filter.id(), filter.name(), parent_->name(), rv.info());
	else msg.print(Messenger::Parse, "Final result from execution of tree '%s' (in forest '%s') is %s\n", name_.get(), parent_->name(), rv.info());
	if (!result) msg.print(Messenger::Parse, "Execution FAILED.\n");
	msg.exit("Tree::execute");
	return result;
}

// Execute tree using provided parsing source
bool Tree::execute(LineParser *parser, ReturnValue &rv)
{
	msg.enter("Tree::execute[LineParser]");
	// Check LineParser
	parser_ = parser;
	if (parser_ == NULL)
	{
		msg.print("Error: NULL parsing source passed.\n");
		msg.exit("Tree::execute[LineParser]");
		return FALSE;
	}
	// Execute the commands
	bool result = execute(rv);
	parser_ = NULL;
	msg.exit("Tree::execute[LineParser]");
	return result;
}

// Execute, opening specified file as input source (no return value)
bool Tree::executeRead(const char *filename, ReturnValue &rv)
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
	bool result = execute(rv);
	parser_->closeFile();
	delete parser_;
	parser_ = NULL;
	msg.exit("Tree::executeRead[filename]");
	return result;
}

// Execute, with specified filename as data target
bool Tree::executeWrite(const char *filename, ReturnValue &rv)
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
	bool result = execute(rv);
	parser_->closeFile();
	delete parser_;
	parser_ = NULL;
	msg.exit("Tree::executeWrite[filename]");
	return result;
}

// Execute, opening specified file as input source (no return value)
bool Tree::executeRead(const char *filename)
{
	ReturnValue rv;
	return executeRead(filename, rv);
}

// Execute, with specified filename as data target (no return value)
bool Tree::executeWrite(const char *filename)
{
	ReturnValue rv;
	return executeWrite(filename, rv);
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
		printf("item pointer is %p\n", ri->item);
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
	msg.print(Messenger::Parse, "Added statement node %p\n", leaf);
	leaf->setParent(this);
	statements_.add(leaf);
	return TRUE;
}

// Add an operator to the Tree
TreeNode *Tree::addOperator(Command::Function func, TreeNode *arg1, TreeNode *arg2)
{
	msg.enter("Tree::addOperator");
	// Check compatibility between supplied nodes and the operator, since we didn't check the types in the lexer.
	VTypes::DataType rtype;
	bool returnsarray;
	if (arg2 == NULL) rtype = checkUnaryOperatorTypes(func, arg1->returnType(), arg1->returnsArray(), returnsarray);
	else rtype = checkBinaryOperatorTypes(func, arg1->returnType(), arg1->returnsArray(), arg2->returnType(), arg2->returnsArray(), returnsarray);
	if (rtype == VTypes::NoData) return NULL;
	// Create new command node
	CommandNode *leaf = new CommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added operator '%s' (%p)...\n", Command::data[func].keyword, leaf);
	// Add arguments and set parent
	leaf->addArguments(1,arg1);
	leaf->setParent(this);
	if (arg2 != NULL) leaf->addArguments(1,arg2);
	leaf->setReturnType(rtype);
	leaf->setReturnsArray(returnsarray);
	msg.exit("Tree::addOperator");
	return leaf;
}

// Add function-based leaf node to topmost branch on stack
TreeNode *Tree::addFunctionWithArglist(Command::Function func, TreeNode *arglist)
{
	msg.enter("Tree::addFunctionWithArglist");
	// Create new command node
	CommandNode *leaf = new CommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added function '%s' (%p)...\n", Command::data[func].keyword, leaf);
	// Add argument list to node and set parent
	leaf->addJoinedArguments(arglist);
	leaf->setParent(this);
	// Store the function's return type
	leaf->setReturnType(Command::data[func].returnType);
	// Check that the correct arguments were given to the command and run any prep functions
	if (!leaf->checkArguments(Command::data[func].arguments, Command::data[func].keyword))
	{
		msg.print("Error: Function syntax is '%s(%s)'.\n", Command::data[func].keyword, Command::data[func].argText);
		leaf = NULL;
	}
	else if (!leaf->prepFunction()) leaf = NULL;
	msg.exit("Tree::addFunctionWithArglist");
	return leaf;
}

// Add a function node to the list (overloaded to accept simple arguments instead of a list)
TreeNode *Tree::addFunction(Command::Function func, TreeNode *a1, TreeNode *a2, TreeNode *a3, TreeNode *a4)
{
	msg.enter("Tree::addFunction");
	// Create new command node
	CommandNode *leaf = new CommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added function '%s' (%p)...\n", Command::data[func].keyword, leaf);
	if (a1 != NULL) leaf->addArgument(a1);
	if (a2 != NULL) leaf->addArgument(a2);
	if (a3 != NULL) leaf->addArgument(a3);
	if (a4 != NULL) leaf->addArgument(a4);
	leaf->setParent(this);
	// Store the function's return type
	leaf->setReturnType(Command::data[func].returnType);
	// Check that the correct arguments were given to the command and run any prep functions
	if (!leaf->checkArguments(Command::data[func].arguments, Command::data[func].keyword))
	{
		msg.print("Error: Function syntax is '%s(%s)'.\n", Command::data[func].keyword, Command::data[func].argText);
		leaf = NULL;
	}
	else if (!leaf->prepFunction()) leaf = NULL;
	msg.exit("Tree::addFunction");
	return leaf;
}

// Add user-defined function-based leaf node to topmost branch on stack
TreeNode *Tree::addUserFunction(Tree *func, TreeNode *arglist)
{
	msg.enter("Tree::addUserFunction");
	// Create new command node
	UserCommandNode *leaf = new UserCommandNode(func);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added user function '%s' (%p)...\n", func->name(), leaf);
	// Add argument list to node and set parent
	leaf->addJoinedArguments(arglist);
	leaf->setParent(this);
	// Store the function's return type
	leaf->setReturnType(func->returnType());
	// Check that the correct arguments were given to the command and run any prep functions
	if (!leaf->checkArguments()) leaf = NULL;
	msg.exit("Tree::addUserFunction");
	return leaf;
}

// Add a declaration list
TreeNode *Tree::addDeclarations(TreeNode *declist)
{
	msg.enter("Tree::addDeclarations");
	// Create new command node
	CommandNode *leaf = new CommandNode(Command::Declarations);
	nodes_.own(leaf);
	msg.print(Messenger::Parse, "Added declarations node (%p)...\n", leaf);
	// Add argument list to node and set parent
	leaf->addJoinedArguments(declist);
	leaf->setParent(this);
	// Check that the correct arguments were given to the command and run any prep functions
	if (!leaf->checkArguments(Command::data[Command::Declarations].arguments, Command::data[Command::Declarations].keyword)) leaf = NULL;
	msg.exit("Tree::addDeclarations");
	return leaf;
}

// Link two arguments together with their member pointers
TreeNode *Tree::joinArguments(TreeNode *arg1, TreeNode *arg2)
{
	arg1->prevArgument = arg2;
	arg2->nextArgument = arg1;
	msg.print(Messenger::Parse, "Joining arguments %p and %p\n", arg1, arg2);
	return arg1;
}

// Join two commands together
TreeNode *Tree::joinCommands(TreeNode *node1, TreeNode *node2)
{
	CommandNode *leaf = new CommandNode(Command::Joiner);
	nodes_.own(leaf);
	leaf->setParent(this);
	if (node1 != NULL) leaf->addArgument(node1);
	if (node2 != NULL) leaf->addArgument(node2);
	msg.print(Messenger::Parse, "Joined command nodes %p and %p (joiner node is %p)\n", node1, node2, leaf);
	return leaf;
}

// Add on a new scope to the stack
TreeNode *Tree::pushScope(Command::Function func)
{
	ScopeNode *node = new ScopeNode();
	nodes_.own(node);
	scopeStack_.add(node,func);
	msg.print(Messenger::Parse, "ScopeNode %p is pushed.\n", node);
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
	msg.print(Messenger::Parse, "ScopeNode %p is popped.\n", temp);
	return TRUE;
}

/*
// Variables / Constants
*/

// Add constant value to tompost scope
TreeNode *Tree::addConstant(VTypes::DataType type, Dnchar *token)
{
	if (type == VTypes::IntegerData)
	{
		IntegerVariable *var = new IntegerVariable(atoi(token->get()), TRUE);
		nodes_.own(var);
		return var;
	}
	else if (type == VTypes::DoubleData)
	{
		DoubleVariable *var = new DoubleVariable(atof(token->get()), TRUE);
		nodes_.own(var);
		return var;
	}
	else if (type == VTypes::StringData)
	{
		StringVariable *var = new StringVariable(token->get(), TRUE);
		nodes_.own(var);
		return var;
	}
	else printf("Internal Error: Don't know how to create a constant of type '%s' for Tree.\n", VTypes::dataType(type));
	return NULL;
}

// Add integer constant
TreeNode *Tree::addConstant(int i)
{
	IntegerVariable *var = new IntegerVariable(i, TRUE);
	nodes_.own(var);
	return var;
}

// Add double constant
TreeNode *Tree::addConstant(double d)
{
	DoubleVariable *var = new DoubleVariable(d, TRUE);
	nodes_.own(var);
	return var;
}

// Add string constant
TreeNode *Tree::addConstant(const char *s)
{
	StringVariable *var = new StringVariable(s, TRUE);
	nodes_.own(var);
	return var;
}

// Add Element constant
TreeNode *Tree::addElementConstant(int el)
{
	ElementVariable *var;
	if ((el < 1) || (el > elements().nElements())) var = new ElementVariable(NULL,TRUE);
	else var = new ElementVariable(&elements().el[el], TRUE);
	nodes_.own(var);
	return var;
}

// Add variable to topmost scope
TreeNode *Tree::addVariable(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.print(Messenger::Parse, "A new variable '%s' is being created with type %s.\n", name->get(), VTypes::dataType(type));
	// Get topmost scopenode
// 	printf("nscope = %i, %p  %p\n", scopeStack_.nItems(), scopeStack_.first(), scopeStack_.last());
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No current scope in which to define variable '%s'.\n", name->get());
		return NULL;
	}
	// Check initialvalue....
	if ((initialValue != NULL) && (type != VTypes::VectorData))
	{
		if ((initialValue->nodeType() == TreeNode::ArrayVarNode) || (initialValue->nodeType() == TreeNode::ArrayConstantNode))
		{
			msg.print("Error: A non-array variable cannot be initialised from an array.\n");
			return NULL;
		}
	}
	// Create the supplied variable in the list of the topmost scope
	Variable *var = ri->item->variables.create(type, name->get(), initialValue);
	if (!var)
	{
// 		printf("Failed to create variable '%s' in local scope.\n", name->get());
		return NULL;
	}
	msg.print(Messenger::Parse, "Created variable '%s' in scopenode %p\n", name->get(), ri->item);
	return var;
}

// Add variable as function argument to topmost scope
TreeNode *Tree::addVariableAsArgument(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.print(Messenger::Parse, "A new variable '%s' is being created with type %s.\n", name->get(), VTypes::dataType(type));
	if (type_ != Tree::FunctionTree)
	{
		printf("Internal Error: Target tree is not a function.\n");
		return NULL;
	}
	// Get topmost scopenode
// 	printf("nscope = %i, %p  %p\n", scopeStack_.nItems(), scopeStack_.first(), scopeStack_.last());
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No current scope in which to define variable '%s'.\n", name->get());
		return NULL;
	}
	// Create the supplied variable in the list of the topmost scope
	Variable *var = ri->item->variables.create(type, name->get(), initialValue);
	if (!var)
	{
// 		printf("Failed to create variable '%s' in local scope.\n", name->get());
		return NULL;
	}
	msg.print(Messenger::Parse, "Created variable '%s' in scopenode %p\n", name->get(), ri->item);
	// Wrap the variable and add it to the arguments_ list
	VariableNode *vnode = new VariableNode(var);
	arguments_.own(vnode);
	vnode->setParent(this);
	return vnode;
}

// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *Tree::addArrayVariable(VTypes::DataType type, Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	msg.print(Messenger::Parse, "A new array variable '%s' is being created with type %s.\n", name->get(), VTypes::dataType(type));
	// Get topmost scopenode
// 	printf("nscope = %i, %p  %p\n", scopeStack_.nItems(), scopeStack_.first(), scopeStack_.last());
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No current scope in which to define array variable '%s'.\n", name->get());
		return NULL;
	}
	// Create the supplied variable in the list of the topmost scope
	Variable *var = ri->item->variables.createArray(type, name->get(), sizeexpr, initialvalue);
	if (!var)
	{
		printf("Internal Error: Failed to create array variable '%s' in local scope.\n", name->get());
		return NULL;
	}
// 	printf("Created array variable '%s' in scopenode %p   %i\n", name->get(), ri->item, scopeStack_.nItems());
	return var;
}

// Add constant vector
TreeNode *Tree::addArrayConstant(TreeNode *values)
{
	Refitem<ScopeNode,int> *ri = scopeStack_.last();
	// Determine numbers of each type in array
	TreeNode *first;
	bool baddata = FALSE;
	int nints = 0, ndoubles = 0, nstrings = 0, npointers = 0, nvalues = 0;
	VTypes::DataType dt = VTypes::NoData;
	for (first = values; first != NULL; first = first->prevArgument)
	{
		++nvalues;
		switch (first->returnType())
		{
			case (VTypes::IntegerData):
				++nints;
				if (nstrings+npointers > 0) baddata = TRUE;
				break;
			case (VTypes::DoubleData):
				++ndoubles;
				if (nstrings+npointers > 0) baddata = TRUE;
				break;
			case (VTypes::StringData):
				++nstrings;
				if (nints+ndoubles+npointers > 0) baddata = TRUE;
				break;
			default:
				++npointers;
				if (nints+ndoubles+nstrings > 0) baddata = TRUE;
				if ((dt != VTypes::NoData) && (dt != first->returnType())) baddata = TRUE;
				dt = first->returnType();
				break;
		}
		if (baddata) break;
		if (first->prevArgument == NULL) break;
	}
	// Check for bad data in array specification
	if (baddata)
	{
		msg.print("Error: Incompatible mixture of data types found in array declaration.\n");
		return NULL;
	}
	// Type of array will be 'highest' type that we found
	if (npointers > 0) dt = values->returnType();
	else if (nstrings > 0) dt = VTypes::StringData;
	else if (ndoubles > 0) dt = VTypes::DoubleData;
	else dt = VTypes::IntegerData;
	Variable *var = ri->item->variables.createArrayConstant(dt, nvalues);
	var->setParent(this);
	var->addJoinedArguments(values);
	nodes_.own(var);
	return var;
}

// Search for variable in current scope
Variable *Tree::findVariableInScope(const char *name, int &scopelevel)
{
	Variable *result = NULL;
	scopelevel = 0;
	msg.print(Messenger::Parse, "Searching scope for variable '%s'...\n", name);
	// Search the current ScopeNode list for the variable name requested
	for (Refitem<ScopeNode,int> *ri = scopeStack_.last(); ri != NULL; ri = ri->prev)
	{
		msg.print(Messenger::Parse," ... scopenode %p...\n", ri->item);
		result = ri->item->variables.find(name);
		if (result != NULL) break;
		scopelevel --;
	}
	if (result == NULL)
	{
		result = aten.findPassedValue(name);
		if (result == NULL) msg.print(Messenger::Parse, "...variable '%s' not found in any scope.\n", name);
		else
		{
			scopelevel = -99;
			msg.print(Messenger::Parse, "...variable '%s' found as a passed value.\n", name);
		}
	}
	else msg.print(Messenger::Parse, "...variable '%s' found at a scope level of %i.\n", name, scopelevel);
	return result;
}

// Wrap named variable (and array index)
TreeNode *Tree::wrapVariable(Variable *var, TreeNode *arrayindex)
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
	if ((arrayindex == NULL) && (var->nodeType() == TreeNode::ArrayVarNode)) vnode->setReturnsArray(TRUE);
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
	msg.print(Messenger::Parse, "Path beginning from variable '%s' has been finalised.\n", ri->item->name());
	pathStack_.remove(ri);
	msg.exit("Tree::finalisePath");
	return result;
}

// Expand the topmost path on the stack
bool Tree::expandPath(Dnchar *name, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("Tree::expandPath");
	// Get last item on path stack
	Refitem<VariableNode,TreeNode*> *ri = pathStack_.last();
	if (ri == NULL)
	{
		printf("Internal Error: No path on stack to expand with accessor '%s'.\n", name->get());
		return FALSE;
	}
	msg.print(Messenger::Parse,"Tree is evaluating accessor '%s' as step %i from the basenode '%s'...\n", name->get(), ri->item->nArgs()+1, ri->item->name());
	// If the last step was an array and an array index was not give, we complain!
	if (ri->item != ri->data)
	{
		StepNode *laststep = (StepNode*) ri->data;
		if ((laststep->arraySize() != 0) && (laststep->arrayIndex() == NULL))
		{
			msg.print("Previous step in path requires an array index to be specified.\n");
			msg.exit("Tree::expandPath");
			return FALSE;
		}
	}
	// Find next step accessor
	StepNode *result = ri->data->findAccessor(name->get(), arrayindex, arglist);
	// If we found a valid accessor, update the pathstack entry
	if (result)
	{
		msg.print(Messenger::Parse,"...OK - matching accessor found: return type is %s\n", VTypes::dataType(result->returnType()));
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
// 	else msg.print("Error: Object of type '%s' has no matching accessor for '%s'.\n", VTypes::dataType(ri->data->returnType()), name->get());
	msg.exit("Tree::expandPath");
	return result;
}

// Return number of arguments defined (for function)
int Tree::nArgs() const
{
	return arguments_.nItems();
}

// Return first argument defined (for function)
TreeNode *Tree::args() const
{
	return arguments_.first();
}

/*
// Local Functions
*/

// Add new local function
Tree *Tree::addLocalFunction(const char *funcname)
{
	Tree* result = functions_.add();
	result->setName(funcname);
	result->setType(Tree::FunctionTree);
	result->setParent(parent_);
	return result;
}

// Search for existing local function
Tree *Tree::findLocalFunction(const char *funcname) const
{
	Tree *result;
	for (result = functions_.first(); result != NULL; result = result ->next) if (strcmp(result->name(),funcname) == 0) break;
	return result;
}

/*
// Custom GUI Widgets
*/

// Add new (GUI-based) widget linked to a variable
TreeNode *Tree::addWidget(TreeNode *arglist)
{
	msg.enter("Tree::addWidget");
	// Wrap the variable and add it to the arguments_ list
	WidgetNode *node = new WidgetNode();
	arguments_.own(node);
	node->setParent(this);
	// Store in reflist also...
	widgets_.add(node);
	// Add arguments to node (also sets return type)
	if (node->addJoinedArguments(arglist)) msg.print(Messenger::Parse, "Added GUI widget '%s'...\n", node->name());
	else
	{
		msg.print("Failed to add GUI widget.\n");
		msg.exit("Tree::addWidget");
		return NULL;
	}
	msg.exit("Tree::addWidget");
	return node;
}

// Return first item in list of widgets
Refitem<WidgetNode,int> *Tree::widgets()
{
	return widgets_.first();
}

// Set GUI main widget
void Tree::setMainWidget(QWidget *layout)
{
	mainWidget_ = layout;
}

// Return main widget
QWidget *Tree::mainWidget()
{
	return mainWidget_;
}

// Locate named widget
WidgetNode *Tree::findWidget(const char *name)
{
	// Search through disordered node list for ScopedNodes and then search their variable listst
	for (Refitem<WidgetNode,int> *ri = widgets_.first(); ri != NULL; ri = ri->next)
	{
		if (strcmp(name, ri->item->name()) == 0) return ri->item;
	}
	printf("Internal Error: Couldn't find widget named '%s' in tree '%s'.\n", name, name_.get());
	return NULL;
}

// Retrieve current value of named widget as a double
double Tree::widgetValued(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return 0.0;
	ReturnValue rv;
	node->execute(rv);
	return rv.asDouble();
}

// Retrieve current value of named widget as an integer
int Tree::widgetValuei(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return 0;
	ReturnValue rv;
	node->execute(rv);
	return rv.asInteger();
}

// Retrieve current value of named widget as a string
const char *Tree::widgetValuec(const char *name)
{
	WidgetNode *node = findWidget(name);
	if (node == NULL) return "NULL";
	ReturnValue rv;
	node->execute(rv);
	return rv.asString();
}

// Retrieve current value of named widget triplet as a vector
Vec3<double> Tree::widgetValue3d(const char *name1, const char *name2, const char *name3)
{
	ReturnValue rv;
	Vec3<double> result;
	// First value
	WidgetNode *node = findWidget(name1);
	if (node == NULL) result.x = 0.0;
	node->execute(rv);
	result.x = rv.asDouble();
	// Second value
	node = findWidget(name2);
	if (node == NULL) result.y = 0.0;
	node->execute(rv);
	result.y = rv.asDouble();
	// Third value
	node = findWidget(name3);
	if (node == NULL) result.z = 0.0;
	node->execute(rv);
	result.z = rv.asDouble();
	return result;
}
