/*
	*** User Command Node
	*** src/parser/usercommandnode.cpp
	Copyright T. Youngs 2007-2015

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

#include "parser/usercommandnode.h"
#include "parser/tree.h"
#include "parser/variablenode.h"

ATEN_USING_NAMESPACE

// Constructor
UserCommandNode::UserCommandNode(Tree* func) : TreeNode()
{
	// Private variables
	function_ = func;
	nodeType_ = TreeNode::UserCmdNode;
}

// Destructor
UserCommandNode::~UserCommandNode()
{
}

// Check validity of supplied arguments
bool UserCommandNode::checkArguments()
{
	Messenger::enter("UserCommandNode::checkArguments");
	Messenger::print(Messenger::Parse, "Checking the %i argument(s) given to user function '%s'...\n", args_.nItems(), function_->name());
	bool required;
	int count = 0;
	Variable *v;
	for (TreeNode* arg = function_->args(); arg != NULL; arg = arg->next)
	{
		v = ((VariableNode*) arg)->variable();
		// Is this a required argument?
		required = v->initialValue() == NULL;
		if (required && (args_.nItems() <= count))
		{
			Messenger::print("Error: Argument %i to user function '%s' is required.\n", count+1, function_->name());
			Messenger::exit("UserCommandNode::checkArguments");
			return FALSE;
		}
		else if ((!required) && ((args_.nItems() <= count))) break;
		// Check type of argument against type provided.
		// Allow doubles/ints to be interchangeable ('N' in normal command args). Otherwise, exact type match is required
		if ((v->returnType() == VTypes::IntegerData) || (v->returnType() == VTypes::DoubleData))
		{
			if ((args_[count]->item->returnType() == VTypes::IntegerData) || (args_[count]->item->returnType() == VTypes::DoubleData))
			{
				count++;
				continue;
			}
			else
			{
				Messenger::print("Error: Argument %i to user function '%s' expected %s but was given %s.\n", count+1, function_->name(), VTypes::aDataType(v->returnType()), VTypes::aDataType(args_[count]->item->returnType()));
				Messenger::exit("UserCommandNode::checkArguments");
				return FALSE;
			}
		}
		else if (v->returnType() == args_[count]->item->returnType())
		{
			count++;
			continue;
		}
		else
		{
			Messenger::print("Error: Argument %i to user function '%s' expected %s but was given %s.\n", count+1, function_->name(), VTypes::aDataType(v->returnType()), VTypes::aDataType(args_[count]->item->returnType()));
			Messenger::exit("UserCommandNode::checkArguments");
			return FALSE;
		}
	}
	// Extra arguments provided?
	if (args_.nItems() > count)
	{
		Messenger::print("Error: %i extra arguments given to user function '%s'.\n", args_.nItems()-count, function_->name());
		Messenger::exit("UserCommandNode::checkArguments");
		return FALSE;
	}
	Messenger::exit("UserCommandNode::checkArguments");
	return TRUE;
}

// Execute command
bool UserCommandNode::execute(ReturnValue& rv)
{
	// Check for valid function
	if (function_ == NULL) return FALSE;

	// Poke arguments into the functions argument variables
	Refitem<TreeNode,int>* value = args_.first();
	ReturnValue varval;
	Variable *v;
	for (TreeNode* arg = function_->args(); arg != NULL; arg = arg->next)
	{
		// If 'value' is not NULL, execute it and get the value to pass to the argument
		if (value != NULL)
		{
			if (!value->item->execute(varval)) return FALSE;
			if (!arg->set(varval)) return FALSE;
			value = value->next;
		}
		else
		{
			// Presumably a required argument?
			v = ((VariableNode*) arg)->variable();
			if (v->initialValue() == NULL) printf("Required argument not fulfilled?\n");
			else if (!v->initialise()) return FALSE;
		}
	}
	// We must pass the current input 'state' of this node's parent tree - give it the LineParser pointer...
	LineParser *parser = parent_->parser();
	bool result;
	result = parser == NULL ? function_->execute(rv) : function_->execute(parser, rv);
	return result;
}

// Print node contents
void UserCommandNode::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
// 	printf("Function id = %p\n", function_);
	printf("[UC]%s%s (UserCommand) (%i arguments)\n", tab.get(), function_->name(), args_.nItems());
	// Output Argument data
	for (Refitem<TreeNode,int>* ri = args_.first(); ri != NULL; ri = ri->next) ri->item->nodePrint(offset+1);
}

// Set from returnvalue node
bool UserCommandNode::set(ReturnValue& rv)
{
	printf("Internal Error: Trying to 'set' a UserCommandNode.\n");
	return FALSE;
}

// Initialise node
bool UserCommandNode::initialise()
{
	printf("Internal Error: A UserCommandNode cannot be initialised.\n");
	return FALSE;
}

// Set function pointer
void UserCommandNode::setFunction(Tree* func)
{
	function_ = func;
}

// Create, run, and free a single command with simple argument list
bool UserCommandNode::run(Tree* func, ReturnValue& rv, const char* argList ...)
{
	Messenger::enter("UserCommandNode::run");
	// Local tree to contain usercommandnode and its arguments
	Tree tree;

	// Create our temporary node
	UserCommandNode node(func);
	node.parent_ = &tree;

	// Set arguments from supplied list
	const char* c;
	va_list vars;
	va_start(vars, argList);
	TreeNode* var = NULL;
	for (c = &argList[0]; *c != '\0'; c++)
	{
		switch (*c)
		{
			case ('i'):
				var = tree.addConstant(va_arg(vars, int));
				break;
			case ('d'):
				var = tree.addConstant(va_arg(vars, double));
				break;
			case ('c'):
			case ('s'):
				var = tree.addConstant(va_arg(vars, const char* ));
				break;
			default:
				printf("Invalid argument specifier '%c' in UserCommandNode::run.\n", *c);
				var = NULL;
				break;
		}
		node.addArgument(var);
	}
	va_end(vars);
	// Now, run the command...
	bool result = node.execute(rv);
	Messenger::exit("UserCommandNode::run");
	return result;
}

// Create, run, and free a single command with simple argument list
bool UserCommandNode::run(Tree* func, ReturnValue& rv, TreeNode* argListhead)
{
	Messenger::enter("UserCommandNode::run(argList)");
	// Local tree to contain usercommandnode and its arguments
	Tree tree;

	// Create our temporary node
	UserCommandNode node(func);
	node.parent_ = &tree;

	// Set arguments from supplied list
	node.addListArguments(argListhead);

	// Now, run the command...
	bool result = node.execute(rv);
	Messenger::exit("UserCommandNode::run(argList)");
	return result;
}
