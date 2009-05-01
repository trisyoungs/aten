/*
	*** Parser Tree Interface
	*** src/parser/parser_tree.cpp
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

#include "parser/parser.h"
#include "command/commands.h"
#include "parser/forest.h"

// Create a new path on the stack with the specified base 'variable'
TreeNode *CommandParser::createPath(TreeNode *var)
{
	msg.enter("CommandParser::createPath");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (createPath).\n");
		else printf("Internal Error: No current Tree target for Parser (createPath).\n");
		msg.exit("CommandParser::createPath");
		return FALSE;
	}
	TreeNode *result = tree_->createPath(var);
	msg.exit("CommandParser::createPath");
	return result;
}

// Expand topmost path
bool CommandParser::expandPath(Dnchar *name, TreeNode *arrayindex)
{
	msg.enter("CommandParser::expandPath");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (expandPath).\n");
		else printf("Internal Error: No current Tree target for Parser (expandPath).\n");
		msg.exit("CommandParser::createPath");	
		return FALSE;
	}
	bool result = tree_->expandPath(name, arrayindex);
	msg.exit("CommandParser::expandPath");
	return result;
}

// Finalise and remove the topmost path on the stack
TreeNode *CommandParser::finalisePath()
{
	msg.enter("CommandParser::finalisePath");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (finalisePath).\n");
		else printf("Internal Error: No current Tree target for Parser (finalisePath).\n");
		msg.exit("CommandParser::finalisePath");	
		return FALSE;
	}
	TreeNode *result = tree_->finalisePath();
	msg.exit("CommandParser::finalisePath");
	return result;
}

// Add a node representing a whole statement to the execution list
bool CommandParser::addStatement(TreeNode *leaf)
{
	msg.enter("CommandParser::addStatement");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addStatement).\n");
		else printf("Internal Error: No current Tree target for Parser (addStatement).\n");
		msg.exit("CommandParser::addStatement");	
		return FALSE;
	}
	else tree_->addStatement(leaf);
	msg.exit("CommandParser::addStatement");
	return TRUE;
}

// Add an operator to the Tree
TreeNode *CommandParser::addOperator(Command::Function func, TreeNode *arg1, TreeNode *arg2)
{
	msg.enter("CommandParser::addOperator");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addOperator).\n");
		else printf("Internal Error: No current Tree target for Parser (addOperator).\n");
		msg.exit("CommandParser::addOperator");	
		return NULL;
	}
	TreeNode *result = tree_->addOperator(func, arg1, arg2);
	msg.exit("CommandParser::addOperator");
	return result;
}

// Add a function node to the list (overloaded to accept simple arguments instead of a list)
TreeNode *CommandParser::addFunction(Command::Function func, TreeNode *a1, TreeNode *a2, TreeNode *a3, TreeNode *a4)
{
	msg.enter("CommandParser::addFunction");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addFunction).\n");
		else printf("Internal Error: No current Tree target for Parser (addFunction).\n");
		msg.exit("CommandParser::addFunction");
		return NULL;
	}
	TreeNode *result = tree_->addFunction(func, a1, a2, a3, a4);
	msg.exit("CommandParser::addFunction");
	return result;
}

// Associate a command-based leaf node to the Tree
TreeNode *CommandParser::addFunctionWithArglist(Command::Function func, TreeNode *arglist)
{
	msg.enter("CommandParser::addFunctionWithArglist");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addFunctionWithArglist).\n");
		else printf("Internal Error: No current Tree target for Parser (addFunctionWithArglist).\n");
		msg.exit("CommandParser::addFunctionWithArglist");	
		return NULL;
	}
	TreeNode *result = tree_->addFunctionWithArglist(func, arglist);
	msg.exit("CommandParser::addFunctionWithArglist");
	return result;
}

// Add a used-define function node to the list
TreeNode *CommandParser::addUserFunction(Tree *func, TreeNode *arglist)
{
	msg.enter("CommandParser::addUserFunction");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addUserFunction).\n");
		else printf("Internal Error: No current Tree target for Parser (addUserFunction).\n");
		msg.exit("CommandParser::addUserFunction");
		return NULL;
	}
	TreeNode *result = tree_->addUserFunction(func, arglist);
	msg.exit("CommandParser::addUserFunction");
	return result;
}

// Add a declaration list
TreeNode *CommandParser::addDeclarations(TreeNode *declist)
{
	msg.enter("CommandParser::addDeclarations");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addDeclarations).\n");
		else printf("Internal Error: No current Tree target for Parser (addDeclarations).\n");
		msg.exit("CommandParser::addDeclarations");	
		return NULL;
	}
	TreeNode *result = tree_->addDeclarations(declist);
	msg.exit("CommandParser::addDeclarations");
	return result;
}

// Add an argument list
bool CommandParser::addArguments(TreeNode *arglist)
{
	msg.enter("CommandParser::addArguments");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addArguments).\n");
		else printf("Internal Error: No current Tree target for Parser (addArguments).\n");
		msg.exit("CommandParser::addArguments");	
		return NULL;
	}
	bool result = tree_->addArguments(arglist);
	msg.exit("CommandParser::addArguments");
	return result;
}

// Join two commands together
TreeNode *CommandParser::joinCommands(TreeNode *node1, TreeNode *node2)
{
	msg.enter("CommandParser::joinCommands");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (joinCommands).\n");
		else printf("Internal Error: No current Tree target for Parser (joinCommands).\n");
		msg.exit("CommandParser::joinCommands");	
		return FALSE;
	}
	TreeNode *result = tree_->joinCommands(node1, node2);
	msg.exit("CommandParser::joinCommands");
	return result;
}

// Add on a new scope to the stack
TreeNode *CommandParser::pushScope(Command::Function func)
{
	msg.enter("CommandParser::pushScope");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (pushScope).\n");
		else printf("Internal Error: No current Tree target for Parser (pushScope).\n");
		msg.exit("CommandParser::pushScope");	
		return FALSE;
	}
	TreeNode *result = tree_->pushScope(func);
	msg.exit("CommandParser::pushScope");
	return result;
}

// Pop the topmost scope node
bool CommandParser::popScope()
{
	msg.enter("CommandParser::popScope");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (popScope).\n");
		else printf("Internal Error: No current Tree target for Parser (popScope).\n");
		msg.exit("CommandParser::popScope");	
		return FALSE;
	}
	else tree_->popScope();
	msg.exit("CommandParser::popScope");
	return TRUE;
}

// Add constant value to tompost scope
TreeNode *CommandParser::addConstant(VTypes::DataType type, Dnchar *token)
{
	msg.enter("CommandParser::addConstant");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addConstant).\n");
		else printf("Internal Error: No current Tree target for Parser (addConstant).\n");
		msg.exit("CommandParser::addConstant");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(type, token);
	msg.exit("CommandParser::addConstant");
	return result;
}

// Add integer constant
TreeNode *CommandParser::addConstant(int i)
{
	msg.enter("CommandParser::addConstant(int)");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addConstant(int)).\n");
		else printf("Internal Error: No current Tree target for Parser (addConstant(int)).\n");
		msg.exit("CommandParser::addConstant(int)");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(i);
	msg.exit("CommandParser::addConstant(int)");
	return result;
}

// Add double constant
TreeNode *CommandParser::addConstant(double d)
{
	msg.enter("CommandParser::addConstant(double)");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addConstant(double)).\n");
		else printf("Internal Error: No current Tree target for Parser (addConstant(double)).\n");
		msg.exit("CommandParser::addConstant(double)");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(d);
	msg.exit("CommandParser::addConstant(double)");
	return result;
}

// Add string constant
TreeNode *CommandParser::addConstant(const char *s)
{
	msg.enter("CommandParser::addConstant(const char)");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addConstant(const char)).\n");
		else printf("Internal Error: No current Tree target for Parser (addConstant(const char)).\n");
		msg.exit("CommandParser::addConstant(const char)");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(s);
	msg.exit("CommandParser::addConstant(const char)");
	return result;
}

// Add variable to topmost ScopeNode
TreeNode *CommandParser::addVariable(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.enter("CommandParser::addVariable");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addVariable).\n");
		else printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		msg.exit("CommandParser::addVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(type, name, initialValue);
	msg.exit("CommandParser::addVariable");
	return result;
}

// Add variable as function argument to topmost ScopeNode
TreeNode *CommandParser::addVariableAsArgument(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.enter("CommandParser::addVariableAsArgument");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addVariableAsArgument).\n");
		else printf("Internal Error: No current Tree target for Parser (addVariableAsArgument).\n");
		msg.exit("CommandParser::addVariableAsArgument");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariableAsArgument(type, name, initialValue);
	msg.exit("CommandParser::addVariableAsArgument");
	return result;
}


// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *CommandParser::addArrayVariable(VTypes::DataType type, Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	msg.enter("CommandParser::addArrayVariable");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addArrayVariable).\n");
		else printf("Internal Error: No current Tree target for Parser (addArrayVariable).\n");
		msg.exit("CommandParser::addArrayVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addArrayVariable(type, name, sizeexpr, initialvalue);
	msg.exit("CommandParser::addArrayVariable");
	return result;
}

// Add constant vector
TreeNode *CommandParser::addArrayConstant(TreeNode *values)
{
	msg.enter("CommandParser::addArrayConstant");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (addArrayConstant).\n");
		else printf("Internal Error: No current Tree target for Parser (addArrayConstant).\n");
		msg.exit("CommandParser::addArrayConstant");	
		return FALSE;
	}
	TreeNode *result = tree_->addArrayConstant(values);
	msg.exit("CommandParser::addArrayConstant");
	return result;
}

// Wrap named variable (and array index)
TreeNode *CommandParser::wrapVariable(Variable *var, TreeNode *arrayindex)
{
	msg.enter("CommandParser::wrapVariable");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (wrapVariable).\n");
		else printf("Internal Error: No current Tree target for Parser (wrapVariable).\n");
		msg.exit("CommandParser::wrapVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->wrapVariable(var, arrayindex);
	msg.exit("CommandParser::wrapVariable");
	return result;
}

// Set filter option
bool CommandParser::setFilterOption(Dnchar *name, TreeNode *value)
{
	msg.enter("CommandParser::setFilterOption");
	if (tree_ == NULL)
	{
		if (forest_->isFromFilterFile()) msg.print("Statements found outside of a filter/function definition (setFilterOption).\n");
		else printf("Internal Error: No current Tree target for Parser (setFilterOption).\n");
		msg.exit("CommandParser::setFilterOption");	
		return FALSE;
	}
	bool result = tree_->filter.setOption(name, value);
	msg.exit("CommandParser::setFilterOption");
	return result;
}
