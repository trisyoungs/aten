/*
	*** Parser Tree Interface
	*** src/parser/parser_tree.cpp
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

#include "parser/parser.h"
#include "command/commands.h"

// Create a new path on the stack with the specified base 'variable'
TreeNode *CommandParser::createPath(TreeNode *var)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (createPath).\n");
		return FALSE;
	}
	TreeNode *result = tree_->createPath(var);
	return result;
}

// Expand topmost path
bool CommandParser::expandPath(Dnchar *name, TreeNode *arrayindex, TreeNode *arglist)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (expandPath).\n");
		return FALSE;
	}
	bool result = tree_->expandPath(name, arrayindex, arglist);
	return result;
}

// Finalise and remove the topmost path on the stack
TreeNode *CommandParser::finalisePath()
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (finalisePath).\n");
		return FALSE;
	}
	TreeNode *result = tree_->finalisePath();
	return result;
}

// Add a node representing a whole statement to the execution list
bool CommandParser::addStatement(TreeNode *leaf)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addStatement).\n");
		return FALSE;
	}
	else tree_->addStatement(leaf);
	return TRUE;
}

// Add an operator to the Tree
TreeNode *CommandParser::addOperator(Command::Function func, TreeNode *arg1, TreeNode *arg2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addOperator).\n");
		return NULL;
	}
	TreeNode *result = tree_->addOperator(func, arg1, arg2);
	return result;
}

// Add a function node to the list (overloaded to accept simple arguments instead of a list)
TreeNode *CommandParser::addFunction(Command::Function func, TreeNode *a1, TreeNode *a2, TreeNode *a3, TreeNode *a4)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunction).\n");
		return NULL;
	}
	TreeNode *result = tree_->addFunction(func, a1, a2, a3, a4);
	return result;
}

// Associate a command-based leaf node to the Tree
TreeNode *CommandParser::addFunctionWithArglist(Command::Function func, TreeNode *arglist)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunctionWithArglist).\n");
		return NULL;
	}
	TreeNode *result = tree_->addFunctionWithArglist(func, arglist);
	return result;
}

// Add a used-define function node to the list
TreeNode *CommandParser::addUserFunction(Tree *func, TreeNode *arglist)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addUserFunction).\n");
		return NULL;
	}
	TreeNode *result = tree_->addUserFunction(func, arglist);
	return result;
}

// Add a declaration list
TreeNode *CommandParser::addDeclarations(TreeNode *declist)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addDeclarations).\n");
		return NULL;
	}
	TreeNode *result = tree_->addDeclarations(declist);
	return result;
}

// Join two commands together
TreeNode *CommandParser::joinCommands(TreeNode *node1, TreeNode *node2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (joinCommands).\n");
		return FALSE;
	}
	TreeNode *result = tree_->joinCommands(node1, node2);
	return result;
}

// Add on a new scope to the stack
TreeNode *CommandParser::pushScope(Command::Function func)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (pushScope).\n");
		return FALSE;
	}
	TreeNode *result = tree_->pushScope(func);
	return result;
}

// Pop the topmost scope node
bool CommandParser::popScope()
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (popScope).\n");
		return FALSE;
	}
	else tree_->popScope();
	return TRUE;
}

// Add constant value to tompost scope
TreeNode *CommandParser::addConstant(VTypes::DataType type, Dnchar *token)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(type, token);
	return result;
}

// Add integer constant
TreeNode *CommandParser::addConstant(int i)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant(int)).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(i);
	return result;
}

// Add double constant
TreeNode *CommandParser::addConstant(double d)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant(double)).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(d);
	return result;
}

// Add variable to topmost ScopeNode
TreeNode *CommandParser::addVariable(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(type, name, initialValue);
	return result;
}

// Add variable as function argument to topmost ScopeNode
TreeNode *CommandParser::addVariableAsArgument(VTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariableAsArgument).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addVariableAsArgument(type, name, initialValue);
	return result;
}


// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *CommandParser::addArrayVariable(VTypes::DataType type, Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addArrayVariable).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addArrayVariable(type, name, sizeexpr, initialvalue);
	return result;
}

// Add constant vector
TreeNode *CommandParser::addArrayConstant(TreeNode *values)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addArrayConstant).\n");
		return FALSE;
	}
	TreeNode *result = tree_->addArrayConstant(values);
	return result;
}

// Wrap named variable (and array index)
TreeNode *CommandParser::wrapVariable(Variable *var, TreeNode *arrayindex)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (wrapVariable).\n");
		return FALSE;
	}
	TreeNode *result = tree_->wrapVariable(var, arrayindex);
	return result;
}
