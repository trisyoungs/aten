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
#include "nucommand/commands.h"

// Add a node representing a whole statement to the execution list
bool NuParser::addStatement(TreeNode *leaf)
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
TreeNode *NuParser::addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addOperator).\n");
		return NULL;
	}
	else return tree_->addOperator(func, typearg, arg1, arg2);
}

// Add 'if' statement
TreeNode *NuParser::addIf(TreeNode *condition, TreeNode *expr1, TreeNode *expr2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addIf).\n");
		return NULL;
	}
	else return tree_->addIf(condition, expr1, expr2);
}

// Add 'for' statement
TreeNode *NuParser::addFor(TreeNode *init, TreeNode *condition, TreeNode *action, TreeNode *statements)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFor).\n");
		return NULL;
	}
	else return tree_->addFor(init, condition, action, statements);
}

// Associate a command-based leaf node to the Tree
TreeNode *NuParser::addFunction(NuCommand::Function func, TreeNode *arglist)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunction).\n");
		return NULL;
	}
	else return tree_->addFunction(func, arglist);
}

// Join two nodes together
TreeNode *NuParser::joinArguments(TreeNode *arg1, TreeNode *arg2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (joinArguments).\n");
		return NULL;
	}
	else return tree_->joinArguments(arg1, arg2);
}

// Pop the most recent function leaf from the stack and own any stored arguments
bool NuParser::finaliseFunction()
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (finaliseFunction).\n");
		return FALSE;
	}
	else tree_->finaliseFunction();
	return TRUE;
}

// Join two commands together
TreeNode *NuParser::joinCommands(TreeNode *node1, TreeNode *node2)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (joinCommands).\n");
		return FALSE;
	}
	else return tree_->joinCommands(node1, node2);
}

// Add on a new scope to the stack
TreeNode *NuParser::pushScope()
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (pushScope).\n");
		return FALSE;
	}
	else return tree_->pushScope();
}

// Pop the topmost scope node
bool NuParser::popScope()
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (popScope).\n");
		return FALSE;
	}
	else tree_->popScope();
	return TRUE;
}

// Set current type for variable declarations
bool NuParser::setDeclaredVariableType(NuVTypes::DataType type)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setDeclaredVariableType).\n");
		return FALSE;
	}
	else return tree_->setDeclarationAssignment(type);
}

// Set declarations assignment flag
bool NuParser::setDeclarationAssignment(bool b)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setDeclarationAssignment).\n");
		return FALSE;
	}
	else return tree_->setDeclarationAssignment(b);
}

// Add constant value to tompost scope
TreeNode *NuParser::addConstant(NuVTypes::DataType type, Dnchar *token)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant).\n");
		return FALSE;
	}
	else return tree_->addConstant(type, token);
}

// Add variable to topmost ScopeNode
TreeNode *NuParser::addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		return FALSE;
	}
	else return tree_->addVariable(type, name, initialValue);
}

// Add variable to topmost ScopeNode using the most recently declared type
TreeNode *NuParser::addVariable(Dnchar *name, TreeNode *initialValue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		return FALSE;
	}
	else return tree_->addVariable(name, initialValue);
}

// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *NuParser::addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addArrayVariable).\n");
		return FALSE;
	}
	else return tree_->addArrayVariable(name, sizeexpr, initialvalue);
}

// Search for variable in current scope
bool NuParser::isVariableInScope(const char *name, NuVariable *&result)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (isVariableInScope).\n");
		return FALSE;
	}
	else return tree_->isVariableInScope(name, result);
}

// Wrap named variable (and array index)
TreeNode *NuParser::wrapVariable(NuVariable *var, TreeNode *arrayindex)
{
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (wrapVariable).\n");
		return FALSE;
	}
	else return tree_->wrapVariable(var, arrayindex);
}

