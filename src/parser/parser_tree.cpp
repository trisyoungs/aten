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

// Create a new path on the stack with the specified base 'variable'
TreeNode *NuParser::createPath(TreeNode *var)
{
	msg.enter("NuParser::createPath");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (createPath).\n");
		msg.exit("NuParser::createPath");
		return FALSE;
	}
	TreeNode *result = tree_->createPath(var);
	msg.exit("NuParser::createPath");
	return result;
}

// Expand topmost path
bool NuParser::expandPath(Dnchar *name, TreeNode *arrayindex)
{
	msg.enter("NuParser::expandPath");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (expandPath).\n");
		msg.exit("NuParser::createPath");	
		return FALSE;
	}
	bool result = tree_->expandPath(name, arrayindex);
	msg.exit("NuParser::expandPath");
	return result;
}

// Finalise and remove the topmost path on the stack
TreeNode *NuParser::finalisePath()
{
	msg.enter("NuParser::finalisePath");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (finalisePath).\n");
		msg.exit("NuParser::finalisePath");	
		return FALSE;
	}
	TreeNode *result = tree_->finalisePath();
	msg.exit("NuParser::finalisePath");
	return result;
}

// Add a node representing a whole statement to the execution list
bool NuParser::addStatement(TreeNode *leaf)
{
	msg.enter("NuParser::addStatement");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addStatement).\n");
		msg.exit("NuParser::addStatement");	
		return FALSE;
	}
	else tree_->addStatement(leaf);
	msg.exit("NuParser::addStatement");
	return TRUE;
}

// Add an operator to the Tree
TreeNode *NuParser::addOperator(NuCommand::Function func, int typearg, TreeNode *arg1, TreeNode *arg2)
{
	msg.enter("NuParser::addOperator");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addOperator).\n");
		msg.exit("NuParser::addOperator");	
		return NULL;
	}
	TreeNode *result = tree_->addOperator(func, typearg, arg1, arg2);
	msg.exit("NuParser::addOperator");
	return result;
}

// Add 'if' statement
TreeNode *NuParser::addIf(TreeNode *condition, TreeNode *expr1, TreeNode *expr2)
{
	msg.enter("NuParser::addIf");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addIf).\n");
		msg.exit("NuParser::addIf");	
		return NULL;
	}
	TreeNode *result = tree_->addIf(condition, expr1, expr2);
	msg.exit("NuParser::addIf");
	return result;
}

// Add 'for' statement
TreeNode *NuParser::addFor(TreeNode *init, TreeNode *condition, TreeNode *action, TreeNode *statements)
{
	msg.enter("NuParser::addFor");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFor).\n");
		msg.exit("NuParser::addFor");	
		return NULL;
	}
	TreeNode *result = tree_->addFor(init, condition, action, statements);
	msg.exit("NuParser::addFor");
	return result;
}

// Associate a command-based leaf node to the Tree
TreeNode *NuParser::addFunction(NuCommand::Function func, TreeNode *arglist)
{
	msg.enter("NuParser::addFunction");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunction).\n");
		msg.exit("NuParser::addFunction");	
		return NULL;
	}
	TreeNode *result = tree_->addFunction(func, arglist);
	msg.exit("NuParser::addFunction");
	return result;
}

// Join two commands together
TreeNode *NuParser::joinCommands(TreeNode *node1, TreeNode *node2)
{
	msg.enter("NuParser::joinCommands");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (joinCommands).\n");
		msg.exit("NuParser::joinCommands");	
		return FALSE;
	}
	TreeNode *result = tree_->joinCommands(node1, node2);
	msg.exit("NuParser::joinCommands");
	return result;
}

// Add on a new scope to the stack
TreeNode *NuParser::pushScope()
{
	msg.enter("NuParser::pushScope");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (pushScope).\n");
		msg.exit("NuParser::pushScope");	
		return FALSE;
	}
	TreeNode *result = tree_->pushScope();
	msg.exit("NuParser::pushScope");
	return result;
}

// Pop the topmost scope node
bool NuParser::popScope()
{
	msg.enter("NuParser::popScope");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (popScope).\n");
		msg.exit("NuParser::popScope");	
		return FALSE;
	}
	else tree_->popScope();
	msg.exit("NuParser::popScope");
	return TRUE;
}

// Set current type for variable declarations
bool NuParser::setDeclaredVariableType(NuVTypes::DataType type)
{
	msg.enter("NuParser::setDeclaredVariableType");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setDeclaredVariableType).\n");
		msg.exit("NuParser::setDeclaredVariableType");	
		return FALSE;
	}
	bool result = tree_->setDeclaredVariableType(type);
	msg.exit("NuParser::setDeclaredVariableType");
	return result;
}

// Set declarations assignment flag
bool NuParser::setDeclarationAssignment(bool b)
{
	msg.enter("NuParser::setDeclarationAssignment");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setDeclarationAssignment).\n");
		msg.exit("NuParser::setDeclarationAssignment");	
		return FALSE;
	}
	bool result = tree_->setDeclarationAssignment(b);
	msg.exit("NuParser::setDeclarationAssignment");
	return result;
}

// Add constant value to tompost scope
TreeNode *NuParser::addConstant(NuVTypes::DataType type, Dnchar *token)
{
	msg.enter("NuParser::addConstant");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant).\n");
		msg.exit("NuParser::addConstant");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(type, token);
	msg.exit("NuParser::addConstant");
	return result;
}

// Add variable to topmost ScopeNode
TreeNode *NuParser::addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.enter("NuParser::addVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		msg.exit("NuParser::addVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(type, name, initialValue);
	msg.exit("NuParser::addVariable");
	return result;
}

// Add variable to topmost ScopeNode using the most recently declared type
TreeNode *NuParser::addVariable(Dnchar *name, TreeNode *initialValue)
{
	msg.enter("NuParser::addVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		msg.exit("NuParser::addVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(name, initialValue);
	msg.exit("NuParser::addVariable");
	return result;
}

// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *NuParser::addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	msg.enter("NuParser::addArrayVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addArrayVariable).\n");
		msg.exit("NuParser::addArrayVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addArrayVariable(name, sizeexpr, initialvalue);
	msg.exit("NuParser::addArrayVariable");
	return result;
}

// Search for variable in current scope
bool NuParser::isVariableInScope(const char *name, NuVariable *&res)
{
	msg.enter("NuParser::isVariableInScope");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (isVariableInScope).\n");
		msg.exit("NuParser::isVariableInScope");	
		return FALSE;
	}
	bool result = tree_->isVariableInScope(name, res);
	msg.exit("NuParser::isVariableInScope");
	return result;
}

// Wrap named variable (and array index)
TreeNode *NuParser::wrapVariable(NuVariable *var, TreeNode *arrayindex)
{
	msg.enter("NuParser::wrapVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (wrapVariable).\n");
		msg.exit("NuParser::wrapVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->wrapVariable(var, arrayindex);
	msg.exit("NuParser::wrapVariable");
	return result;
}

// Set filter option
bool NuParser::setFilterOption(Dnchar *name, TreeNode *value)
{
	msg.enter("NuParser::setFilterOption");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setFilterOption).\n");
		msg.exit("NuParser::setFilterOption");	
		return FALSE;
	}
	bool result = tree_->filter.setOption(name, value);
	msg.exit("NuParser::setFilterOption");
	return result;
}
