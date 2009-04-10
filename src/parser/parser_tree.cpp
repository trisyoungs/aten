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
TreeNode *CommandParser::createPath(TreeNode *var)
{
	msg.enter("CommandParser::createPath");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (createPath).\n");
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
		printf("Internal Error: No current Tree target for Parser (expandPath).\n");
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
		printf("Internal Error: No current Tree target for Parser (finalisePath).\n");
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
		printf("Internal Error: No current Tree target for Parser (addStatement).\n");
		msg.exit("CommandParser::addStatement");	
		return FALSE;
	}
	else tree_->addStatement(leaf);
	msg.exit("CommandParser::addStatement");
	return TRUE;
}

// Add an operator to the Tree
TreeNode *CommandParser::addOperator(NuCommand::Function func, TreeNode *arg1, TreeNode *arg2)
{
	msg.enter("CommandParser::addOperator");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addOperator).\n");
		msg.exit("CommandParser::addOperator");	
		return NULL;
	}
	TreeNode *result = tree_->addOperator(func, arg1, arg2);
	msg.exit("CommandParser::addOperator");
	return result;
}

// Add a function node to the list (overloaded to accept simple arguments instead of a list)
TreeNode *CommandParser::addFunction(NuCommand::Function func, TreeNode *a1, TreeNode *a2, TreeNode *a3, TreeNode *a4)
{
	msg.enter("CommandParser::addFunction");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunction).\n");
		msg.exit("CommandParser::addFunction");
		return NULL;
	}
	TreeNode *result = tree_->addFunction(func, a1, a2, a3, a4);
	msg.exit("CommandParser::addFunction");
	return result;
}

// Associate a command-based leaf node to the Tree
TreeNode *CommandParser::addFunctionWithArglist(NuCommand::Function func, TreeNode *arglist)
{
	msg.enter("CommandParser::addFunctionWithArglist");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addFunctionWithArglist).\n");
		msg.exit("CommandParser::addFunctionWithArglist");	
		return NULL;
	}
	TreeNode *result = tree_->addFunctionWithArglist(func, arglist);
	msg.exit("CommandParser::addFunctionWithArglist");
	return result;
}

// Join two commands together
TreeNode *CommandParser::joinCommands(TreeNode *node1, TreeNode *node2)
{
	msg.enter("CommandParser::joinCommands");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (joinCommands).\n");
		msg.exit("CommandParser::joinCommands");	
		return FALSE;
	}
	TreeNode *result = tree_->joinCommands(node1, node2);
	msg.exit("CommandParser::joinCommands");
	return result;
}

// Add on a new scope to the stack
TreeNode *CommandParser::pushScope(NuCommand::Function func)
{
	msg.enter("CommandParser::pushScope");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (pushScope).\n");
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
		printf("Internal Error: No current Tree target for Parser (popScope).\n");
		msg.exit("CommandParser::popScope");	
		return FALSE;
	}
	else tree_->popScope();
	msg.exit("CommandParser::popScope");
	return TRUE;
}

// Set current type for variable declarations
bool CommandParser::setDeclarationType(NuVTypes::DataType type)
{
	msg.enter("CommandParser::setDeclarationType");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (setDeclarationType).\n");
		msg.exit("CommandParser::setDeclarationType");	
		return FALSE;
	}
	bool result = tree_->setDeclarationType(type);
	msg.exit("CommandParser::setDeclarationType");
	return result;
}

// Return current type to be used for declarations
NuVTypes::DataType CommandParser::declarationType()
{
	msg.enter("CommandParser::declarationType");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (declarationType).\n");
		msg.exit("CommandParser::declarationType");	
		return NuVTypes::NoData;
	}
	NuVTypes::DataType result = tree_->declarationType();
	msg.exit("CommandParser::declarationType");
	return result;
}



// Set declarations assignment flag
bool CommandParser::flagDeclarationAssignment(bool b)
{
	msg.enter("CommandParser::flagDeclarationAssignment");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (flagDeclarationAssignment).\n");
		msg.exit("CommandParser::flagDeclarationAssignment");	
		return FALSE;
	}
	bool result = tree_->flagDeclarationAssignment(b);
	msg.exit("CommandParser::flagDeclarationAssignment");
	return result;
}

// Add constant value to tompost scope
TreeNode *CommandParser::addConstant(NuVTypes::DataType type, Dnchar *token)
{
	msg.enter("CommandParser::addConstant");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addConstant).\n");
		msg.exit("CommandParser::addConstant");	
		return FALSE;
	}
	TreeNode *result = tree_->addConstant(type, token);
	msg.exit("CommandParser::addConstant");
	return result;
}

// Add variable to topmost ScopeNode
TreeNode *CommandParser::addVariable(NuVTypes::DataType type, Dnchar *name, TreeNode *initialValue)
{
	msg.enter("CommandParser::addVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		msg.exit("CommandParser::addVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(type, name, initialValue);
	msg.exit("CommandParser::addVariable");
	return result;
}

// Add variable to topmost ScopeNode using the most recently declared type
TreeNode *CommandParser::addVariable(Dnchar *name, TreeNode *initialValue)
{
	msg.enter("CommandParser::addVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addVariable).\n");
		msg.exit("CommandParser::addVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addVariable(name, initialValue);
	msg.exit("CommandParser::addVariable");
	return result;
}

// Add array variable to topmost ScopeNode using the most recently declared type
TreeNode *CommandParser::addArrayVariable(Dnchar *name, TreeNode *sizeexpr, TreeNode *initialvalue)
{
	msg.enter("CommandParser::addArrayVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (addArrayVariable).\n");
		msg.exit("CommandParser::addArrayVariable");	
		return FALSE;
	}
	TreeNode *result = tree_->addArrayVariable(name, sizeexpr, initialvalue);
	msg.exit("CommandParser::addArrayVariable");
	return result;
}

// Search for variable in current scope
bool CommandParser::isVariableInScope(const char *name, NuVariable *&res)
{
	msg.enter("CommandParser::isVariableInScope");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (isVariableInScope).\n");
		msg.exit("CommandParser::isVariableInScope");	
		return FALSE;
	}
	bool result = tree_->isVariableInScope(name, res);
	msg.exit("CommandParser::isVariableInScope");
	return result;
}

// Wrap named variable (and array index)
TreeNode *CommandParser::wrapVariable(NuVariable *var, TreeNode *arrayindex)
{
	msg.enter("CommandParser::wrapVariable");
	if (tree_ == NULL)
	{
		printf("Internal Error: No current Tree target for Parser (wrapVariable).\n");
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
		printf("Internal Error: No current Tree target for Parser (setFilterOption).\n");
		msg.exit("CommandParser::setFilterOption");	
		return FALSE;
	}
	bool result = tree_->filter.setOption(name, value);
	msg.exit("CommandParser::setFilterOption");
	return result;
}
