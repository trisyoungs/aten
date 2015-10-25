/*
	*** Aten's Commands
	*** src/main/commands.cpp
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

#include "main/aten.h"

ATEN_USING_NAMESPACE

// Return specified command keyword
const char* Aten::commandKeyword(Commands::Function func)
{
	return commands_.data(func).keyword;
}

// Return specified command arguments
const char* Aten::commandArguments(Commands::Function func)
{
	return commands_.data(func).arguments;
}

// Return specified return-value datatype
VTypes::DataType Aten::commandReturnType(Commands::Function func)
{
	return commands_.data(func).returnType;
}

// Return whether specified command takes any arguments
bool Aten::commandHasArguments(Commands::Function func)
{
	return commands_.data(func).hasArguments();
}

// Return specified command argument names
const char* Aten::commandArgText(Commands::Function func)
{
	return commands_.data(func).argText;
}

// Return specified command syntax
const char* Aten::commandSyntax(Commands::Function func)
{
	return commands_.data(func).syntax;
}

// Execute command
bool Aten::callCommand(Commands::Function cf, CommandNode* node, ReturnValue& rv)
{
	Messenger::print(Messenger::Commands, "Calling command '%s' (node is %p)...", commandKeyword(cf), node);
	return commands_.call(cf, node, current_, rv);
}

// Execute specified command with specified bundle
bool Aten::callCommand(Commands::Function cf, TreeNode* node, ReturnValue& rv, Bundle& bundle)
{
	Messenger::print(Messenger::Commands, "Calling command '%s' (treenode is %p) with custom bundle...", commandKeyword(cf), node);
	CommandNode cmdNode(node);
	return commands_.call(cf, &cmdNode, bundle, rv);
}
