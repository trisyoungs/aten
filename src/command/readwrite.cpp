/*
	*** Read/write Commands
	*** src/command/readwrite.cpp
	Copyright T. Youngs 2007-2017

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

#include "parser/commandnode.h"
#include "parser/tree.h"
#include "parser/variablenode.h"
#include "parser/double.h"
#include "parser/integer.h"

ATEN_USING_NAMESPACE

// Parse given variable using delimiters
bool Commands::function_ReadVariable(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readVar'.\n");
		return false;
	}
	rv.set( format->read( c->argc(0), Parser::Defaults ) );
	return true;
}

// Parse given variable with format
bool Commands::function_ReadVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'readVarF'.\n");
		return false;
	}
	rv.set( format->read( c->argc(0), Parser::Defaults ) );
	return true;
}

// Write delimited line to variable
bool Commands::function_WriteVariable(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(-1,1);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevar'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return true;
}

// Write formatted line to variable
bool Commands::function_WriteVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Format* format = c->createFormat(1,2);
	if (format == NULL)
	{
		printf("Internal Error: No format node associated to command 'writevarf'.\n");
		return false;
	}
	// Create the string to be output
	if (!format->writeToString())
	{
		Messenger::print("Failed to format string for output.");
		return false;
	}
	ReturnValue string;
	string.set(format->string());
	c->setArg(0, string);
	return true;
}

