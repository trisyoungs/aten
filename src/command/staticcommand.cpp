/*
	*** Static Command
	*** src/command/staticcommand.cpp
	Copyright T. Youngs 2007,2008

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

#include "command/commandlist.h"
#include "command/staticcommand.h"
#include "variables/variablelist.h"
#include <stdarg.h>

// Static Members
VariableList StaticCommandNode::staticVariableList_;
CommandList StaticCommandNode::staticCommandList_;

// Constructor
StaticCommandNode::StaticCommandNode(Command::Function func, const char *args, ...)
{
	// Private variables
	cmd_.setFunction(func);
	cmd_.setParent(&staticCommandList_);
	cmd_.setVariableList(&staticVariableList_);

	// Set arguments from supplied list
	const char *c;
	va_list vars;
	va_start(vars,args);
	Variable *v = NULL;
	for (c = args; *c != '\0'; c = c + 1)
	{
		switch (*c)
		{
			case ('i'):
				v = staticVariableList_.addSimpleVariable(VTypes::IntegerData);
				v->set(va_arg(vars, int));
				break;
			case ('d'):
				v = staticVariableList_.addSimpleVariable(VTypes::RealData);
				v->set(va_arg(vars, double));
				break;
			case ('s'):
				v = staticVariableList_.addSimpleVariable(VTypes::CharacterData);
				v->set(va_arg(vars, const char*));
				break;
			default:
				printf("Invalid argument specifier '%c' in StaticCommandNode.\n", *c);
				v = NULL;
				break;
		}
		cmd_.addArgument(v);
	}
	va_end(vars);
}

// Set (overwrite) command
void StaticCommandNode::setFunction(Command::Function cf)
{
	cmd_.setFunction(cf);
}

// Set argument data
void StaticCommandNode::pokeArguments(const char *args, ...)
{
	// Set arguments from supplied list
	const char *c;
	va_list vars;
	va_start(vars,args);
	int count = 0;
	for (c = args; *c != '\0'; c = c + 1)
	{
		switch (*c)
		{
			case ('i'):
				if (cmd_.argt(count) != VTypes::IntegerData) msg.print("Type mismatch when poking arguments in StaticCommandNode - wanted 'integer' but found '%s'.\n", VTypes::dataType(cmd_.argt(count)));
				cmd_.arg(count)->set(va_arg(vars, int));
				break;
			case ('d'):
				if (cmd_.argt(count) != VTypes::RealData) msg.print("Type mismatch when poking arguments in StaticCommandNode - wanted 'real' but found '%s'.\n", VTypes::dataType(cmd_.argt(count)));
				cmd_.arg(count)->set(va_arg(vars, double));
				break;
			case ('s'):
				if (cmd_.argt(count) != VTypes::CharacterData) msg.print("Type mismatch when poking arguments in StaticCommandNode - wanted 'character' but found '%s'.\n", VTypes::dataType(cmd_.argt(count)));
				cmd_.arg(count)->set(va_arg(vars, const char*));
				break;
			default:
				printf("Invalid argument specifier '%c' found when poking StaticCommandNode.\n", *c);
				break;
		}
		count++;
	}
	va_end(vars);
}

// Execute command
int StaticCommandNode::execute()
{
	CommandNode *cmdPointer = &cmd_;
	return cmd_.execute(cmdPointer, TRUE);
}