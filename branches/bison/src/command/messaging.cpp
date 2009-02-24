/*
	*** Messaging command functions
	*** src/command/messaging.cpp
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

#include "command/commandlist.h"
#include "command/format.h"

// Write line to msg output and stop
int Command::function_CA_ERROR(CommandNode *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		msg.print("Warning - No format defined in 'error' command.\n");
		return Command::Fail;
	}
	else if (fmt->createString()) msg.print("%s\n",fmt->createdString());
	else return Command::Fail;
	return Command::ExitWithError;
}

// Print formatted string
int Command::function_CA_PRINT(CommandNode *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'print' command.\n");
		return Command::Fail;
	}
	else if (fmt->createString()) msg.print("%s\n",fmt->createdString());
	else return Command::Fail;
	return Command::Success;
}

// Print formatted string (in verbose output only)
int Command::function_CA_VERBOSE(CommandNode *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'verbose' command.\n");
		return Command::Fail;
	}
	else if (fmt->createString()) msg.print(Messenger::Verbose,"%s\n", fmt->createdString());
	else return Command::Fail;
	return Command::Success;
}

// Write line to msg output
int Command::function_CA_WARN(CommandNode *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'error' command.\n");
		return Command::Fail;
	}
	else if (fmt->createString()) msg.print("Warning: %s\n",fmt->createdString());
	else return Command::Fail;
	return Command::Success;
}
