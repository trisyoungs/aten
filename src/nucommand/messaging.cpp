/*
	*** Messaging functions
	*** src/parser/messaging.cpp
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

#include "nucommand/commands.h"
#include "command/format.h"

// Write line to msg output and stop
bool NuCommand::function_Error(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		msg.print("Warning - No format defined in 'error' command.\n");
		return FALSE;
	}
	else if (fmt->createString()) msg.print("%s\n",fmt->createdString());
	else return FALSE;
	return Command::ExitWithError;
}

// Print formatted string
bool NuCommand::function_Print(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'print' command.\n");
		return FALSE;
	}
	else if (fmt->createString()) msg.print("%s\n",fmt->createdString());
	else return FALSE;
	return TRUE;
}

// Print formatted string (in verbose output only)
bool NuCommand::function_Verbose(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'verbose' command.\n");
		return FALSE;
	}
	else if (fmt->createString()) msg.print(Messenger::Verbose,"%s\n", fmt->createdString());
	else return FALSE;
	return TRUE;
}

// Write line to msg output
bool NuCommand::function_Warn(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	Format *fmt = c->format();
	if (fmt == NULL)
	{
		printf("Warning - No format defined in 'error' command.\n");
		return FALSE;
	}
	else if (fmt->createString()) msg.print("Warning: %s\n",fmt->createdString());
	else return FALSE;
	return TRUE;
}
