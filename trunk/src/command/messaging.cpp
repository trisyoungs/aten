/*
	*** Messaging command functions
	*** src/command/messaging.cpp
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
#include "parse/format.h"

// Write line to msg output and stop
int CommandData::function_CA_ERROR(Command *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL) printf("Warning - No format defined in 'error' command.\n");
	else msg.print("%s\n",fmt->createString());
	return CR_EXITWITHERROR;
}

// Print formatted string
int CommandData::function_CA_PRINT(Command *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL) printf("Warning - No format defined in 'print' command.\n");
	else msg.print("%s\n",fmt->createString());
	return CR_SUCCESS;
}

// Print formatted string (in verbose output only)
int CommandData::function_CA_VERBOSE(Command *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL) printf("Warning - No format defined in 'verbose' command.\n");
	else msg.print(Messenger::Verbose,"%s\n",fmt->createString());
	return CR_SUCCESS;
}

// Write line to msg output
int CommandData::function_CA_WARN(Command *&c, Bundle &obj)
{
	Format *fmt = c->format();
	if (fmt == NULL) printf("Warning - No format defined in 'error' command.\n");
	else msg.print("Warning: %s\n",fmt->createString());
	return CR_SUCCESS;
}
