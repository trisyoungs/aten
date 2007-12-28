/*
	*** Messaging command functions
	*** src/command/transform.cpp
	Copyright T. Youngs 2007

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
int command_functions::function_CA_ERROR(command *&c, bundle &obj)
{
	msg(DM_NONE,"Filter Error: %s\n",c->argc(0));
	return CR_EXIT;
}

// Print formatted string
int command_functions::function_CA_PRINT(command *&c, bundle &obj)
{
	Call format->create_string() here
	msg(DM_NONE,"%s\n",printstr.get());
	return CR_SUCCESS;
}

// Write line to msg output
int command_functions::function_CA_WARN(command *&c, bundle &obj)
{
	msg(DM_NONE,"Filter Warning: %s\n",c->argc(0));
	return CR_SUCCESS;
}
