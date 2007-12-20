/*
	*** Field command functions
	*** src/command/field.cpp
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

#include "command/commands.h"
#include "base/master.h"
#include "base/debug.h"
#include "file/temp_dlpfield.h"
#include "file/filter.h"

// Save field definition ('savefield <filename>')
int command_functions::function_CA_SAVEFIELD(command *&c, bundle &obj)
{
	savedlpfield(c->argc(0),obj.m);
	return CR_SUCCESS;
}

// Save field definition ('savefield2 <format> <file>')
int command_functions::function_CA_SAVEFIELD2(command *&c, bundle &obj)
{
	// Find filter with a nickname matching that given in argc(0)
	filter *f;
	for (f = master.filters[FT_FIELD_EXPORT].first(); f != NULL; f = f->next)
	{
		//printf("Checking %s against %s\n",f->get_nickname(),cmd->argc(0));
		if (strcmp(f->get_nickname(), c->argc(0)) == 0) break;
	}
	// Check that a suitable format was found
	if (f == NULL)
	{
		msg(DM_NONE,"script : No field export filter was found that matches the extension '%s'.\nNot saved.\n",c->argc(0));
		return CR_FAIL;
	}
	f->execute_with_model(obj.m, c->argc(1));
	return CR_SUCCESS;
}
