/*
	*** Field command functions
	*** src/command/field.cpp
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

#include "command/commands.h"
#include "base/master.h"
#include "base/debug.h"
#include "parse/filter.h"

// Save field definition ('savefield <format> <file>')
int commanddata::function_CA_SAVEFIELD(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
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
	f->execute(c->argc(1));
	return CR_SUCCESS;
}
