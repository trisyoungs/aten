/*
	*** Script field functions
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

// Field-related script commands (root=SR_FIELD)
bool script::command_field(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_field");
	bool result = TRUE;
	atom *i;
	filter *f;
	model *m = check_activemodel("Field commands");
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_field");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_SAVEFIELD):	// Save field definition ('savefield <filename>')
			savedlpfield(cmd->argc(0),m);
			break;
		case (SC_SAVEFIELD2):	// NEW Save field definition ('savefield2 <format> <file>')
			// Find filter with a nickname matching that given in argc(0)
			for (f = master.filters[FT_FIELD_EXPORT].first(); f != NULL; f = f->next)
			{
				//printf("Checking %s against %s\n",f->get_nickname(),cmd->argc(0));
				if (strcmp(f->get_nickname(), cmd->argc(0)) == 0) break;
			}
			// Check that a suitable format was found
			if (f == NULL)
			{
				msg(DM_NONE,"script : No field export filter was found that matches the extension '%s'.\nNot saved.\n",cmd->argc(0));
				result = FALSE;
				break;
			}
			f->export_field(m,cmd->argc(1));
			break;
		default:
			printf("Error - missed field command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_field");
	return result;
}
