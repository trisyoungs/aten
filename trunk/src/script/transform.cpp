/*
	*** Script transformation functions
	*** src/script/transform.cpp

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

#include "script/script.h"
#include "base/debug.h"

// Model transform script commands
bool script::command_transform(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_translate");
	bool result = TRUE;
	atom *i;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_translate");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		// Translate activeatom ('translateatom <dx dy dz>')
		case (SC_TRANSLATEATOM):
			if (check_activeatom(text_from_SC(cmd->get_command()))) activeatom->r += cmd->get_vector3d(0);
			else return FALSE;
			break;
		// Translate selection ('translate <dx dy dz>')
		case (SC_TRANSLATESELECTION):
			m->translate_selection_local(cmd->get_vector3d(0));
			break;
		// Mirror selection along specified axis
		case (SC_MIRRORSELECTION):
			m->mirror_selection_local(cmd->datavar[0]->get_as_int());
			break;
		default:
			printf("Error - missed translate command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_translate");
	return result;
}
