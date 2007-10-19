/*
	*** Script selection functions
	*** src/script/select.cpp
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

#include "script/script.h"
#include "base/elements.h"
#include "base/sysfunc.h"
#include "base/debug.h"
#include "classes/forcefield.h"

// Selection-related script commands
bool script::command_select(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_select");
	bool result = TRUE;
	atom *i;
	ffatom *ffa;
	forcefield *ff;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_select");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		// Select all ('selectall')
		case (SC_SELECTALL):
			m->select_all();
			break;
		// Select by atom ('selectatom <n>')
		case (SC_SELECTATOM):
			i = m->find_atom(cmd->datavar[0]->get_as_int());
			if (i != NULL) m->select_atom(i);
			break;
		// Select by element ('selectelement <el>')
		case (SC_SELECTELEMENT):
			m->select_element(elements.find(cmd->datavar[0]->get_as_char(),ZM_ALPHA));
			break;
		// Select by forcefield type ('selecttype <fftype>')
		case (SC_SELECTTYPE):
			for (i = m->get_atoms(); i != NULL; i = i->next)
			{
				ff = m->get_ff();
				if (ff == NULL) break;
				ffa = i->get_fftype();
				if (ffa != NULL)
				{
					if (ff->match_type(ffa->get_name(),cmd->datavar[0]->get_as_char()) != 0) m->select_atom(i);
				}
			}
		// Select no atoms ('selectnone')
		case (SC_SELECTNONE):
			m->select_none();
			break;
 		//case (SC_SELECT_ATOMTYPE):
		// Invert selection
		case (SC_SELECTINVERT):
			m->selection_invert();
			break;
		default:
			printf("Error - missed select command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_select");
	return result;
}

