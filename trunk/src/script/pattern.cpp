/*
	*** Script pattern functions
	*** src/script/pattern.cpp

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
#include "classes/pattern.h"

// Pattern-related script commands
bool script::command_pattern(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_pattern");
	bool result = TRUE;
	pattern *p;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_pattern");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_CREATEPATTERNS):	// Autocreate pattern definition ('createpatterns')
			m->autocreate_patterns();
			break;
		case (SC_CLEARPATTERNS):	// Clear current pattern definition ('clearpatterns')
			m->clear_patterns();
			break;
		case (SC_ADDPATTERN):		// Add manual pattern definition ('addpattern <name> <nmols> <natoms>')
			m->add_pattern(cmd->datavar[1]->get_as_int(), cmd->datavar[2]->get_as_int(), cmd->datavar[0]->get_as_char());
			// TODO Add 'check_pattern(pattern*) method to model*
			break;
		case (SC_PRINTPATTERNS):	// Print pattern definition for current model ('printpatterns')
			msg(DM_NONE,"Pattern info for model '%s':\n",m->get_name());
			m->autocreate_patterns();
			p = m->get_patterns();
			p != NULL ? printf("  ID  nmols  starti  finali  name\n") : printf("None.\n");
			while (p != NULL)
			{
				msg(DM_NONE,"  %2i  %5i  %6i  %6i  %s\n",p->get_id(),p->get_nmols(), p->get_startatom(), p->get_endatom(), p->get_name());
				p = p->next;
			}
			break;
		case (SC_SELECTPATTERN):	// Select working pattern from model ('selectpattern <name>')
			p = m->find_pattern(cmd->datavar[0]->get_as_char());
			if (p != NULL) activepattern = p;
			else result = FALSE;
			break;
		default:
			printf("Error - missed pattern command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_pattern");
	return result;
}
