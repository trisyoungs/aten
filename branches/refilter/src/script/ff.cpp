/*
	*** Script forcefield functions
	*** src/script/ff.cpp
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
#include "base/master.h"
#include "base/prefs.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"

// Forcefield-related script commands (root=SR_FF)
bool script::command_ff(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_ff");
	bool result = TRUE;
	forcefield *ff;
	ffatom *ffa;
	model *m;
	switch (cmd->get_command())
	{
		// Load forcefield ('loadff <name> <filename>')
		case (SC_LOADFF):
			ff = master.load_ff(cmd->datavar[1]->get_as_char());
			if (ff != NULL)
			{
				ff->set_name(cmd->datavar[0]->get_as_char());
				master.set_currentff(ff);
				msg(DM_NONE,"Forcefield '%s' loaded, name '%s'\n",cmd->datavar[1]->get_as_char(),cmd->datavar[0]->get_as_char());
			}
			else result = FALSE;
			break;
		// Select current forcefield ('selectff <name>')
		case (SC_SELECTFF):
			ff = master.find_ff(cmd->datavar[0]->get_as_char());
			if (ff != NULL) master.set_currentff(ff);
			else
			{
				msg(DM_NONE,"Forcefield '%s' is not loaded.\n");
				result = FALSE;
			}
			break;
		// Associate current ff to current model ('ffmodel')
		case (SC_FFMODEL):
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL) m->set_ff(master.get_currentff());
			else result = FALSE;
			break;
		// Set current forcefield for named pattern ('ffpattern')
		case (SC_FFPATTERN):
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL)
			{
				if (check_activepattern(text_from_SC(cmd->get_command()))) activepattern->set_ff(master.get_currentff());
				else result = FALSE;
			}
			else result = FALSE;
			break;
		// Set current forcefield for pattern id given ('ffpatternid <id>')
		case (SC_FFPATTERNID):
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL)
			{
				int nodeid = cmd->datavar[0]->get_as_int();
				if ((nodeid < 0) || (nodeid > m->get_npatterns()))
				{
					msg(DM_NONE,"Pattern ID %i is out of range for model (which has %i patterns).\n",nodeid,m->get_npatterns());
					result = FALSE;
				}
				else m->plist[nodeid]->set_ff(master.get_currentff());
			}
			else result = FALSE;
			break;
		// Test specified type ID of current forcefield
		case (SC_TYPETEST):
			m = check_activemodel(text_from_SC(cmd->get_command()));
			ff = master.get_currentff();
			if ((m == NULL) || (ff == NULL))
			{
				result = FALSE;
				break;
			}
			// Find the specified type...
			ffa = ff->find_type(cmd->datavar[0]->get_as_int());
			if (ffa == NULL)
			{
				msg(DM_NONE,"Type ID %i does not exist in the forcefield '%s'.\n",cmd->datavar[0]->get_as_int(),ff->get_name());
				result = FALSE;
			}
			else
			{
				if (m->autocreate_patterns())
				{
					// Prepare for typing
					m->describe_atoms();
					// Get atom, element, and the atom's pattern
					atom *i = m->get_staticatoms()[cmd->datavar[1]->get_as_int()-1];
					int el = i->get_element();
					pattern *p = m->get_pattern(i);
					int score = ffa->get_atomtype()->match_atom(i,p->get_ringlist(),m,i);
					if (score != 0) msg(DM_NONE,"Atom %i matched type %i (%s) with score %i.\n", i->get_id()+1, ffa->get_ffid(), ffa->get_name(), score);
					else msg(DM_NONE,"Atom %i did not match type %i (%s).\n", i->get_id()+1, ffa->get_ffid(), ffa->get_name());
				}
				else result = FALSE;
			}
			break;
		// Perform typing on current model
		case (SC_TYPEMODEL):
			m = check_activemodel(text_from_SC(cmd->get_command()));
			if (m != NULL) result = m->type_all();
			else result = FALSE;
			break;
		default:
			printf("Error - missed ff command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_ff");
	return result;
}

