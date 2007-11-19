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
	static atom *i;
        static int count, el, matchscore, atomscore, n;
	static atomtype *testat;
	static pattern *p;
	static ffatom *ffa;
	static forcefield *ff;
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
		// Select by forcefield type ('selecffttype <fftype>')
		case (SC_SELECTFFTYPE):
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
		// Select by supplied atom type description ('selecttype <el> <typedesc>')
 		case (SC_SELECTTYPE):
			testat = new atomtype();
			testat->el = elements.find(cmd->datavar[0]->get_as_char());
			testat->expand(cmd->datavar[1]->get_as_char(),NULL,NULL);
			// Apply it to the atoms in the model, selecting atoms that match
			count = 0;
			matchscore = 0;
			if (m->autocreate_patterns())
			{
				// Prepare for typing
				m->describe_atoms();
				// Loop over patterns and select atoms
				p = m->get_patterns();
				while (p != NULL)
				{
					i = p->get_firstatom();
					for (n=0; n<p->get_natoms(); n++)
					{
						p->reset_tempi(0);
						i->tempi = 1;
						if (i->get_element() == testat->el)
						{
							atomscore = testat->match_atom(i,p->get_ringlist(),m,i);
							if (atomscore != 0)
							{
								m->select_atom(i);
								count ++;
								matchscore = atomscore;
							}
						}
						i = i->next;
					}
					p = p->next;
				}
				// Write results
				msg(DM_NONE,"Type description score = %i. Matched %i atoms.\n", matchscore, count);
				// Update model and delete temporary atomtype
				m->log_change(LOG_SELECTION);
				delete testat;
			}
			else msg(DM_NONE,"Can't test atomtype description without a valid pattern definition!\n");
			break;
		// Invert selection
		case (SC_SELECTINVERT):
			m->selection_invert();
			break;
		// Detect and select overlapping atoms
		case (SC_SELECTOVERLAPS):
			m->select_overlaps(cmd->datavar[0]->get_as_double());
			break;
		default:
			printf("Error - missed select command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_select");
	return result;
}

