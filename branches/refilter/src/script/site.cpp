/*
	*** Script molecule site functions
	*** src/script/site.cpp
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
#include "base/debug.h"
#include "classes/pattern.h"

// Site-related script commands (root=SR_SITE)
bool script::command_site(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_site");
	bool result = TRUE;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_site");
		return FALSE;
	}
	site *s;
	listitem<int> *li;
	pattern *p;
	int n;
	switch (cmd->get_command())
	{
		// Print site definitions for model ('printsites')
		case (SC_PRINTSITES):
			s = m->sites.first();
			if (s == NULL) msg(DM_NONE,"No sites defined for model '%s'.\n",m->get_name());
			else
			{
				msg(DM_NONE,"Site list for model '%s':\n",m->get_name());
				for (s=s; s != NULL; s = s->next)
				{
					msg(DM_NONE," %15s %15s  ",s->get_name(), s->get_pattern()->get_name());
					if (s->atoms.size() == 0) msg(DM_NONE,"All atoms assumed (none defined)");
					else for (li = s->atoms.first(); li != NULL; li = li->next)msg(DM_NONE," %i",li->data);
					msg(DM_NONE,"\n");
				}
			}
			break;
		// Add site definition to model ('addsite <name> <pattern> <"atomids...">')
		case (SC_ADDSITE):
			// First, check that the pattern name provided refers to a pattern of the current model
			p = m->find_pattern(cmd->datavar[1]->get_as_char());
			if (p == NULL) result = FALSE;
			else
			{
				s = m->sites.add();
				activesite = s;
				s->set_name(cmd->datavar[0]->get_as_char());
				s->set_pattern(p);
				// Parse the atom list which should have been given as: "1,2,3,4,5......"
				if (cmd->datavar[2] != NULL)
				{
					parser.get_args_delim(cmd->datavar[2]->get_as_char(), PO_DEFAULTS);
					for (n=0; n<parser.get_nargs(); n++)
					{
						li = s->atoms.add();
						// Store n-1 since internally we work in 0-n range
						li->data = parser.argi(n) - 1;
					}
				}
				msg(DM_NONE,"New site added for model: '%s', for pattern '%s', %i atoms defined%s", s->get_name(), p->get_name(), s->atoms.size(), (s->atoms.size() == 0 ? " (will use centre of geometry)\n" : "\n"));
			}
			break;
		// Set x and y-axis definitions for current site ('setaxes <"X-atomids..."> <"Y-atomids">')
		case (SC_SETAXES):
			// Check that we have an active site
			s = check_activesite(text_from_SC(cmd->get_command()));
			if (s != NULL)
			{
				// Parse atom list for x-axis
				parser.get_args_delim(cmd->datavar[0]->get_as_char(), PO_DEFAULTS);
				for (n=0; n<parser.get_nargs(); n++)
				{
					li = s->xaxisatoms.add();
					// Store n-1 since internally we work in 0-n range
					li->data = parser.argi(n) - 1;
				}
				// Parse atom list for y-axis
				parser.get_args_delim(cmd->datavar[1]->get_as_char(), PO_DEFAULTS);
				for (n=0; n<parser.get_nargs(); n++)
				{
					li = s->yaxisatoms.add();
					// Store n-1 since internally we work in 0-n range
					li->data = parser.argi(n) - 1;
				}
			}
			break;
		default:
			printf("Error - missed site command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_site");
	return result;
}
