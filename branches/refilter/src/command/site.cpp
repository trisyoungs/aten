/*
	*** Site command functions
	*** src/command/site.cpp
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
#include "base/debug.h"
#include "classes/pattern.h"
#include "parse/parse.h"

// Add site definition to model ('addsite <name> <pattern> <"atomids...">')
int command_functions::function_CA_ADDSITE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// First, check that the pattern name provided refers to a pattern of the current model
	pattern *p = obj.m->find_pattern(c->argc(1));
	if (p == NULL) return CR_FAIL;
	obj.s = obj.m->sites.add();
	obj.s->set_name(c->argc(0));
	obj.s->set_pattern(p);
	// Parse the atom list which should have been given as: "1,2,3,4,5......"
	if (c->has_arg(2))
	{
		parser.get_args_delim(c->argc(2), PO_DEFAULTS);
		for (int n=0; n<parser.get_nargs(); n++)
		{
			listitem<int> *li = obj.s->atoms.add();
			// Store n-1 since internally we work in 0-n range
			li->data = parser.argi(n) - 1;
		}
	}
	msg(DM_NONE,"New site added for model: '%s', for pattern '%s', %i atoms defined%s", obj.s->get_name(), p->get_name(), obj.s->atoms.size(), (obj.s->atoms.size() == 0 ? " (will use centre of geometry)\n" : "\n"));
	return CR_SUCCESS;
}

// Print site definitions for model ('printsites')
int command_functions::function_CA_PRINTSITES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	site *s = obj.m->sites.first();
	if (s == NULL) msg(DM_NONE,"No sites defined for model '%s'.\n",obj.m->get_name());
	else
	{
		msg(DM_NONE,"Site list for model '%s':\n",obj.m->get_name());
		for (s = s; s != NULL; s = s->next)
		{
			msg(DM_NONE," %15s %15s  ",s->get_name(), s->get_pattern()->get_name());
			if (s->atoms.size() == 0) msg(DM_NONE,"All atoms assumed (none defined)");
			else for (listitem<int> *li = s->atoms.first(); li != NULL; li = li->next)msg(DM_NONE," %i",li->data);
			msg(DM_NONE,"\n");
		}
	}
	return CR_SUCCESS;
}

int command_functions::function_CA_SELECTSITE(command *&c, bundle &obj)
{
	return CR_FAIL;
}

// Set x and y-axis definitions for current site ('setaxes <"X-atomids..."> <"Y-atomids">')
int command_functions::function_CA_SETAXES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_SITE)) return CR_FAIL;
	int n;
	listitem<int> *li;
	// Parse atom list for x-axis
	parser.get_args_delim(c->argc(0), PO_DEFAULTS);
	for (n=0; n<parser.get_nargs(); n++)
	{
		li = obj.s->xaxisatoms.add();
		// Store n-1 since internally we work in 0-n range
		li->data = parser.argi(n) - 1;
	}
	// Parse atom list for y-axis
	parser.get_args_delim(c->argc(1), PO_DEFAULTS);
	for (n=0; n<parser.get_nargs(); n++)
	{
		li = obj.s->yaxisatoms.add();
		// Store n-1 since internally we work in 0-n range
		li->data = parser.argi(n) - 1;
	}
	return CR_SUCCESS;
}
