/*
	*** Forcefield command functions
	*** src/command/ff.cpp
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
#include "base/master.h"
#include "base/prefs.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"

// Associate current ff to current model ('ffmodel')
int command_functions::function_CA_FFMODEL(command *&c, bundle &obj)
{
	obj.m->set_ff(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for named pattern ('ffpattern')
int command_functions::function_CA_FFPATTERN(command *&c, bundle &obj)
{
	obj.p->set_ff(obj.ff);
	return CR_SUCCESS;
}

// Set current forcefield for pattern id given ('ffpatternid <id>')
int command_functions::function_CA_FFPATTERNID(command *&c, bundle &obj)
{
	int nodeid = c->argi(0) - 1;
	if ((nodeid < 0) || (nodeid > obj.m->get_npatterns()))
	{
		msg(DM_NONE,"Pattern ID %i is out of range for model (which has %i atterns).\n", nodeid, obj.m->get_npatterns());
		return CR_FAIL;
	}
	else obj.m->get_pattern(nodeid)->set_ff(obj.ff);
	return CR_SUCCESS;
}

// Load forcefield ('loadff <filename> [nickname]')
int command_functions::function_CA_LOADFF(command *&c, bundle &obj)
{
	forcefield *ff = master.load_ff(c->argc(1));
	if (ff != NULL)
	{
		master.set_currentff(ff);
		if (c->has_argument(1)) ff->set_name(c->argc(1));
		msg(DM_NONE,"Forcefield '%s' loaded, name '%s'\n", c->argc(0), ff->get_name());
		return CR_SUCCESS;
	}
	else return CR_FAIL;
}

// Select current forcefield ('selectff <name>')
int command_functions::function_CA_SELECTFF(command *&c, bundle &obj)
{
	forcefield *ff = master.find_ff(c->argc(0));
	if (ff != NULL)
	{
		master.set_currentff(ff);
		return CR_SUCCESS;
	}
	else
	{
		msg(DM_NONE,"Forcefield '%s' is not loaded.\n", c->argc(0));
		return CR_FAIL;
	}
}

// Perform typing on current model
int command_functions::function_CA_TYPEMODEL(command *&c, bundle &obj)
{
	return (obj.m->type_all() ? CR_SUCCESS : CR_FAIL);
}

// Test specified type ID of current forcefield
int command_functions::function_CA_TYPETEST(command *&c, bundle &obj)
{
	if (obj.ff == NULL)
	{
		msg(DM_NONE,"No forcefield loaded.\n");
		return CR_FAIL;
	}
	// Find the specified type...
	ffatom *ffa = obj.ff->find_type(c->argi(0));
	if (ffa == NULL)
	{
		msg(DM_NONE,"Type ID %i does not exist in the forcefield '%s'.\n",c->argi(0), obj.ff->get_name());
		return CR_FAIL;
	}
	else
	{
		if (obj.m->autocreate_patterns())
		{
			// Prepare for typing
			obj.m->describe_atoms();
			// Get atom, element, and the atom's pattern
			atom *i = obj.m->get_staticatoms()[c->argi(1)-1];
			int el = i->get_element();
			pattern *p = obj.m->get_pattern(i);
			int score = ffa->get_atomtype()->match_atom(i,p->get_ringlist(),obj.m,i);
			if (score != 0) msg(DM_NONE,"Atom %i matched type %i (%s) with score %i.\n", i->get_id()+1, ffa->get_ffid(), ffa->get_name(), score);
			else msg(DM_NONE,"Atom %i did not match type %i (%s).\n", i->get_id()+1, ffa->get_ffid(), ffa->get_name());
		}
		else return CR_FAIL;
	}
	return CR_SUCCESS;
}
