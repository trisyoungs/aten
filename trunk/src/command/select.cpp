/*
	*** Selection command functions
	*** src/command/select.cpp
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
#include "model/model.h"
#include "base/elements.h"
#include "base/sysfunc.h"
#include "base/debug.h"
#include "classes/forcefield.h"
#include "classes/pattern.h"

// Select all ('selectall')
int command_functions::function_CA_SELECTALL(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->select_all();
	return CR_SUCCESS;
}

// Select by atom ('selectatom <n>')
int command_functions::function_CA_SELECTATOM(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atom *i = obj.m->get_atom(c->argi(0));
	if (i != NULL) obj.m->select_atom(i);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Select by element ('selectelement <el>')
int command_functions::function_CA_SELECTELEMENT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	int el = elements.find(c->argc(0), ZM_ALPHA);
	for (atom *i = obj.m->get_atoms(); i != NULL; i = i->next) if (i->get_element() == el) obj.m->select_atom(i);
	return CR_SUCCESS;
}

// Select by forcefield type ('selecffttype <fftype>')
int command_functions::function_CA_SELECTFFTYPE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	forcefield *ff = obj.m->get_ff();
	if (ff == NULL)
	{
		msg(DM_NONE,"No forcefield associated to model.\n");
		return CR_FAIL;
	}
	ffatom *ffa;
	for (atom *i = obj.m->get_atoms(); i != NULL; i = i->next)
	{
		ffa = i->get_type();
		if (ffa != NULL)
		{
			if (ff->match_type(ffa->get_name(),c->argc(0)) != 0) obj.m->select_atom(i);
		}
	}
	return CR_SUCCESS;
}

// Invert selection
int command_functions::function_CA_SELECTINVERT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->selection_invert();
	return CR_SUCCESS;
}

// Select no atoms ('selectnone')
int command_functions::function_CA_SELECTNONE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->select_none();
	return CR_SUCCESS;
}

// Detect and select overlapping atoms
int command_functions::function_CA_SELECTOVERLAPS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->select_overlaps(c->argd(0));
	return CR_SUCCESS;
}

// Select by supplied atom type description ('selecttype <el> <typedesc>')
int command_functions::function_CA_SELECTTYPE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atomtype testat;
	testat.el = elements.find(c->argc(0));
	testat.expand(c->argc(1),NULL,NULL);
	// Apply it to the atoms in the model, selecting atoms that match
	int count = 0, matchscore, atomscore;
	if (obj.m->autocreate_patterns())
	{
		// Prepare for typing
		obj.m->describe_atoms();
		// Loop over patterns and select atoms
		for (pattern *p = obj.m->get_patterns(); p != NULL; p = p->next)
		{
			atom *i = p->get_firstatom();
			for (int n=0; n<p->get_totalatoms(); n++)
			{
				p->reset_tempi(0);
				i->tempi = 1;
				if (i->get_element() == testat.el)
				{
					atomscore = testat.match_atom(i,p->get_ringlist(),obj.m,i);
					if (atomscore != 0)
					{
						obj.m->select_atom(i);
						count ++;
						matchscore = atomscore;
					}
				}
				i = i->next;
			}
		}
		// Write results
		msg(DM_NONE,"Type description score = %i. Matched %i atoms.\n", matchscore, count);
		// Update model and delete temporary atomtype
		obj.m->log_change(LOG_SELECTION);
		return CR_SUCCESS;
	}
	else msg(DM_NONE,"Can't test atomtype description without a valid pattern definition!\n");
	return CR_FAIL;
}
