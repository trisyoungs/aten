/*
	*** atom command functions
	*** src/command/atom.cpp
	Copyright T. Youngs 2007,2008

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
#include "base/elements.h"
#include "classes/forcefield.h"
#include "parse/filter.h"

// Draw unbound atom ('addatom <el> [x y z]')
int commanddata::function_CA_ADDATOM(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// Determine element (based on type of variable provided)
	int el;
	switch (c->argt(0))
	{
		case (VT_INTEGER):
			el = c->argi(0);
			break;
		case (VT_DOUBLE):
			el = (int) floor(c->argd(0) + 0.15);
			break;
		case (VT_CHAR):
			el = elements.find(c->argc(0));
			break;
		case (VT_ATOM):
			c->arga(0) == NULL ? el = 0 : c->arga(0)->get_element();
			break;
		default:
			msg(DM_NONE,"Type '%s' is not a valid one to pass to CA_ADDATOM.\n", text_from_VT(c->argt(0)));
			el = 0;
			break;
	}
	if (c->has_arg(3)) master.current.i = obj.m->add_atom(el, c->arg3d(1));
	else master.current.i = obj.m->add_atom(el, c->get_parent()->penpos);
	return CR_SUCCESS;
}

// Draw atom with bond to 'activeatom' ('addchain <el>')
int commanddata::function_CA_ADDCHAIN(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	atom *i = obj.m->add_atom(elements.find(c->argc(0),ZM_ALPHA), c->get_parent()->penpos);
	if (obj.i != NULL) obj.m->bond_atoms(obj.i,i,BT_SINGLE);
	master.current.i = i;
	return CR_SUCCESS;
}

// Set current atom charge
int commanddata::function_CA_SETCHARGE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_charge(c->argd(0));
	return CR_SUCCESS;
}

// Set current atom coordinates
int commanddata::function_CA_SETCOORDS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(3)) obj.i = obj.m->get_atom(c->argi(3) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.m->position_atom(obj.i, c->arg3d(0));
	return CR_SUCCESS;
}

// Set current atom element
int commanddata::function_CA_SETELEMENT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_element(elements.find(c->argc(0)));
	return CR_SUCCESS;
}

// Set current atom forces
int commanddata::function_CA_SETFORCES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(3)) obj.i = obj.m->get_atom(c->argi(3) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x force
int commanddata::function_CA_SETFX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y force
int commanddata::function_CA_SETFY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z force
int commanddata::function_CA_SETFZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom ID
int commanddata::function_CA_SETID(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_id(c->argi(0));
	return CR_SUCCESS;
}

// Set current atom x coordinate
int commanddata::function_CA_SETRX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y coordinate
int commanddata::function_CA_SETRY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z coordinate
int commanddata::function_CA_SETRZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom velocities
int commanddata::function_CA_SETVELOCITIES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(3)) obj.i = obj.m->get_atom(c->argi(3) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x velocity
int commanddata::function_CA_SETVX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y velocity
int commanddata::function_CA_SETVY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z velocity
int commanddata::function_CA_SETVZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(2,c->argd(0));
	return CR_SUCCESS;
}
