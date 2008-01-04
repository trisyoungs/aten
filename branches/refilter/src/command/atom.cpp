/*
	*** atom command functions
	*** src/command/atom.cpp
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

#include "command/commands.h"
#include "base/master.h"
#include "base/debug.h"
#include "base/elements.h"
#include "classes/forcefield.h"
#include "parse/filter.h"

// Set current atom charge
int command_functions::function_CA_SETCHARGE(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_charge(c->argd(0));
	return CR_SUCCESS;
}

// Set current atom coordinates
int command_functions::function_CA_SETCOORDS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom element
int command_functions::function_CA_SETELEMENT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_element(elements.find(c->argc(0)));
	return CR_SUCCESS;
}

// Set current atom forces
int command_functions::function_CA_SETFORCES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x force
int command_functions::function_CA_SETFX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y force
int command_functions::function_CA_SETFY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z force
int command_functions::function_CA_SETFZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom ID
int command_functions::function_CA_SETID(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->set_id(c->argi(0));
	return CR_SUCCESS;
}

// Set current atom x coordinate
int command_functions::function_CA_SETRX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y coordinate
int command_functions::function_CA_SETRY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z coordinate
int command_functions::function_CA_SETRZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom velocities
int command_functions::function_CA_SETVELOCITIES(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x velocity
int command_functions::function_CA_SETVX(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y velocity
int command_functions::function_CA_SETVY(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z velocity
int command_functions::function_CA_SETVZ(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	if (c->has_arg(1)) obj.i = obj.m->get_atom(c->argi(1) - 1);
	if (obj.notify_null(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(2,c->argd(0));
	return CR_SUCCESS;
}
