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

#include "command/commandlist.h"
#include "base/master.h"
#include "base/debug.h"
#include "base/elements.h"
#include "classes/forcefield.h"
#include "parse/filter.h"
#include "model/model.h"

// Draw unbound atom ('newatom <el> [x y z]')
int CommandData::function_CA_NEWATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Determine element (based on type of variable provided)
	Namemap<int> *nm;
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
			// Attempt conversion of the stnmng first from the users type list
			for (nm = master.typeMap.first(); nm != NULL; nm = nm->next)
				if (strcmp(nm->name(),c->argc(0)) == 0) break;
			if (nm == NULL) el = elements.find(c->argc(0));
			else el = nm->data();
			break;
		case (VT_ATOM):
			c->arga(0) == NULL ? el = 0 : c->arga(0)->element();
			break;
		default:
			msg(Debug::None,"Type '%s' is not a valid one to pass to CA_ADDATOM.\n", text_from_VT(c->argt(0)));
			el = 0;
			break;
	}
	if (c->hasArg(3)) master.current.i = obj.m->addAtom(el, c->arg3d(1));
	else master.current.i = obj.m->addAtom(el, c->parent()->penPosition);
	return CR_SUCCESS;
}

// Draw unbound atom ('newatom <el> [fracx fracy fracz]')
int CommandData::function_CA_NEWATOMFRAC(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
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
			c->arga(0) == NULL ? el = 0 : c->arga(0)->element();
			break;
		default:
			msg(Debug::None,"Type '%s' is not a valid one to pass to CA_ADDATOM.\n", text_from_VT(c->argt(0)));
			el = 0;
			break;
	}
	// Check for presence of unit cell
	Vec3<double> r = c->arg3d(1);
	if (obj.m->cell()->type() == CT_NONE) msg(Debug::None,"Warning: No unit cell present - atom added with supplied coordinates.\n");
	else r = obj.m->cell()->fracToReal(r);
	master.current.i = obj.m->addAtom(el, r);
	return CR_SUCCESS;
}

// Draw atom with bond to alst atom ('chain <el>')
int CommandData::function_CA_CHAIN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Atom *i = obj.m->addAtom(elements.find(c->argc(0),Prefs::AlphaZmap), c->parent()->penPosition);
	if (obj.i != NULL) obj.m->bondAtoms(obj.i,i,Bond::Single);
	master.current.i = i;
	return CR_SUCCESS;
}

// Set current atom charge
int CommandData::function_CA_SETCHARGE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setCharge(c->argd(0));
	return CR_SUCCESS;
}

// Set current atom coordinates
int CommandData::function_CA_SETCOORDS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.m->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.m->positionAtom(obj.i, c->arg3d(0));
	return CR_SUCCESS;
}

// Set current atom element
int CommandData::function_CA_SETELEMENT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setElement(elements.find(c->argc(0)));
	return CR_SUCCESS;
}

// Set current atom forces
int CommandData::function_CA_SETFORCES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.m->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x force
int CommandData::function_CA_SETFX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y force
int CommandData::function_CA_SETFY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z force
int CommandData::function_CA_SETFZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom ID
int CommandData::function_CA_SETID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setId(c->argi(0));
	return CR_SUCCESS;
}

// Set current atom x coordinate
int CommandData::function_CA_SETRX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y coordinate
int CommandData::function_CA_SETRY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z coordinate
int CommandData::function_CA_SETRZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom velocities
int CommandData::function_CA_SETVELOCITIES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.m->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x velocity
int CommandData::function_CA_SETVX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y velocity
int CommandData::function_CA_SETVY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z velocity
int CommandData::function_CA_SETVZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.m->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(2,c->argd(0));
	return CR_SUCCESS;
}
