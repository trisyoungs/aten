/*
	*** Atom command functions
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
#include "base/aten.h"
#include "base/messenger.h"
#include "base/elements.h"
#include "classes/forcefield.h"
#include "parse/filter.h"
#include "model/model.h"

// Set atom style for current selection
int CommandData::function_CA_ATOMSTYLE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds != Atom::nDrawStyles) for (Atom *i = obj.rs->firstSelected(); i != NULL; i = i->nextSelected()) i->setStyle(ds);
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Retrieve atom info ('getatom <id> [var]')
int CommandData::function_CA_GETATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Atom *i = obj.rs->atom(c->argi(0)-1);
	if (i == NULL) return CR_FAIL;
	// Set atom information
	obj.i = i;
	if (c->hasArg(1)) c->parent()->setAtomVariables(c->arg(1)->name(), i);
	return CR_SUCCESS;
}

// Hide current atom selection
int CommandData::function_CA_HIDE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectionSetHidden(TRUE);
	return CR_SUCCESS;
}

// Set current atom charge
int CommandData::function_CA_SETCHARGE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setCharge(c->argd(0));
	return CR_SUCCESS;
}

// Set current atom coordinates
int CommandData::function_CA_SETCOORDS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.rs->positionAtom(obj.i, c->arg3d(0));
	return CR_SUCCESS;
}

// Set current atom element
int CommandData::function_CA_SETELEMENT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setElement(elements.find(c->argc(0)));
	return CR_SUCCESS;
}

// Set current atom forces
int CommandData::function_CA_SETFORCES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x force
int CommandData::function_CA_SETFX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y force
int CommandData::function_CA_SETFY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z force
int CommandData::function_CA_SETFZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->f().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom ID
int CommandData::function_CA_SETID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->setId(c->argi(0));
	return CR_SUCCESS;
}

// Set current atom x coordinate
int CommandData::function_CA_SETRX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y coordinate
int CommandData::function_CA_SETRY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z coordinate
int CommandData::function_CA_SETRZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->r().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom velocities
int CommandData::function_CA_SETVELOCITIES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v() = c->arg3d(0);
	return CR_SUCCESS;
}

// Set current atom x velocity
int CommandData::function_CA_SETVX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(0,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom y velocity
int CommandData::function_CA_SETVY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(1,c->argd(0));
	return CR_SUCCESS;
}

// Set current atom z velocity
int CommandData::function_CA_SETVZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(BP_ATOM)) return CR_FAIL;
	obj.i->v().set(2,c->argd(0));
	return CR_SUCCESS;
}

// Show current atom selection
int CommandData::function_CA_SHOW(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.rs->selectionSetHidden(FALSE);
	return CR_SUCCESS;
}

// Show all atoms
int CommandData::function_CA_SHOWALL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) obj.rs->setHidden(i,FALSE);
	return CR_SUCCESS;
}
