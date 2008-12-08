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
#include "model/model.h"
#include "base/elements.h"

// Set atom style for current selection
int Command::function_CA_ATOMSTYLE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Atom::DrawStyle ds = Atom::drawStyle(c->argc(0));
	if (ds == Atom::nDrawStyles) return Command::Fail;
	if (c->hasArg(1))
	{
		Atom *i = obj.rs->atom(c->argi(1));
		if (i == NULL) return Command::Fail;
		obj.rs->beginUndoState("Style individual atom");
		obj.rs->styleAtom(i, ds);
		obj.rs->endUndoState();
	}
	else
	{
		obj.rs->beginUndoState("Style atom selection");
		obj.rs->styleSelection(ds);
		obj.rs->endUndoState();
	}
	return Command::Success;
}

// Retrieve atom info ('getatom <id> [var]')
int Command::function_CA_GETATOM(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	Atom *i = obj.rs->atom(c->argi(0)-1);
	if (i == NULL) return Command::Fail;
	// Set atom information
	obj.i = i;
	return Command::Success;
}

// Hide current atom selection
int Command::function_CA_HIDE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->selectionSetHidden(TRUE);
	return Command::Success;
}

// Set current atom charge
int Command::function_CA_SETCHARGE(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->setCharge(c->argd(0));
	return Command::Success;
}

// Set current atom coordinates
int Command::function_CA_SETCOORDS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.rs->positionAtom(obj.i, c->arg3d(0));
	return Command::Success;
}

// Set current atom element
int Command::function_CA_SETELEMENT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->setElement(elements().find(c->argc(0)));
	return Command::Success;
}

// Set current atom forces
int Command::function_CA_SETFORCES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->f() = c->arg3d(0);
	return Command::Success;
}

// Set current atom x force
int Command::function_CA_SETFX(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->f().set(0,c->argd(0));
	return Command::Success;
}

// Set current atom y force
int Command::function_CA_SETFY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->f().set(1,c->argd(0));
	return Command::Success;
}

// Set current atom z force
int Command::function_CA_SETFZ(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->f().set(2,c->argd(0));
	return Command::Success;
}

// Set current atom ID
int Command::function_CA_SETID(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->setId(c->argi(0));
	return Command::Success;
}

// Set current atom x coordinate
int Command::function_CA_SETRX(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->r().set(0,c->argd(0));
	return Command::Success;
}

// Set current atom y coordinate
int Command::function_CA_SETRY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->r().set(1,c->argd(0));
	return Command::Success;
}

// Set current atom z coordinate
int Command::function_CA_SETRZ(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->r().set(2,c->argd(0));
	return Command::Success;
}

// Set current atom velocities
int Command::function_CA_SETVELOCITIES(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(3)) obj.i = obj.rs->atom(c->argi(3) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->v() = c->arg3d(0);
	return Command::Success;
}

// Set current atom x velocity
int Command::function_CA_SETVX(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->v().set(0,c->argd(0));
	return Command::Success;
}

// Set current atom y velocity
int Command::function_CA_SETVY(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->v().set(1,c->argd(0));
	return Command::Success;
}

// Set current atom z velocity
int Command::function_CA_SETVZ(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	if (c->hasArg(1)) obj.i = obj.rs->atom(c->argi(1) - 1);
	if (obj.notifyNull(Bundle::AtomPointer)) return Command::Fail;
	obj.i->v().set(2,c->argd(0));
	return Command::Success;
}

// Show current atom selection
int Command::function_CA_SHOW(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->selectionSetHidden(FALSE);
	return Command::Success;
}

// Show all atoms
int Command::function_CA_SHOWALL(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	for (Atom *i = obj.rs->atoms(); i != NULL; i = i->next) obj.rs->setHidden(i,FALSE);
	return Command::Success;
}
