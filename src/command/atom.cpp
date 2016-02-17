/*
	*** Atom Commands
	*** src/command/atom.cpp
	Copyright T. Youngs 2007-2016

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
#include "parser/commandnode.h"
#include "model/bundle.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Set atom style for current selection
bool Commands::function_AtomStyle(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Prefs::DrawStyle ds = Prefs::drawStyle(c->argc(0), true);
	if (ds == Prefs::nDrawStyles) return false;
	if (c->hasArg(1))
	{
		Atom* i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
		if (i == NULL) return false;
		obj.rs()->beginUndoState("Style individual atom");
		obj.rs()->atomSetStyle(i, ds);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Style atom selection");
		obj.rs()->selectionSetStyle(ds);
		obj.rs()->endUndoState();
	}
	rv.reset();
	return true;
}

// Set custom colours of selected atoms
bool Commands::function_ColourAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Set custom colour of %i atoms", obj.rs()->nSelected());
	obj.rs()->selectionSetColour(c->argd(0), c->argd(1), c->argd(2), (c->hasArg(3) ? c->argd(3) : 1.0));
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Set current atom ('currentatom <id>')
bool Commands::function_CurrentAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* i = c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0,VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1);
	if (i == NULL) return false;
	// Set atom information
	obj.i = i;
	rv.set(VTypes::AtomData, i);
	return true;
}

// Fix positions of current atom selection or specified atom
bool Commands::function_Fix(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(0))
	{
		Atom* i = c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0,VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1);
		obj.rs()->beginUndoState("Fix position of single atom");
		obj.rs()->atomSetFixed(i, true);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Fix positions of %i atoms", obj.rs()->nSelected());
		obj.rs()->selectionSetFixed(true);
		obj.rs()->endUndoState();
	}
	rv.reset();
	return true;
}

// Free positions of current atom selection or specified atom
bool Commands::function_Free(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(0))
	{
		Atom* i = c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0,VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1);
		obj.rs()->beginUndoState("Free position of single atom");
		obj.rs()->atomSetFixed(i, false);
		obj.rs()->endUndoState();
	}
	else
	{
		obj.rs()->beginUndoState("Free positions of %i atoms", obj.rs()->nSelected());
		obj.rs()->selectionSetFixed(false);
		obj.rs()->endUndoState();
	}
	rv.reset();
	return true;
}

// Retrieve atom info ('getatom <id>')
bool Commands::function_GetAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	Atom* i = c->argType(0) == VTypes::AtomData ? (Atom*) c->argp(0,VTypes::AtomData) : obj.rs()->atom(c->argi(0)-1);
	if (i == NULL) return false;
	rv.set(VTypes::AtomData, i);
	return true;
}

// Hide current atom selection
bool Commands::function_Hide(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Hide %i atoms", obj.rs()->nSelected());
	obj.rs()->selectionSetHidden(true);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Set custom colours of selected atoms
bool Commands::function_RecolourAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Reset custom colour of %i atoms", obj.rs()->nSelected());
	obj.rs()->selectionResetColour();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Set current atom charge
bool Commands::function_SetCharge(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->setCharge(c->argd(0));
	rv.reset();
	return true;
}

// Set current atom coordinates
bool Commands::function_SetCoords(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(3)) obj.i = c->argType(3) == VTypes::AtomData ? (Atom*) c->argp(3,VTypes::AtomData) : obj.rs()->atom(c->argi(3)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.rs()->positionAtom(obj.i, c->arg3d(0));
	rv.reset();
	return true;
}

// Set current atom element
bool Commands::function_SetElement(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->setElement(c->argz(0));
	rv.reset();
	return true;
}

// Set current atom forces
bool Commands::function_SetForces(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(3)) obj.i = c->argType(3) == VTypes::AtomData ? (Atom*) c->argp(3,VTypes::AtomData) : obj.rs()->atom(c->argi(3)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->f() = c->arg3d(0);
	rv.reset();
	return true;
}

// Set current atom x force
bool Commands::function_SetFX(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->f().set(0,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom y force
bool Commands::function_SetFY(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->f().set(1,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom z force
bool Commands::function_SetFZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->f().set(2,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom x coordinate
bool Commands::function_SetRX(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->r().set(0,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom y coordinate
bool Commands::function_SetRY(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->r().set(1,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom z coordinate
bool Commands::function_SetRZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->r().set(2,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom velocities
bool Commands::function_SetVelocities(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(3)) obj.i = c->argType(3) == VTypes::AtomData ? (Atom*) c->argp(3,VTypes::AtomData) : obj.rs()->atom(c->argi(3)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->v() = c->arg3d(0);
	rv.reset();
	return true;
}

// Set current atom x velocity
bool Commands::function_SetVX(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->v().set(0,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom y velocity
bool Commands::function_SetVY(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->v().set(1,c->argd(0));
	rv.reset();
	return true;
}

// Set current atom z velocity
bool Commands::function_SetVZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (c->hasArg(1)) obj.i = c->argType(1) == VTypes::AtomData ? (Atom*) c->argp(1,VTypes::AtomData) : obj.rs()->atom(c->argi(1)-1);
	if (obj.notifyNull(Bundle::AtomPointer)) return false;
	obj.i->v().set(2,c->argd(0));
	rv.reset();
	return true;
}

// Show current atom selection
bool Commands::function_Show(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Show %i atoms", obj.rs()->nSelected());
	obj.rs()->selectionSetHidden(false);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}
