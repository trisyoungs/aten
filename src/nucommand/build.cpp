/*
	*** Build command functions
	*** src/command/build.cpp
	Copyright T. Youngs 2007-2009

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

#include "parser/commands.h"
#include "parser/commandnode.h"
#include "main/aten.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"

// Add hydrogens to model ('addhydrogen')
bool NuCommand::function_AddHydrogen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Optional argument specifies an atom, either by id or pointer
	if (c->hasArg(0))
	{
		obj.rs->beginUndoState("Add Hydrogens to Atom");
		Atom *i;
		static NuReturnValue v1;
		if (!c->arg(0, v1)) return FALSE;
		if (v1.type() == NuVTypes::IntegerData) i = obj.rs->atom(v1.asInteger()-1);
		else if (v1.type() == NuVTypes::AtomData) i = (Atom*) v1.asPointer(NuVTypes::AtomData);
		else
		{
			msg.print("Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.\n");
			return FALSE;
		}
		obj.rs->hydrogenSatisfy(i);
	}
	else
	{
		obj.rs->beginUndoState("Add Hydrogens to Model");
		obj.rs->hydrogenSatisfy();
	}
	obj.rs->endUndoState();
	return Command::Success;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
bool NuCommand::function_Bohr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
bool NuCommand::function_Chain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// In the first form, draw element at current pen position. In the second, add at the specified coordinates
	obj.rs->beginUndoState("Draw Chain");
	Atom *i;
	NuReturnValue v1;
	if (c->hasArg(3))
	{
		Vec3<double> pos = c->arg3d(1);
		i = obj.rs->addAtom(elements().find(c->argc(0)), pos);
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(4))
			{
				if (!c->arg(4, v1)) return FALSE;
				if (v1.type() == NuVTypes::CharacterData) bt = Bond::bondType(rv.asCharacter());
				else bt = Bond::bondType(rv.asInteger());
			}
			else bt = Bond::Single;
			obj.rs->bondAtoms(obj.i, i, bt);
		}
	}
	else
	{
		i = obj.rs->addAtomAtPen(elements().find(c->argc(0)));
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(1))
			{
				if (!c->arg(1, v1)) return FALSE;
				if (v1.type() == NuVTypes::CharacterData) bt = Bond::bondType(rv.asCharacter());
				else bt = Bond::bondType(rv.asInteger());
			}
			else bt = Bond::Single;
			obj.rs->bondAtoms(obj.i, i, bt);
		}
	}
	obj.rs->endUndoState();
	aten.current.i = i;
	return Command::Success;
}

// Terminate chain ('endchain')
bool NuCommand::function_EndChain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	return Command::Success;
}

// Draw unbound atom with ID specified ('insertatom <el> <id> [x y z]')
bool NuCommand::function_InsertAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Determine element (based on type of variable provided)
	Forcefield *f;
	Atom *i;
	ForcefieldAtom *ffa;
	Namemap<int> *nm;
	int el;
	NuReturnValue v1;
	if (!c->arg(0, v1)) return FALSE;
	switch (v1.type())
	{
		case (NuVTypes::IntegerData):
			el = v1.asInteger();
			break;
		case (NuVTypes::RealData):
			el = (int) floor(v1.asReal() + 0.15);
			break;
		case (NuVTypes::CharacterData):
			// Attempt conversion of the string first from the users type list
			for (nm = aten.typeMap.first(); nm != NULL; nm = nm->next)
				if (strcmp(nm->name(),v1.asCharacter()) == 0) break;
			if (nm == NULL) el = elements().find(v1.asCharacter());
			else el = nm->data();
			break;
		case (NuVTypes::AtomData):
			i = (Atom*) v1.asPointer(NuVTypes::AtomData);
			i == NULL ? el = 0 : i->element();
			break;
		default:
			msg.print("Type '%s' is not a valid one to pass to 'newatom'.\n", NuVTypes::dataType(v1.type()));
			el = 0;
			break;
	}
	obj.rs->beginUndoState("Draw Atom");
	// Get and check requested ID
	int id = c->argi(1);
	if ((id < 1) && (id > (obj.rs->nAtoms()+1)))
	{
		msg.print("Requested ID for new atom (%i) is out of range (target model has %i atoms).\n", id, obj.rs->nAtoms());
		return Command::Fail;
	}
	if (c->hasArg(4)) aten.current.i = obj.rs->addAtom(el, c->arg3d(1), id-1);
	else aten.current.i = obj.rs->addAtomAtPen(el, id-1);
	// Add the name to the model's namesForcefield, if requested and it exists
 	if (prefs.keepNames() && obj.rs->namesForcefield())
 	{
 		// Search for this typename in the ff
 		f = obj.rs->namesForcefield();
 		ffa = f->findType(c->argc(0));
 		if (ffa == NULL) 
 		{
 			ffa = f->addType();
 			ffa->setName(c->argc(0));
			ffa->atomtype()->setCharacterElement(el);
 		}
 		aten.current.i->setType(ffa);
 		aten.current.i->setTypeFixed(TRUE);
 	}
	obj.rs->endUndoState();
	return Command::Success;
}

// Set pen coordinates ('locate <dx dy dz>')
bool NuCommand::function_Locate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	obj.rs->setPenPosition(c->arg3d(0));
	return Command::Success;
}

// Move pen along pen axes ('move <dx dy dz>')
bool NuCommand::function_Move(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	obj.rs->movePenPosition(c->arg3d(0));
	return Command::Success;
}

// Move current selection to end of list ('toend')
bool NuCommand::function_MoveToEnd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Move selection to end");
	obj.rs->moveSelectionToEnd();
	obj.rs->endUndoState();
	return Command::Success;
}

// Move current selection to start of list ('tostart')
bool NuCommand::function_MoveToStart(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Move selection to start");
	obj.rs->moveSelectionToStart();
	obj.rs->endUndoState();
	return Command::Success;
}

// Draw unbound atom ('newatom <el> [x y z]')
bool NuCommand::function_NewAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Determine element (based on type of variable provided)
	Forcefield *f;
	Atom *i;
	ForcefieldAtom *ffa;
	Namemap<int> *nm;
	int el;
	NuReturnValue v1;
	if (!c->arg(0, v1)) return FALSE;
	switch (v1.type())
	{
		case (NuVTypes::IntegerData):
			el = v1.asInteger();
			break;
		case (NuVTypes::RealData):
			el = (int) floor(v1.asReal() + 0.15);
			break;
		case (NuVTypes::CharacterData):
			// Attempt conversion of the string first from the users type list
			for (nm = aten.typeMap.first(); nm != NULL; nm = nm->next)
				if (strcmp(nm->name(),v1.asCharacter()) == 0) break;
			if (nm == NULL) el = elements().find(v1.asCharacter());
			else el = nm->data();
			break;
		case (NuVTypes::AtomData):
			i = (Atom*) v1.asPointer(NuVTypes::AtomData);
			i == NULL ? el = 0 : i->element();
			break;
		default:
			msg.print("Type '%s' is not a valid one to pass to 'newatom'.\n", NuVTypes::dataType(v1.type()));
			el = 0;
			break;
	}
	obj.rs->beginUndoState("Draw Atom");
	if (c->hasArg(3)) aten.current.i = obj.rs->addAtom(el, c->arg3d(1));
	else aten.current.i = obj.rs->addAtomAtPen(el);
	// Add the name to the model's namesForcefield, if requested and it exists
 	if (prefs.keepNames() && obj.rs->namesForcefield())
 	{
 		// Search for this typename in the ff
 		f = obj.rs->namesForcefield();
 		ffa = f->findType(c->argc(0));
 		if (ffa == NULL) 
 		{
 			ffa = f->addType();
 			ffa->setName(c->argc(0));
			ffa->atomtype()->setCharacterElement(el);
 		}
 		aten.current.i->setType(ffa);
 		aten.current.i->setTypeFixed(TRUE);
 	}
	obj.rs->endUndoState();
	return Command::Success;
}

// Draw unbound atom ('newatom <el> [fracx fracy fracz]')
bool NuCommand::function_NewAtomFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// Determine element (based on type of variable provided)
	int el;
	Atom *i;
	NuReturnValue v1;
	if (!c->arg(0, v1)) return FALSE;
	switch (v1.type())
	{
		case (NuVTypes::IntegerData):
			el = v1.asInteger();
			break;
		case (NuVTypes::RealData):
			el = (int) floor(v1.asReal() + 0.15);
			break;
		case (NuVTypes::CharacterData):
			el = elements().find(v1.asCharacter());
			break;
		case (NuVTypes::AtomData):
			i = (Atom*) v1.asPointer(NuVTypes::AtomData);
			i == NULL ? el = 0 : i->element();
			break;
		default:
			msg.print("Type '%s' is not a valid one to pass to Addatom.\n", NuVTypes::dataType(v1.type()));
			el = 0;
			break;
	}
	// Check for presence of unit cell
	Vec3<double> r = c->arg3d(1);
	if (r.x < 0.0) r.x += 1.0;
	else if (r.x > 1.0) r.x -= 1.0;
	if (r.y < 0.0) r.y += 1.0;
	else if (r.y > 1.0) r.y -= 1.0;
	if (r.z < 0.0) r.z += 1.0;
	else if (r.z > 1.0) r.z -= 1.0;	
	if (obj.rs->cell()->type() == Cell::NoCell) msg.print("Warning: No unit cell present - atom added with supplied coordinates.\n");
	else r = obj.rs->cell()->fracToReal(r);
	obj.rs->beginUndoState("Draw atom (fractional)");
	aten.current.i = obj.rs->addAtom(el, r);
	obj.rs->endUndoState();
	return Command::Success;
}

// Reorder current atom selection ('reorder')
bool NuCommand::function_ReOrder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Reorder selected atoms");
	obj.rs->reorderSelectedAtoms();
	obj.rs->endUndoState();
	return Command::Success;
}

// Reset pen orientation
bool NuCommand::function_ResetPen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->resetPenOrientation();
	return Command::Success;
}

// Rotate pen orientation about x axis ('rotx <theta>')
bool NuCommand::function_RotX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->rotatePenAxis(0, c->argd(0));
	return Command::Success;
}

// Rotate pen orientation about y axis ('roty <theta>')
bool NuCommand::function_RotY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->rotatePenAxis(1, c->argd(0));
	return Command::Success;
}

// Rotate pen orientation about z axis ('rotz <theta>')
bool NuCommand::function_RotZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->rotatePenAxis(2, c->argd(0));
	return Command::Success;
}

// Shift the current selection down ('shiftdown [n]')
bool NuCommand::function_ShiftDown(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Shift selection down");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionDown();
	obj.rs->endUndoState();
	return Command::Success;
}

// Shift the current selection up ('shiftup [n]')
bool NuCommand::function_ShiftUp(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Shift selection up");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionUp();
	obj.rs->endUndoState();
	return Command::Success;
}

// Transmute the current selection ('transmute <el>')
bool NuCommand::function_Transmute(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	int el = elements().findAlpha(c->argc(0));
	obj.rs->beginUndoState("Transmute selection");
	for (Atom *i = obj.rs->firstSelected(); i != NULL; i = i->nextSelected()) obj.rs->transmuteAtom(i,el);
	obj.rs->endUndoState();
	return Command::Success;
}
