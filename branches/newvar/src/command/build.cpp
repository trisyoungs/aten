/*
	*** Build command functions
	*** src/command/build.cpp
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
#include "main/aten.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"

// Add hydrogens to model ('addhydrogen')
int CommandData::function_CA_ADDHYDROGEN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// Optional argument specifies an atom, either by id or pointer
	if (c->hasArg(0))
	{
		obj.rs->beginUndoState("Add Hydrogens to Atom");
		Atom *i;
		if (c->argt(0) == VTypes::IntegerData) i = obj.rs->atom(c->argi(0)-1);
		else if (c->argt(0) == VTypes::AtomData) i = c->arga(0);
		else
		{
			msg.print("Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.\n");
			return CR_FAIL;
		}
		obj.rs->hydrogenSatisfy(i);
	}
	else
	{
		obj.rs->beginUndoState("Add Hydrogens to Model");
		obj.rs->hydrogenSatisfy();
	}
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
int CommandData::function_CA_BOHR(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
int CommandData::function_CA_CHAIN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// In the first form, draw element at current pen position. In the second, add at the specified coordinates
	obj.rs->beginUndoState("Draw Chain");
	Atom *i;
	if (c->hasArg(3))
	{
		Vec3<double> pos = c->arg3d(1);
		i = obj.rs->addAtom(elements.find(c->argc(0),ElementMap::AlphaZmap), pos);
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(4))
			{
				if (c->argt(4) == VTypes::CharacterData) bt = Bond::bondType(c->argc(4));
				else bt = Bond::bondType(c->argi(4));
			}
			else bt = Bond::Single;
			obj.rs->bondAtoms(obj.i, i, bt);
		}
	}
	else
	{
		i = obj.rs->addAtomAtPen(elements.find(c->argc(0),ElementMap::AlphaZmap));
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(1))
			{
				if (c->argt(1) == VTypes::CharacterData) bt = Bond::bondType(c->argc(1));
				else bt = Bond::bondType(c->argi(1));
			}
			else bt = Bond::Single;
			obj.rs->bondAtoms(obj.i, i, bt);
		}
	}
	obj.rs->endUndoState();
	aten.current.i = i;
	return CR_SUCCESS;
}

// Terminate chain ('endchain')
int CommandData::function_CA_ENDCHAIN(Command *&c, Bundle &obj)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	return CR_SUCCESS;
}

// Set pen coordinates ('locate <dx dy dz>')
int CommandData::function_CA_LOCATE(Command *&c, Bundle &obj)
{
	obj.rs->setPenPosition(c->arg3d(0));
	return CR_SUCCESS;
}

// Move pen along pen axes ('move <dx dy dz>')
int CommandData::function_CA_MOVE(Command *&c, Bundle &obj)
{
	obj.rs->movePenPosition(c->arg3d(0));
	return CR_SUCCESS;
}

// Draw unbound atom ('newatom <el> [x y z]')
int CommandData::function_CA_NEWATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// Determine element (based on type of variable provided)
	Forcefield *f;
	ForcefieldAtom *ffa;
	Namemap<int> *nm;
	int el;
	switch (c->argt(0))
	{
		case (VTypes::IntegerData):
			el = c->argi(0);
			break;
		case (VTypes::RealData):
			el = (int) floor(c->argd(0) + 0.15);
			break;
		case (VTypes::CharacterData):
			// Attempt conversion of the string first from the users type list
			for (nm = aten.typeMap.first(); nm != NULL; nm = nm->next)
				if (strcmp(nm->name(),c->argc(0)) == 0) break;
			if (nm == NULL) el = elements.find(c->argc(0));
			else el = nm->data();
			break;
		case (VTypes::AtomData):
			c->arga(0) == NULL ? el = 0 : c->arga(0)->element();
			break;
		default:
			msg.print("Type '%s' is not a valid one to pass to 'newatom'.\n", VTypes::dataType(c->argt(0)));
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
	return CR_SUCCESS;
}

// Draw unbound atom ('newatom <el> [fracx fracy fracz]')
int CommandData::function_CA_NEWATOMFRAC(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	// Determine element (based on type of variable provided)
	int el;
	switch (c->argt(0))
	{
		case (VTypes::IntegerData):
			el = c->argi(0);
			break;
		case (VTypes::RealData):
			el = (int) floor(c->argd(0) + 0.15);
			break;
		case (VTypes::CharacterData):
			el = elements.find(c->argc(0));
			break;
		case (VTypes::AtomData):
			c->arga(0) == NULL ? el = 0 : c->arga(0)->element();
			break;
		default:
			msg.print("Type '%s' is not a valid one to pass to CA_ADDATOM.\n", VTypes::dataType(c->argt(0)));
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
	return CR_SUCCESS;
}

// Reset pen orientation
int CommandData::function_CA_RESETPEN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->resetPenOrientation();
	return CR_SUCCESS;
}

// Rotate pen orientation about x axis ('rotx <theta>')
int CommandData::function_CA_ROTX(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->rotatePenAxis(0, c->argd(0));
	return CR_SUCCESS;
}

// Rotate pen orientation about y axis ('roty <theta>')
int CommandData::function_CA_ROTY(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->rotatePenAxis(1, c->argd(0));
	return CR_SUCCESS;
}

// Rotate pen orientation about z axis ('rotz <theta>')
int CommandData::function_CA_ROTZ(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->rotatePenAxis(2, c->argd(0));
	return CR_SUCCESS;
}

// Shift the current selection down ('shiftdown [n]')
int CommandData::function_CA_SHIFTDOWN(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Shift selection down");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionDown();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Shift the current selection up ('shiftup [n]')
int CommandData::function_CA_SHIFTUP(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Shift selection up");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs->shiftSelectionUp();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Move current selection to end of list ('toend')
int CommandData::function_CA_TOEND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Move selection to end");
	obj.rs->moveSelectionToEnd();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Move current selection to start of list ('tostart')
int CommandData::function_CA_TOSTART(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Move selection to start");
	obj.rs->moveSelectionToStart();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Transmute the current selection ('transmute <el>')
int CommandData::function_CA_TRANSMUTE(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	int el = elements.find(c->argc(0));
	obj.rs->beginUndoState("Transmute selection");
	for (Atom *i = obj.rs->firstSelected(); i != NULL; i = i->nextSelected()) obj.rs->transmuteAtom(i,el);
	obj.rs->endUndoState();
	return CR_SUCCESS;
}
