/*
	*** Build Commands
	*** src/command/build.cpp
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
#include "ff/forcefield.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

// Add hydrogens to model ('addhydrogen')
bool Commands::function_AddHydrogen(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	// Optional argument specifies an atom, either by id or pointer
	if (c->hasArg(0))
	{
		obj.rs()->beginUndoState("Add Hydrogens to Atom");
		Atom* i;
		static ReturnValue v1;
		if (!c->arg(0, v1)) return false;
		if (v1.type() == VTypes::IntegerData) i = obj.rs()->atom(v1.asInteger()-1);
		else if (v1.type() == VTypes::AtomData) i = (Atom*) v1.asPointer(VTypes::AtomData);
		else
		{
			Messenger::print("Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.");
			return false;
		}
		obj.rs()->hydrogenSatisfy(i);
	}
	else
	{
		obj.rs()->beginUndoState("Add Hydrogens to Model");
		obj.rs()->hydrogenSatisfy();
	}
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Convert coordinates/data of object(s) to Bohr
bool Commands::function_Bohr(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	Atom* i;
	Model* m;
	Grid* g;
	bool result = true;
	for (int n=0; n<c->nArgs(); ++n)
	{
		// Check for valid pointer
		if (c->argp(n, c->argType(n)) == NULL) Messenger::print("Argument %i passed to 'bohr' is a NULL pointer.", n+1);
		else switch (c->argType(n))
		{
			case (VTypes::AtomData):
				i = (Atom*) c->argp(n, VTypes::AtomData);
				m = i->parent();
				m->beginUndoState("Convert coordinates of atom to Angstroms");
				m->positionAtom(i, i->r()/ANGBOHR);
				m->endUndoState();
				break;
			case (VTypes::ModelData):
				m = (Model*) c->argp(n, VTypes::ModelData);
				m->bohrToAngstrom();
				break;
			case (VTypes::GridData):
				g = (Grid*) c->argp(n, VTypes::GridData);
				g->bohrToAngstrom();
				break;			case (VTypes::IntegerData):
			case (VTypes::DoubleData):
			case (VTypes::StringData):
				Messenger::print("No valid conversion for ordinary types.");
				result = false;
				break;
			case (VTypes::ElementData):
				Messenger::print("No valid Bohr conversion for type '%s'.", VTypes::dataType(c->argType(n)));
				result = false;
				break;
			default:
				Messenger::print("Bohr conversion for type '%s' not implemented.", VTypes::dataType(c->argType(n)));
				result = false;
				break;
		}
	}
	return result;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
bool Commands::function_Chain(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// In the first form, draw element at current pen position. In the second, add at the specified coordinates
	obj.rs()->beginUndoState("Draw Chain");
	Atom* i;
	ReturnValue v1;
	short int el = c->argz(0);
	if (c->hasArg(3))
	{
		Vec3<double> pos = c->arg3d(1);
		i = obj.rs()->addAtom(el, pos);
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(4))
			{
				if (!c->arg(4, v1)) return false;
				if (v1.type() == VTypes::StringData) bt = Bond::bondType(v1.asString());
				else bt = Bond::bondType(v1.asDouble());
			}
			else bt = Bond::Single;
			obj.rs()->bondAtoms(obj.i, i, bt);
		}
	}
	else
	{
		i = obj.rs()->addAtomAtPen(el);
		if (obj.i != NULL)
		{
			Bond::BondType bt;
			if (c->hasArg(1))
			{
				if (!c->arg(1, v1)) return false;
				if (v1.type() == VTypes::StringData) bt = Bond::bondType(v1.asString());
				else bt = Bond::bondType(v1.asInteger());
			}
			else bt = Bond::Single;
			obj.rs()->bondAtoms(obj.i, i, bt);
		}
	}
	obj.rs()->endUndoState();
	aten_.current().i = i;
	rv.set(VTypes::AtomData, i);
	return true;
}

// Terminate chain ('endchain')
bool Commands::function_EndChain(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	rv.reset();
	return true;
}

// Grow atom on target atom
bool Commands::function_GrowAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);

	Atom* i;
	ReturnValue v1;
	if (!c->arg(1, v1)) return false;
	if (v1.type() == VTypes::IntegerData) i = obj.rs()->atom(v1.asInteger()-1);
	else if (v1.type() == VTypes::AtomData) i = (Atom*) v1.asPointer(VTypes::AtomData);
	else
	{
		Messenger::print("Second argument to 'growAtom' must be a variable of int or Atom type.");
		return false;
	}
	
	// Check geometry specification
	Atom::AtomGeometry ag = Atom::TetrahedralGeometry;
	if (c->hasArg(2)) ag = Atom::atomGeometry(c->argc(2), true);
	if (ag == Atom::nAtomGeometries) return false;

	// Get distance (if specified)
	double distance = -1.0;
	if (c->hasArg(3)) distance = c->argd(3);

	// Get bond boolean (if specified)
	bool bond = true;
	if (c->hasArg(4)) bond = c->argb(4);

	obj.rs()->beginUndoState("Grow Atom");
	aten_.current().i = obj.rs()->growAtom(i, el, distance, ag, bond);
	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten_.current().i);
	return true;
}

// Draw unbound atom with ID specified ('insertatom <el> <id> [x y z]')
bool Commands::function_InsertAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Begin undostate
	obj.rs()->beginUndoState("Draw Atom");

	// Determine element (based on type of variable provided)
	short int el = c->argz(0);

	// Get and check requested ID
	int id = c->argi(1);
	if ((id < 1) || (id > (obj.rs()->nAtoms()+1)))
	{
		Messenger::print("Requested ID for new atom (%i) is out of range (target model has %i atoms).", id, obj.rs()->nAtoms());
		return false;
	}
	Vec3<double> pos = (c->hasArg(4) ? c->arg3d(2) : obj.rs()->penPosition());
	aten_.current().i = obj.rs()->addAtomWithId(el, pos, id-1);

	// End undostate
	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten_.current().i);
	return true;
}

// Set pen coordinates ('locate <dx dy dz>')
bool Commands::function_Locate(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.rs()->setPenPosition(c->arg3d(0));
	rv.reset();
	return true;
}

// Move pen along pen axes ('move <dx dy dz>')
bool Commands::function_Move(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	obj.rs()->movePenPosition(c->arg3d(0));
	rv.reset();
	return true;
}

// Move current selection to end of list ('toend')
bool Commands::function_MoveToEnd(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Move selection to end");
	obj.rs()->moveSelectionToEnd();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Move current selection to start of list ('tostart')
bool Commands::function_MoveToStart(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Move selection to start");
	obj.rs()->moveSelectionToStart();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Draw unbound atom ('newatom <el> [x y z] [vx vy vz] [fx fy fz]')
bool Commands::function_NewAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	// Determine element (based on type of variable provided)
	short int el = c->argz(0);
	obj.rs()->beginUndoState("Draw Atom");
	if (c->hasArg(9)) aten_.current().i = obj.rs()->addAtom(el, c->arg3d(1), c->arg3d(4), c->arg3d(7));
	else if (c->hasArg(6)) aten_.current().i = obj.rs()->addAtom(el, c->arg3d(1), c->arg3d(4));
	else if (c->hasArg(3)) aten_.current().i = obj.rs()->addAtom(el, c->arg3d(1));
	else aten_.current().i = obj.rs()->addAtomAtPen(el);

	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten_.current().i);

	return true;
}

// Draw unbound atom ('newatom <el> [fracx fracy fracz]')
bool Commands::function_NewAtomFrac(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;

	short int el = c->argz(0);
	// Check for presence of unit cell
	Vec3<double> r = c->arg3d(1);
	if (r.x < 0.0) r.x += 1.0;
	else if (r.x > 1.0) r.x -= 1.0;
	if (r.y < 0.0) r.y += 1.0;
	else if (r.y > 1.0) r.y -= 1.0;
	if (r.z < 0.0) r.z += 1.0;
	else if (r.z > 1.0) r.z -= 1.0;	
	if (obj.rs()->cell().type() == UnitCell::NoCell) Messenger::print("Warning: No unit cell present - atom added with supplied coordinates.");
	else r = obj.rs()->cell().fracToReal(r);
	obj.rs()->beginUndoState("Draw atom (fractional)");
	if (c->hasArg(9)) aten_.current().i = obj.rs()->addAtom(el, r, c->arg3d(4), c->arg3d(7));
	else if (c->hasArg(6)) aten_.current().i = obj.rs()->addAtom(el, r, c->arg3d(4));
	else aten_.current().i = obj.rs()->addAtom(el, r);

	obj.rs()->endUndoState();

	rv.set(VTypes::AtomData, aten_.current().i);
	return true;
}

// Reorder current atom selection ('reorder')
bool Commands::function_ReOrder(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Reorder selected atoms");
	obj.rs()->reorderSelectedAtoms();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Reset pen orientation
bool Commands::function_ResetPen(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->resetPenOrientation();
	rv.reset();
	return true;
}

// Rotate pen orientation about x axis ('rotx <theta>')
bool Commands::function_RotX(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->rotatePenAxis(0, c->argd(0));
	rv.reset();
	return true;
}

// Rotate pen orientation about y axis ('roty <theta>')
bool Commands::function_RotY(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->rotatePenAxis(1, c->argd(0));
	rv.reset();
	return true;
}

// Rotate pen orientation about z axis ('rotz <theta>')
bool Commands::function_RotZ(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->rotatePenAxis(2, c->argd(0));
	rv.reset();
	return true;
}

// Add hydrogens to current selection ('selectionaddhydrogen')
bool Commands::function_SelectionAddHydrogen(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Add Hydrogens to selection");
	for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.rs()->hydrogenSatisfy(ri->item);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Grow atom on to all atoms in selection
bool Commands::function_SelectionGrowAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);

	// Check geometry specification
	Atom::AtomGeometry ag = Atom::TetrahedralGeometry;
	if (c->hasArg(1)) ag = Atom::atomGeometry(c->argc(1), true);
	if (ag == Atom::nAtomGeometries) return false;

	// Get distance (if specified)
	double distance = -1.0;
	if (c->hasArg(2)) distance = c->argd(2);

	// Get bond boolean (if specified)
	bool bond = true;
	if (c->hasArg(3)) bond = c->argb(3);

	obj.rs()->beginUndoState("Selection Grow Atom");
	for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		// Set distance if no general distance was provided
		if (!c->hasArg(2)) distance = (ElementMap::atomicRadius(ri->item) + ElementMap::atomicRadius(el));
		obj.rs()->growAtom(ri->item, el, distance, ag, true);
	}
	obj.rs()->endUndoState();
	rv.reset();

	return true;
}

// Shift the current selection down ('shiftdown [n]')
bool Commands::function_ShiftDown(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Shift selection down");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs()->shiftSelectionDown();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Shift the current selection up ('shiftup [n]')
bool Commands::function_ShiftUp(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Shift selection up");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs()->shiftSelectionUp();
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}

// Transmute the current selection ('transmute <el>')
bool Commands::function_Transmute(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	short int el = c->argz(0);
	obj.rs()->beginUndoState("Transmute selection");
	for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.rs()->transmuteAtom(ri->item,el);
	obj.rs()->endUndoState();
	rv.reset();
	return true;
}
