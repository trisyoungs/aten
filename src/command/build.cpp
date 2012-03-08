/*
	*** Build Commands
	*** src/command/build.cpp
	Copyright T. Youngs 2007-2012

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
#include "main/aten.h"
#include "model/model.h"
#include "classes/grid.h"
#include "ff/forcefield.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"

// Add hydrogens to model ('addhydrogen')
bool Command::function_AddHydrogen(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Optional argument specifies an atom, either by id or pointer
	if (c->hasArg(0))
	{
		obj.rs()->beginUndoState("Add Hydrogens to Atom");
		Atom *i;
		static ReturnValue v1;
		if (!c->arg(0, v1)) return FALSE;
		if (v1.type() == VTypes::IntegerData) i = obj.rs()->atom(v1.asInteger()-1);
		else if (v1.type() == VTypes::AtomData) i = (Atom*) v1.asPointer(VTypes::AtomData);
		else
		{
			msg.print("Optional argument to 'addhydrogen' must be a variable of Integer or Atom type.\n");
			return FALSE;
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
	return TRUE;
}

// Convert coordinates/data of object(s) to Bohr
bool Command::function_Bohr(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	Atom *i;
	Model *m;
	Grid *g;
	bool result = TRUE;
	for (int n=0; n<c->nArgs(); ++n)
	{
		// Check for valid pointer
		if (c->argp(n, c->argType(n)) == NULL) msg.print("Argument %i passed to 'bohr' is a NULL pointer.\n", n+1);
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
				msg.print("No valid conversion for ordinary types.\n");
				result = FALSE;
				break;
			case (VTypes::ElementData):
				msg.print("No valid Bohr conversion for type '%s'.\n", VTypes::dataType(c->argType(n)));
				result = FALSE;
				break;
			default:
				msg.print("Bohr conversion for type '%s' not implemented.\n", VTypes::dataType(c->argType(n)));
				result = FALSE;
				break;
		}
	}
	return result;
}

// Draw atom with bond to last atom ('chain <el> [bt]' or 'chain <el> <x> <y> <z> [bt]')
bool Command::function_Chain(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// In the first form, draw element at current pen position. In the second, add at the specified coordinates
	obj.rs()->beginUndoState("Draw Chain");
	Atom *i;
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
				if (!c->arg(4, v1)) return FALSE;
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
				if (!c->arg(1, v1)) return FALSE;
				if (v1.type() == VTypes::StringData) bt = Bond::bondType(v1.asString());
				else bt = Bond::bondType(v1.asInteger());
			}
			else bt = Bond::Single;
			obj.rs()->bondAtoms(obj.i, i, bt);
		}
	}
	obj.rs()->endUndoState();
	aten.current.i = i;
	rv.set(VTypes::AtomData, i);
	return TRUE;
}

// Terminate chain ('endchain')
bool Command::function_EndChain(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	// TODO end chain with atom id (optional argument)
	obj.i = NULL;
	rv.reset();
	return TRUE;
}

// Grow atom on target atom
bool Command::function_GrowAtom(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);

	Atom *i;
	ReturnValue v1;
	if (!c->arg(1, v1)) return FALSE;
	if (v1.type() == VTypes::IntegerData) i = obj.rs()->atom(v1.asInteger()-1);
	else if (v1.type() == VTypes::AtomData) i = (Atom*) v1.asPointer(VTypes::AtomData);
	else
	{
		msg.print("Second argument to 'growAtom' must be a variable of int or Atom type.\n");
		return FALSE;
	}
	
	// Check geometry specification
	Atom::AtomGeometry ag = Atom::TetrahedralGeometry;
	if (c->hasArg(2)) ag = Atom::atomGeometry(c->argc(2), TRUE);
	if (ag == Atom::nAtomGeometries) return FALSE;
	double distance;
	if (c->hasArg(3)) distance = c->argd(3);
	else distance = (elements().atomicRadius(i) + elements().atomicRadius(el));

	obj.rs()->beginUndoState("Grow Atom");
	aten.current.i = obj.rs()->growAtom(i, el, distance, ag, TRUE);
	obj.rs()->endUndoState();
	return TRUE;
}

// Draw unbound atom with ID specified ('insertatom <el> <id> [x y z]')
bool Command::function_InsertAtom(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);
	obj.rs()->beginUndoState("Draw Atom");
	// Get and check requested ID
	int id = c->argi(1);
	if ((id < 1) && (id > (obj.rs()->nAtoms()+1)))
	{
		msg.print("Requested ID for new atom (%i) is out of range (target model has %i atoms).\n", id, obj.rs()->nAtoms());
		return FALSE;
	}
	Vec3<double> pos = (c->hasArg(4) ? c->arg3d(2) : obj.rs()->penPosition());
	aten.current.i = obj.rs()->addAtomWithId(el, pos, id-1);
	// Add the name to the model's namesForcefield, if requested and it exists
	if (prefs.keepNames())
	{
		ForcefieldAtom *ffa = obj.rs()->addAtomName(el, c->argc(0));
		aten.current.i->setType(ffa);
		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
	}
	if (prefs.keepTypes())
	{
		ForcefieldAtom *ffa;
		for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
		{
			ffa = ff->findType(c->argc(0));
			if (ffa != NULL) break;
		}
		aten.current.i->setType(ffa);
		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
	}
	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten.current.i);
	return TRUE;
}

// Set pen coordinates ('locate <dx dy dz>')
bool Command::function_Locate(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	obj.rs()->setPenPosition(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Move pen along pen axes ('move <dx dy dz>')
bool Command::function_Move(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	obj.rs()->movePenPosition(c->arg3d(0));
	rv.reset();
	return TRUE;
}

// Move current selection to end of list ('toend')
bool Command::function_MoveToEnd(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Move selection to end");
	obj.rs()->moveSelectionToEnd();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Move current selection to start of list ('tostart')
bool Command::function_MoveToStart(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Move selection to start");
	obj.rs()->moveSelectionToStart();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Draw unbound atom ('newatom <el> [x y z] [vx vy vz] [fx fy fz]')
bool Command::function_NewAtom(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);
	obj.rs()->beginUndoState("Draw Atom");
	if (c->hasArg(9)) aten.current.i = obj.rs()->addAtom(el, c->arg3d(1), c->arg3d(4), c->arg3d(7));
	else if (c->hasArg(6)) aten.current.i = obj.rs()->addAtom(el, c->arg3d(1), c->arg3d(4));
	else if (c->hasArg(3)) aten.current.i = obj.rs()->addAtom(el, c->arg3d(1));
	else aten.current.i = obj.rs()->addAtomAtPen(el);
	// Add the name to the model's namesForcefield, if requested and it exists
 	if (prefs.keepNames())
 	{
		ForcefieldAtom *ffa = obj.rs()->addAtomName(el, c->argc(0));
 		aten.current.i->setType(ffa);
 		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
 	}
 	if (prefs.keepTypes())
	{
		ForcefieldAtom *ffa;
		for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
		{
			ffa = ff->findType(c->argc(0));
			if (ffa != NULL) break;
		}
		aten.current.i->setType(ffa);
		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
	}
	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten.current.i);
	return TRUE;
}

// Draw unbound atom ('newatom <el> [fracx fracy fracz]')
bool Command::function_NewAtomFrac(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	short int el = c->argz(0);
	// Check for presence of unit cell
	Vec3<double> r = c->arg3d(1);
	if (r.x < 0.0) r.x += 1.0;
	else if (r.x > 1.0) r.x -= 1.0;
	if (r.y < 0.0) r.y += 1.0;
	else if (r.y > 1.0) r.y -= 1.0;
	if (r.z < 0.0) r.z += 1.0;
	else if (r.z > 1.0) r.z -= 1.0;	
	if (obj.rs()->cell()->type() == UnitCell::NoCell) msg.print("Warning: No unit cell present - atom added with supplied coordinates.\n");
	else r = obj.rs()->cell()->fracToReal(r);
	obj.rs()->beginUndoState("Draw atom (fractional)");
	if (c->hasArg(9)) aten.current.i = obj.rs()->addAtom(el, r, c->arg3d(4), c->arg3d(7));
	else if (c->hasArg(6)) aten.current.i = obj.rs()->addAtom(el, r, c->arg3d(4));
	else aten.current.i = obj.rs()->addAtom(el, r);
	// Add the name to the model's namesForcefield, if requested and it exists
 	if (prefs.keepNames())
 	{
		ForcefieldAtom *ffa = obj.rs()->addAtomName(el, c->argc(0));
 		aten.current.i->setType(ffa);
 		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
 	}
 	if (prefs.keepTypes())
	{
		ForcefieldAtom *ffa;
		for (Forcefield *ff = aten.forcefields(); ff != NULL; ff = ff->next)
		{
			ffa = ff->findType(c->argc(0));
			if (ffa != NULL) break;
		}
		aten.current.i->setType(ffa);
		if (ffa != NULL) aten.current.i->setTypeFixed(TRUE);
	}
	obj.rs()->endUndoState();
	rv.set(VTypes::AtomData, aten.current.i);
	return TRUE;
}

// Reorder current atom selection ('reorder')
bool Command::function_ReOrder(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Reorder selected atoms");
	obj.rs()->reorderSelectedAtoms();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Reset pen orientation
bool Command::function_ResetPen(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->resetPenOrientation();
	rv.reset();
	return TRUE;
}

// Rotate pen orientation about x axis ('rotx <theta>')
bool Command::function_RotX(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->rotatePenAxis(0, c->argd(0));
	rv.reset();
	return TRUE;
}

// Rotate pen orientation about y axis ('roty <theta>')
bool Command::function_RotY(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->rotatePenAxis(1, c->argd(0));
	rv.reset();
	return TRUE;
}

// Rotate pen orientation about z axis ('rotz <theta>')
bool Command::function_RotZ(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->rotatePenAxis(2, c->argd(0));
	rv.reset();
	return TRUE;
}

// Add hydrogens to current selection ('selectionaddhydrogen')
bool Command::function_SelectionAddHydrogen(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Add Hydrogens to selection");
	for (Refitem<Atom,int> *ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.rs()->hydrogenSatisfy(ri->item);
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Grow atom on to all atoms in selection
bool Command::function_SelectionGrowAtom(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	
	// Determine element (based on type of variable provided)
	short int el = c->argz(0);

	// Check geometry specification
	Atom::AtomGeometry ag = Atom::TetrahedralGeometry;
	if (c->hasArg(1)) ag = Atom::atomGeometry(c->argc(1), TRUE);
	if (ag == Atom::nAtomGeometries) return FALSE;
	double distance;
	if (c->hasArg(2)) distance = c->argd(2);

	obj.rs()->beginUndoState("Selection Grow Atom");
	for (Refitem<Atom,int> *ri = obj.rs()->selection(); ri != NULL; ri = ri->next)
	{
		// Set distance if no general distance was provided
		if (!c->hasArg(2)) distance = (elements().atomicRadius(ri->item) + elements().atomicRadius(el));
		obj.rs()->growAtom(ri->item, el, distance, ag, TRUE);
	}
	obj.rs()->endUndoState();
	return TRUE;
}

// Shift the current selection down ('shiftdown [n]')
bool Command::function_ShiftDown(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Shift selection down");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs()->shiftSelectionDown();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Shift the current selection up ('shiftup [n]')
bool Command::function_ShiftUp(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Shift selection up");
	for (int n=0; n<(c->hasArg(0) ? c->argi(0) : 1); n++) obj.rs()->shiftSelectionUp();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Transmute the current selection ('transmute <el>')
bool Command::function_Transmute(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	short int el = c->argz(0);
	obj.rs()->beginUndoState("Transmute selection");
	for (Refitem<Atom,int> *ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.rs()->transmuteAtom(ri->item,el);
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}
