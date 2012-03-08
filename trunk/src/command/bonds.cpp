/*
	*** Bonding Commands
	*** src/command/bonds.cpp
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
#include "parser/tree.h"
#include "model/model.h"
#include "classes/prefs.h"

// Augment bonds in current model ('augment')
bool Command::function_Augment(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Augment Bonds");
	obj.rs()->augmentBonding();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Change bond tolerance ('bondtol [tol]')
bool Command::function_BondTolerance(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (c->hasArg(0)) prefs.setBondTolerance(c->argd(0));
	rv.set(prefs.bondTolerance());
	return TRUE;
}

// Clear bonds in current model ('clearbonds')
bool Command::function_ClearBonds(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Clear Bonding");
	obj.rs()->clearBonding();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Clear bonds in current model ('clearbonds')
bool Command::function_ClearSelectedBonds(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Clear Bonds in Selection");
	obj.rs()->selectionClearBonding();
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Add bond between atoms ('newbond <atom1> <atom2> [bondtype]')
bool Command::function_NewBond(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Third (optional) argument gives bond type
	Bond::BondType bt = Bond::Single;
	if (c->hasArg(2))
	{
		// Attempt to convert the argument into a BondType.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = Bond::bondType(c->argc(2));
		else bt = (Bond::BondType) n;
	}
	// Add the bond
	Atom *i = c->argType(0) == VTypes::IntegerData ? obj.rs()->findAtom(c->argi(0)-1) : (Atom*) c->argp(0, VTypes::AtomData);
	Atom *j = c->argType(1) == VTypes::IntegerData ? obj.rs()->findAtom(c->argi(1)-1) : (Atom*) c->argp(1, VTypes::AtomData);
	if ((i != NULL) && (j != NULL))
	{
		// Add the bond
		obj.rs()->beginUndoState("Bond Atoms");
		obj.rs()->bondAtoms(i, j, bt);
		obj.rs()->endUndoState();
	}
	else msg.print("Can't bond atoms - one or both atoms not found.\n");
	rv.reset();
	return TRUE;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
bool Command::function_NewBondId(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// Third (optional) argument gives bond type
	Bond::BondType bt = Bond::Single;
	if (c->hasArg(2))
	{
		// Attempt to convert the argument into a BondType.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = Bond::bondType(c->argc(2));
		else bt = (Bond::BondType) n;
	}
	// Find the atoms specified
	Atom *i = obj.rs()->findAtom(c->argi(0));
	Atom *j = obj.rs()->findAtom(c->argi(1));
	if ((i != NULL) && (j != NULL))
	{
		// Add the bond
		obj.rs()->beginUndoState("Bond Atoms");
		obj.rs()->bondAtoms(i, j, bt);
		obj.rs()->endUndoState();
	}
	else msg.print("Can't bond atoms - one or both atoms not found.\n");
	rv.reset();
	return TRUE;
}

// Calculate bonds in current model ('rebond')
bool Command::function_ReBond(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If we're reading from a file (via a filter) check for prefs override
	if (c->parent()->parser() == NULL)
	{
		obj.rs()->beginUndoState("Calculate Bonding");
		obj.rs()->clearBonding();
		obj.rs()->calculateBonding( c->hasArg(0) ? c->argb(0) : prefs.augmentAfterRebond() );
		obj.rs()->endUndoState();
	}
	else if (prefs.bondOnLoad() != Choice::No)
	{
		obj.rs()->clearBonding();
		obj.rs()->calculateBonding( c->hasArg(0) ? c->argb(0) : prefs.augmentAfterRebond() );
	}
	rv.reset();
	return TRUE;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
bool Command::function_ReBondPatterns(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Calculate Bonding (Patterns)");
	obj.rs()->patternCalculateBonding( c->hasArg(0) ? c->argb(0) : prefs.augmentAfterRebond() );
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

// Calculate bonds restricted to current selection ('rebondselection')
bool Command::function_ReBondSelection(CommandNode *c, Bundle &obj, ReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs()->beginUndoState("Calculate Bonding (Selection)");
	obj.rs()->selectionCalculateBonding( c->hasArg(0) ? c->argb(0) : prefs.augmentAfterRebond() );
	obj.rs()->endUndoState();
	rv.reset();
	return TRUE;
}

