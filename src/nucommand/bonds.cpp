/*
	*** Bonding functions
	*** src/parser/bonds.cpp
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

#include "nucommand/commands.h"
#include "parser/commandnode.h"
#include "parser/tree.h"
#include "model/model.h"
#include "classes/prefs.h"

// Augment bonds in current model ('augment')
bool NuCommand::function_Augment(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Augment Bonds");
	obj.rs->augmentBonding();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Change bond tolerance ('bondtol <d>')
bool NuCommand::function_BondTolerance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	prefs.setBondTolerance(c->argd(0));
	rv.reset();
	return TRUE;
}

// Clear bonds in current model ('clearbonds')
bool NuCommand::function_ClearBonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Clear Bonding");
	obj.rs->clearBonding();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Clear bonds in current model ('clearbonds')
bool NuCommand::function_ClearSelectedBonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Clear Bonds in Selection");
	obj.rs->selectionClearBonding();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Add bond between atoms ('newbond <atom1> <atom2> [bondtype]')
bool NuCommand::function_NewBond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	obj.rs->beginUndoState("Bond Atoms");
	obj.rs->bondAtoms(c->argi(0)-1, c->argi(1)-1, bt);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
bool NuCommand::function_NewBondId(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
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
	Atom *i = obj.rs->findAtom(c->argi(0));
	Atom *j = obj.rs->findAtom(c->argi(1));
	// Add the bond
	obj.rs->beginUndoState("Bond Atoms");
	obj.rs->bondAtoms(i, j, bt);
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Calculate bonds in current model ('rebond')
bool NuCommand::function_ReBond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	// If we're reading from a file (via a filter) check for prefs override
	if (!c->parent()->isFileSource())
	{
		obj.rs->beginUndoState("Calculate Bonding");
		obj.rs->clearBonding();
		obj.rs->calculateBonding();
		obj.rs->endUndoState();
	}
	else if (prefs.bondOnLoad() != Prefs::SwitchOff)
	{
		obj.rs->clearBonding();
		obj.rs->calculateBonding();
	}
	rv.reset();
	return TRUE;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
bool NuCommand::function_ReBondPatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Calculate Bonding (Patterns)");
	obj.rs->patternCalculateBonding();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}

// Calculate bonds restricted to current selection ('rebondselection')
bool NuCommand::function_ReBondSelection(NuCommandNode *c, Bundle &obj, NuReturnValue &rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return FALSE;
	obj.rs->beginUndoState("Calculate Bonding (Selection)");
	obj.rs->selectionCalculateBonding();
	obj.rs->endUndoState();
	rv.reset();
	return TRUE;
}
