/*
	*** Bonding command functions
	*** src/command/bonds.cpp
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
#include "classes/prefs.h"

// Augment bonds in current model ('augment')
int Command::function_CA_AUGMENT(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Augment Bonds");
	obj.rs->augmentBonding();
	obj.rs->endUndoState();
	return Command::Success;
}

// Change bond tolerance ('bondtol <d>')
int Command::function_CA_BONDTOLERANCE(CommandNode *&c, Bundle &obj)
{
	prefs.setBondTolerance(c->argd(0));
	return Command::Success;
}

// Clear bonds in current model ('clearbonds')
int Command::function_CA_CLEARBONDS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Clear Bonding");
	obj.rs->clearBonding();
	obj.rs->endUndoState();
	return Command::Success;
}

// Clear bonds in current model ('clearbonds')
int Command::function_CA_CLEARSELECTEDBONDS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Clear Bonds in Selection");
	obj.rs->selectionClearBonding();
	obj.rs->endUndoState();
	return Command::Success;
}

// Add bond between atoms ('newbond <atom1> <atom2> [bondtype]')
int Command::function_CA_NEWBOND(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
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
	return Command::Success;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
int Command::function_CA_NEWBONDID(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
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
	return Command::Success;
}

// Calculate bonds in current model ('rebond')
int Command::function_CA_REBOND(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	// If we're reading from a file (via a filter) check for prefs override
	if (c->parent()->inputFile() == NULL)
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
	return Command::Success;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
int Command::function_CA_REBONDPATTERNS(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Calculate Bonding (Patterns)");
	obj.rs->patternCalculateBonding();
	obj.rs->endUndoState();
	return Command::Success;
}

// Calculate bonds restricted to current selection ('rebondselection')
int Command::function_CA_REBONDSELECTION(CommandNode *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return Command::Fail;
	obj.rs->beginUndoState("Calculate Bonding (Selection)");
	obj.rs->selectionCalculateBonding();
	obj.rs->endUndoState();
	return Command::Success;
}
