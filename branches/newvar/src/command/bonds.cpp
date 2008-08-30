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
int CommandData::function_CA_AUGMENT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Augment Bonds");
	obj.rs->augmentBonding();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Change bond tolerance ('bondtol <d>')
int CommandData::function_CA_BONDTOLERANCE(Command *&c, Bundle &obj)
{
	prefs.setBondTolerance(c->argd(0));
	return CR_SUCCESS;
}

// Clear bonds in current model ('clearbonds')
int CommandData::function_CA_CLEARBONDS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Clear Bonding");
	obj.rs->clearBonding();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Retrieve bond info ('getbond <id> [var]')
int CommandData::function_CA_GETBOND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	Bond *b = obj.rs->bond(c->argi(0)-1);
	if (b == NULL) return CR_FAIL;
	// Set bond information
	c->parent()->setPatternBoundVariables(c->arg(1)->name(), b);
	return CR_SUCCESS;
}

// Add bond between atoms ('newbond <atom1> <atom2> [bondtype]')
int CommandData::function_CA_NEWBOND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
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
	return CR_SUCCESS;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
int CommandData::function_CA_NEWBONDID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
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
	return CR_SUCCESS;
}

// Calculate bonds in current model ('rebond')
int CommandData::function_CA_REBOND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
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
	return CR_SUCCESS;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
int CommandData::function_CA_REBONDPATTERNS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Calculate Bonding (Patterns)");
	obj.rs->patternCalculateBonding();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}

// Calculate bonds restricted to current selection ('rebondselection')
int CommandData::function_CA_REBONDSELECTION(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return CR_FAIL;
	obj.rs->beginUndoState("Calculate Bonding (Selection)");
	obj.rs->selectionCalculateBonding();
	obj.rs->endUndoState();
	return CR_SUCCESS;
}
