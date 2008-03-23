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

// Add bond between atoms ('newbond <atom1> <atom2> [bondtype]')
int CommandData::function_CA_NEWBOND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Third (optional) argument gives bond type
	BondType bt = BT_SINGLE;
	if (c->hasArg(2))
	{
		// Attempt to convert the argument into a BondType.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = BT_from_text(c->argc(2));
		else bt = (BondType) n;
	}
	// Add the bond
	obj.m->bondAtoms(c->argi(0)-1, c->argi(1)-1, bt);
	return CR_SUCCESS;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
int CommandData::function_CA_NEWBONDID(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// Third (optional) argument gives bond type
	BondType bt = BT_SINGLE;
	if (c->hasArg(2))
	{
		// Attempt to convert the argument into a BondType.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = BT_from_text(c->argc(2));
		else bt = (BondType) n;
	}
	// Find the atoms specified
	Atom *i = obj.m->findAtom(c->argi(0));
	Atom *j = obj.m->findAtom(c->argi(1));
	// Add the bond
	obj.m->bondAtoms(i, j, bt);
	return CR_SUCCESS;
}

// Augment bonds in current model ('augment')
int CommandData::function_CA_AUGMENT(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->augmentBonding();
	return CR_SUCCESS;
}

// Calculate bonds in current model ('rebond')
int CommandData::function_CA_REBOND(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	// If we're reading from a file (via a filter) check for prefs override
	if (c->parent()->inputFile() == NULL)
	{
		obj.m->clearBonding();
		obj.m->calculateBonding();
	}
	else if (prefs.bondOnLoad() != PS_NO)
	{
		obj.m->clearBonding();
		obj.m->calculateBonding();
	}
	return CR_SUCCESS;
}

// Clear bonds in current model ('clearbonds')
int CommandData::function_CA_CLEARBONDS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->clearBonding();
	return CR_SUCCESS;
}

// Change bond tolerance ('bondtol <d>')
int CommandData::function_CA_BONDTOLERANCE(Command *&c, Bundle &obj)
{
	prefs.setBondTolerance(c->argd(0));
	return CR_SUCCESS;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
int CommandData::function_CA_REBONDPATTERNS(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->patternCalculateBonding();
	return CR_SUCCESS;
}

// Calculate bonds restricted to current selection ('rebondselection')
int CommandData::function_CA_REBONDSELECTION(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->selectionCalculateBonding();
	return CR_SUCCESS;
}
