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
int commanddata::function_CA_NEWBOND(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// Third (optional) argument gives bond type
	bond_type bt = BT_SINGLE;
	if (c->has_arg(2))
	{
		// Attempt to convert the argument into a bond_type.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = BT_from_text(c->argc(2));
		else bt = (bond_type) n;
	}
	// Add the bond
	obj.m->bond_atoms(c->argi(0)-1, c->argi(1)-1, bt);
	return CR_SUCCESS;
}

// Add bond between atoms with specified ids ('newbondid <id1> <id2> [bondtype]')
int commanddata::function_CA_NEWBONDID(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// Third (optional) argument gives bond type
	bond_type bt = BT_SINGLE;
	if (c->has_arg(2))
	{
		// Attempt to convert the argument into a bond_type.
		// Try direct conversion from number (bond order) first
		// If that fails, try string conversion. Then, give up.
		int n = c->argi(2);
		if ((n < 1) || (n > 3))	bt = BT_from_text(c->argc(2));
		else bt = (bond_type) n;
	}
	// Find the atoms specified
	atom *i = obj.m->find_atom(c->argi(0));
	atom *j = obj.m->find_atom(c->argi(1));
	// Add the bond
	obj.m->bond_atoms(i, j, bt);
	return CR_SUCCESS;
}

// Augment bonds in current model ('augment')
int commanddata::function_CA_AUGMENT(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->augment_bonding();
	return CR_SUCCESS;
}

// Calculate bonds in current model ('rebond')
int commanddata::function_CA_REBOND(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	// If we're reading from a file (via a filter) check for prefs override
	if (c->get_parent()->get_infile() == NULL)
	{
		obj.m->clear_bonding();
		obj.m->calculate_bonding();
	}
	else if (prefs.get_bond_on_load() != PS_NO)
	{
		obj.m->clear_bonding();
		obj.m->calculate_bonding();
	}
	return CR_SUCCESS;
}

// Clear bonds in current model ('clearbonds')
int commanddata::function_CA_CLEARBONDS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->clear_bonding();
	return CR_SUCCESS;
}

// Change bond tolerance ('bondtol <d>')
int commanddata::function_CA_BONDTOLERANCE(command *&c, bundle &obj)
{
	prefs.set_bond_tolerance(c->argd(0));
	return CR_SUCCESS;
}

// Calculate bonds restricted to pattern molecules ('rebondpatterns')
int commanddata::function_CA_REBONDPATTERNS(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->pattern_calculate_bonding();
	return CR_SUCCESS;
}

// Calculate bonds restricted to current selection ('rebondselection')
int commanddata::function_CA_REBONDSELECTION(command *&c, bundle &obj)
{
	if (obj.notify_null(BP_MODEL)) return CR_FAIL;
	obj.m->selection_calculate_bonding();
	return CR_SUCCESS;
}
