/*
	*** Bonding command functions
	*** src/command/bonds.cpp
	Copyright T. Youngs 2007

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

// Augment bonds in current model ('augment')
int command_functions::function_CA_AUGMENT(command *&c, bundle &obj)
{
	obj.m->augment_bonding();
	return CR_SUCCESS;
}

// Calculate bonds in current model ('rebond')
int command_functions::function_CA_REBOND(command *&c, bundle &obj)
{
	obj.m->calculate_bonding();
	return CR_SUCCESS;
}

// Clear bonds in current model ('clearbonds')
int command_functions::function_CA_CLEARBONDS(command *&c, bundle &obj)
{
	obj.m->clear_bonding();
	return CR_SUCCESS;
}

// Change bond tolerance ('bondtol <d>')
int command_functions::function_CA_BONDTOLERANCE(command *&c, bundle &obj)
{
	prefs.set_bond_tolerance(c->argd(0));
	return CR_SUCCESS;
}

// Calculate bonds restricted to pattern molecules ('bondpatterns')
int command_functions::function_CA_BONDPATTERNS(command *&c, bundle &obj)
{
	obj.m->pattern_calculate_bonding();
	return CR_SUCCESS;

}

// Calculate bonds restricted to current selection ('bondselection')
int command_functions::function_CA_BONDSELECTION(command *&c, bundle &obj)
{
	obj.m->selection_calculate_bonding();
	return CR_SUCCESS;
}
