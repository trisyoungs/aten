/*
	*** Script bonding functions
	*** src/script/bonds.cpp
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

#include "script/script.h"
#include "base/prefs.h"
#include "base/elements.h"
#include "base/sysfunc.h"
#include "base/debug.h"

// Bonding-related script commands (root=SR_BONDS)
bool script::command_bonds(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_bonds");
	bool result = TRUE;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_bonds");
		return FALSE;
	}
	atom *i;
	switch (cmd->get_command())
	{
		case (SC_AUGMENT):		// Augment bonds in current model ('augment')
			m->augment_bonding();
			break;
		case (SC_REBOND):		// Calculate bonds in current model ('rebond')
			m->calculate_bonding();
			break;
		case (SC_CLEARBONDS):		// Clear bonds in current model ('clearbonds')
			m->clear_bonding();
			break;
		case (SC_BONDTOLERANCE):	// Change bond tolerance ('bondtol <d>')
			prefs.set_bond_tolerance(cmd->datavar[0]->get_as_double());
			break;
		case (SC_BONDPATTERNS):		// Calculate bonds restricted to pattern molecules ('bondpatterns')
			m->pattern_calculate_bonding();
			break;
		case (SC_BONDSELECTION):	// Calculate bonds restricted to current selection ('bondselection')
			m->selection_calculate_bonding();
			break;
		default:
			printf("Error - missed bonds command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_bonds");
	return result;
}

