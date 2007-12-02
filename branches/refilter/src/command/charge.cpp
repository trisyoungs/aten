/*
	*** Script charge functions
	*** src/script/charge.cpp
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
#include "base/debug.h"
#include "base/prefs.h"
#include "classes/pattern.h"

// Charge-related script commands (root=SR_CHARGE)
bool script::command_charge(command_node<script_command> *cmd)
{
	dbg_begin(DM_CALLS,"script::command_charge");
	bool result = TRUE;
	atom *i;
	pattern *p;
	model *frame;
	model *m = check_activemodel(text_from_SC(cmd->get_command()));
	if (m == NULL)
	{
		dbg_end(DM_CALLS,"script::command_charge");
		return FALSE;
	}
	switch (cmd->get_command())
	{
		case (SC_CLEARCHARGES):		// Clears charge in current model ('clearcharges')
			m->clear_charges();
			break;
		case (SC_CHARGEFF):	// Assign charges from forcefield atom types ('chargeff')
			m->assign_charges(QS_FF);
			m->get_currentframe();
			break;
		case (SC_CHARGEATOM):	// Assign charge to a single atom in the model ('chargeatom <id> <q>')
			i = m->find_atom(cmd->datavar[0]->get_as_int());
			if (i != NULL) i->set_charge(cmd->datavar[1]->get_as_int());
			else result = FALSE;
			break;
		case (SC_CHARGETYPE):	// Assign charges to a specified forcefield type ('chargetype <atomtype> <q>')
			printf("Not implemented yet!\n");
			result = FALSE;
			break;
		case (SC_CHARGEPATOM):	// Assign charge to a pattern atom, propagated over the model ('chargepatom <patname> <id> <q>')
			if (!check_activepattern("chargepatom")) result = FALSE;
			else m->charge_pattern_atom(activepattern,cmd->datavar[0]->get_as_int(),cmd->datavar[1]->get_as_double());
			break;
		case (SC_CHARGESELECTION):	// Assign charge to selected atoms in model ('chargeselection <q>')
			for (i = m->get_first_selected(); i != NULL; i = i->get_next_selected())
				i->set_charge(cmd->datavar[0]->get_as_double());
			break;
		case (SC_CHARGEFROMMODEL):	// Copy atomic charges from model to model's current trajectory frame
			frame = m->get_currentframe();
			if (frame == NULL) 
			{
				msg(DM_NONE,"Error - 'chargefrommodel' requires an active trajectory frame for the current model.\n");
				result = FALSE;
			}
			else frame->copy_atom_data(m, AD_Q);
			break;
		default:
			printf("Error - missed charge command?\n");
			result = FALSE;
			break;
	}
	dbg_end(DM_CALLS,"script::command_charge");
	return result;
}
