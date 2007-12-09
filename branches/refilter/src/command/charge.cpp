/*
	*** Charge command functions
	*** src/command/charge.cpp
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
#include "base/debug.h"
#include "classes/pattern.h"

// Assign charge to a single atom in the model ('chargeatom <id> <q>')
int command_functions::function_CA_CHARGEATOM(command *&c, objects &obj)
{
	atom *i = obj.m->find_atom(c->argi(0));
	if (i != NULL) i->set_charge(c->argi(1));
	else return CR_FAIL;
	return CR_SUCCESS;
}

// Assign charges from forcefield atom types ('chargeff')
int command_functions::function_CA_CHARGEFF(command *&c, objects &obj)
{
	obj.m->assign_charges(QS_FF);
	return CR_SUCCESS;
}

// Copy atomic charges from model to model's current trajectory frame
int command_functions::function_CA_CHARGEFROMMODEL(command *&c, objects &obj)
{
	model *frame = obj.m->get_currentframe();
	if (frame == NULL) 
	{
		msg(DM_NONE,"Error - 'chargefrommodel' requires an active trajectory frame in the current model.\n");
		return CR_FAIL;
	}
	else frame->copy_atom_data(obj.m, AD_Q);
	return CR_SUCCESS;
}

// Assign charge to a pattern atom, propagated over the model ('chargepatom <patname> <id> <q>')
int command_functions::function_CA_CHARGEPATOM(command *&c, objects &obj)
{
	obj.m->charge_pattern_atom(obj.p,c->argi(0),c->argd(1));
	return CR_SUCCESS;
}

// Assign charge to selected atoms in model ('chargeselection <q>')
int command_functions::function_CA_CHARGESELECTION(command *&c, objects &obj)
{
	for (atom *i = obj.m->get_first_selected(); i != NULL; i = i->get_next_selected())
		i->set_charge(c->argd(0));
}

// Assign charges to a specified forcefield type ('chargetype <atomtype> <q>')
int command_functions::function_CA_CHARGETYPE(command *&c, objects &obj)
{
	printf("Not implemented yet!\n");
	return CR_FAIL;
}

// Clears charge in current model ('clearcharges')
int command_functions::function_CA_CLEARCHARGES(command *&c, objects &obj)
{
	obj.m->clear_charges();
	return CR_SUCCESS;
}
