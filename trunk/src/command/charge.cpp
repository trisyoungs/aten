/*
	*** Charge command functions
	*** src/command/charge.cpp
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
#include "base/debug.h"
#include "classes/pattern.h"
#include "model/model.h"

// Assign charges from forcefield atom types ('chargeff')
int CommandData::function_CA_CHARGEFF(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->assignCharges(QS_FF);
	return CR_SUCCESS;
}

// Copy atomic charges from model to model's current trajectory frame
int CommandData::function_CA_CHARGEFROMMODEL(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	Model *frame = obj.m->currentFrame();
	if (frame == NULL) 
	{
		msg(DM_NONE,"Error - 'chargefrommodel' requires an active trajectory frame in the current model.\n");
		return CR_FAIL;
	}
	else frame->copyAtomData(obj.m, AD_Q);
	return CR_SUCCESS;
}

// Assign charge to a pattern atom, propagated over the model ('chargepatom <patname> <id> <q>')
int CommandData::function_CA_CHARGEPATOM(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->chargePatternAtom(obj.p,c->argi(0),c->argd(1));
	return CR_SUCCESS;
}

// Assign charge to selected atoms in model ('chargeselection <q>')
int CommandData::function_CA_CHARGESELECTION(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	for (Atom *i = obj.m->firstSelected(); i != NULL; i = i->nextSelected())
		i->setCharge(c->argd(0));
	return CR_SUCCESS;
}

// Assign charges to a specified forcefield type ('chargetype <atomtype> <q>')
int CommandData::function_CA_CHARGETYPE(Command *&c, Bundle &obj)
{
	printf("Not implemented yet!\n");
	return CR_FAIL;
}

// Clears charge in current model ('clearcharges')
int CommandData::function_CA_CLEARCHARGES(Command *&c, Bundle &obj)
{
	if (obj.notifyNull(BP_MODEL)) return CR_FAIL;
	obj.m->clearCharges();
	return CR_SUCCESS;
}
