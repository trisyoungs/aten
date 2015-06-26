/*
	*** Charge Commands
	*** src/command/charge.cpp
	Copyright T. Youngs 2007-2015

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

#include "parser/commandnode.h"
#include "command/commands.h"
#include "model/bundle.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Assign charge to selected atoms in model ('charge <q>'), or get charge of current selection
bool Commands::function_Charge(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	double q = 0.0;
	if (c->hasArg(0))
	{
		obj.rs()->beginUndoState("Charge selected atoms");
		for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) obj.rs()->atomSetCharge(ri->item, c->argd(0));
		obj.rs()->endUndoState();
	}
	else
	{
		q = 0.0;
		for (RefListItem<Atom,int>* ri = obj.rs()->selection(); ri != NULL; ri = ri->next) q += ri->item->charge();
	}
	rv.set(q);
	return true;
}

// Assign charges from forcefield atom types ('chargeff')
bool Commands::function_ChargeFF(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Assign forcefield charges");
	bool result = obj.rs()->assignForcefieldCharges();
	obj.rs()->endUndoState();
	return (result);
}

// Copy atomic charges from model to model's current trajectory frame
bool Commands::function_ChargeFromModel(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	if (obj.rs() == obj.m) 
	{
		Messenger::print("Error - 'chargefrommodel' requires an active trajectory frame in the current model.");
		return false;
	}
	else obj.rs()->copyAtomData(obj.m, Atom::ChargeData);
	return true;
}

// Assign charge to a pattern atom, propagated over the model ('chargepatom <id> <q>')
bool Commands::function_ChargePAtom(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Charge single pattern atom");
	obj.rs()->chargePatternAtom(obj.p,c->argi(0),c->argd(1));
	obj.rs()->endUndoState();
	return true;
}

// Assign charges to a specified forcefield type ('chargetype <atomtype> <q>')
bool Commands::function_ChargeType(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	printf("Not implemented yet!\n");
	return false;
}

// Clears charge in current model ('clearcharges')
bool Commands::function_ClearCharges(CommandNode* c, Bundle& obj, ReturnValue& rv)
{
	if (obj.notifyNull(Bundle::ModelPointer)) return false;
	obj.rs()->beginUndoState("Remove charges");
	obj.rs()->clearCharges();
	obj.rs()->endUndoState();
	return true;
}

