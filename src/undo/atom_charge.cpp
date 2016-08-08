/*
	*** Undo Event - Atom Charge
	*** src/undo/atom_charge.cpp
	Copyright T. Youngs 2007-2016

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

#include "undo/atom_charge.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomChargeEvent::AtomChargeEvent() : UndoEvent()
{
}

// Destructor
AtomChargeEvent::~AtomChargeEvent()
{
}

// Set change 
void AtomChargeEvent::set(int id, double oldcharge, double newcharge)
{
	Messenger::enter("AtomChargeEvent::set");
	targetId_ = id;
	oldCharge_ = oldcharge;
	newCharge_ = newcharge;
	Messenger::exit("AtomChargeEvent::set");
}

// Undo stored change
void AtomChargeEvent::undo(Model* m)
{
	Messenger::enter("AtomChargeEvent::undo");
	// Atom charge change - from realData[1] to realData[0] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	Atom* i, **modelatoms = m->atomArray();
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom charge change - atom %i, from %i to %i", targetId_, newCharge_, oldCharge_);
		i->setCharge(oldCharge_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom charge change - atom %i, from %i to %i", targetId_, oldCharge_, newCharge_);
		i->setCharge(newCharge_);
	}
	Messenger::exit("AtomChargeEvent::undo");
}

// Print event info
void AtomChargeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, newCharge_, oldCharge_);
	else printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, oldCharge_, newCharge_);
}

