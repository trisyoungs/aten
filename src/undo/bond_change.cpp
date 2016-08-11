/*
	*** Undo Event - Bond Change
	*** src/undo/bond_change.cpp
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

#include "undo/bond_change.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
BondChangeEvent::BondChangeEvent() : UndoEvent()
{
}

// Destructor
BondChangeEvent::~BondChangeEvent()
{
}

// Set change 
void BondChangeEvent::set(int id1, int id2, Bond::BondType oldbondtype, Bond::BondType newbondtype)
{
	Messenger::enter("BondChangeEvent::set");
	targetId1_ = id1;
	targetId2_ = id2;
	oldBondType_ = oldbondtype;
	newBondType_ = newbondtype;
	Messenger::exit("BondChangeEvent::set");
}

// Undo stored change
void BondChangeEvent::undo(Model* m)
{
	Messenger::enter("BondChangeEvent::undo");
	Atom* i, *j, **modelatoms = m->atomArray();
	// Bond order change - from newBondType_ to oldBondType_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	Bond* b = i->findBond(j);
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing bond order change - atoms %i-%i, old = %i, new = %i", targetId1_, targetId2_, newBondType_, oldBondType_);
		m->changeBond(b, oldBondType_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying bond order change - atoms %i-%i, old = %i, new = %i", targetId1_, targetId2_, oldBondType_, newBondType_);
		m->changeBond(b, newBondType_);
	}
	Messenger::exit("BondChangeEvent::undo");
}

// Print event info
void BondChangeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, newBondType_, oldBondType_);
	else printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, oldBondType_, newBondType_);
}

