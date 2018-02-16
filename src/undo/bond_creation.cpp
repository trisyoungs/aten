/*
	*** Undo Event - Bond Creation
	*** src/undo/bond_creation.cpp
	Copyright T. Youngs 2007-2018

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

#include "undo/bond_creation.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
BondCreationEvent::BondCreationEvent() : UndoEvent()
{
}

// Destructor
BondCreationEvent::~BondCreationEvent()
{
}

// Set change 
void BondCreationEvent::set(bool creation, int id1, int id2, Bond::BondType bondtype)
{
	Messenger::enter("BondCreationEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	targetId1_ = id1;
	targetId2_ = id2;
	bondType_ = bondtype;
	Messenger::exit("BondCreationEvent::set");
}

// Undo stored change
void BondCreationEvent::undo(Model* m)
{
	Messenger::enter("BondCreationEvent::undo");
	Atom* i, *j, **modelatoms = m->atomArray();
	// Bond creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	if (direction_ == UndoEvent::Undo)
	{
		// Delete bond between stored atom ids
		Messenger::print(Messenger::Verbose, "Reversing bond creation - atom ids = %i %i", targetId1_, targetId2_);
		m->unbondAtoms(i,j);
	}
	else
	{
		// Add bond between stored atom ids
		Messenger::print(Messenger::Verbose, "Reversing bond deletion - atom ids = %i %i", targetId1_, targetId2_);
		m->bondAtoms(i,j,bondType_);
	}
	Messenger::exit("BondCreationEvent::undo");
}

// Print event info
void BondCreationEvent::print()
{
	if (direction_ == UndoEvent::Undo)	printf("       Bond creation - atom ids = %i %i\n", targetId1_, targetId2_);
	else printf("       Bond deletion - atom ids = %i %i\n", targetId1_, targetId2_);
}

