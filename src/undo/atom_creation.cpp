/*
	*** Undo Event - Atom Creation
	*** src/undo/atom_creation.cpp
	Copyright T. Youngs 2007-2017

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

#include "undo/atom_creation.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomCreationEvent::AtomCreationEvent() : UndoEvent()
{
}

// Destructor
AtomCreationEvent::~AtomCreationEvent()
{
}

// Set change 
void AtomCreationEvent::set(bool creation, Atom* i)
{
	Messenger::enter("AtomCreationEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	// Copy atom data from source atoms, unless they are NULL
	if (i != NULL)
	{
		//atomData_ = new Atom;
		atomData_.copy(i);
		atomData_.setId(i->id());
	}
	else printf("Null pointer passed to AtomCreationEvent::set()!\n");
	Messenger::exit("AtomCreationEvent::set");
}

// Undo stored change
void AtomCreationEvent::undo(Model* m)
{
	Messenger::enter("AtomCreationEvent::undo");
	Atom** modelatoms = m->atomArray();
	int id;
	// Atom creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	if (direction_ == UndoEvent::Undo)
	{
		// We delete the atom at the position referenced by the ID in the atom
		id = atomData_.id();
		Messenger::print(Messenger::Verbose, "Reversing atom creation - atom id = %i", id);
		m->deleteAtom(modelatoms[id]);
	}
	else
	{
		Atom* i;
		// Insert a new atom at the position before the stored atom id
		id = atomData_.id();
		Messenger::print(Messenger::Verbose, "Replaying atom creation - atom id = %i", id);
		if (id == 0) i = m->addCopy(NULL, &atomData_);
		else i = m->addCopy(modelatoms[id-1], &atomData_);
	}
}

// Print event info
void AtomCreationEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom creation - atom id = %i\n", atomData_.id());
	else printf("       Atom deletion - atom id = %i\n", atomData_.id());
}

