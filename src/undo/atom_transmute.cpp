/*
	*** Undo Event - Atom Transmute
	*** src/undo/atom_transmute.cpp
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

#include "undo/atom_transmute.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomTransmuteEvent::AtomTransmuteEvent() : UndoEvent()
{
}

// Destructor
AtomTransmuteEvent::~AtomTransmuteEvent()
{
}

// Set change 
void AtomTransmuteEvent::set(int id, int oldel, int newel)
{
	Messenger::enter("AtomTransmuteEvent::set");
	targetId_ = id;
	oldEl_ = oldel;
	newEl_ = newel;
	Messenger::exit("AtomTransmuteEvent::set");
}

// Undo stored change
void AtomTransmuteEvent::undo(Model* m)
{
	Messenger::enter("AtomTransmuteEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom transmute - newEl_ to oldEl_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom transmute - atom %i, old = %i, new = %i", targetId_, newEl_, oldEl_);
		m->transmuteAtom(i, oldEl_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom transmute - atom %i, old = %i, new = %i", targetId_, oldEl_, newEl_);
		m->transmuteAtom(i, newEl_);
	}
	Messenger::exit("AtomTransmuteEvent::undo");
}

// Print event info
void AtomTransmuteEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, newEl_, oldEl_);
	else printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, oldEl_, newEl_);
}

