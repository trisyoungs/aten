/*
	*** Undo Event - Atom Fix
	*** src/undo/atom_fix.cpp
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

#include "undo/atom_fix.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomFixEvent::AtomFixEvent() : UndoEvent()
{
}

// Destructor
AtomFixEvent::~AtomFixEvent()
{
}

// Set change 
void AtomFixEvent::set(bool fix, int id)
{
	Messenger::enter("AtomFixEvent::set");
	direction_ = (fix ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("AtomFixEvent::set");
}

// Undo stored change
void AtomFixEvent::undo(Model* m)
{
	Messenger::enter("AtomFixEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom hide (UndoEvent::Redo) and show (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom fix - atom id = %i", targetId_);
		m->atomSetFixed(i, false);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom fix - atom id = %i", targetId_);
		m->atomSetFixed(i, true);
	}
	Messenger::exit("AtomFixEvent::undo");
}

// Print event info
void AtomFixEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom fix - atom id = %i\n", targetId_);
	else printf("       Atom free - atom id = %i\n", targetId_);
}

