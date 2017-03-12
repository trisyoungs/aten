/*
	*** Undo Event - Atom Hide
	*** src/undo/atom_hide.cpp
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

#include "undo/atom_hide.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomHideEvent::AtomHideEvent() : UndoEvent()
{
}

// Destructor
AtomHideEvent::~AtomHideEvent()
{
}

// Set change 
void AtomHideEvent::set(bool hide, int id)
{
	Messenger::enter("AtomHideEvent::set");
	direction_ = (hide ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("AtomHideEvent::set");
}

// Undo stored change
void AtomHideEvent::undo(Model* m)
{
	Messenger::enter("AtomHideEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom hide (UndoEvent::Redo) and show (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom hide - atom id = %i", targetId_);
		m->atomSetHidden(i, false);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom hide - atom id = %i", targetId_);
		m->atomSetHidden(i, true);
	}
	Messenger::exit("AtomHideEvent::undo");
}

// Print event info
void AtomHideEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom hide - atom id = %i\n", targetId_);
	else printf("       Atom show - atom id = %i\n", targetId_);
}

