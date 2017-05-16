/*
	*** Undo Event - Atom Select
	*** src/undo/atom_select.cpp
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

#include "undo/atom_select.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomSelectEvent::AtomSelectEvent() : UndoEvent()
{
}

// Destructor
AtomSelectEvent::~AtomSelectEvent()
{
}

// Set change 
void AtomSelectEvent::set(bool select, int id)
{
	Messenger::enter("SelectionEvent::set");
	direction_ = (select ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("SelectionEvent::set");
}

// Undo stored change
void AtomSelectEvent::undo(Model* m)
{
	Messenger::enter("AtomSelectEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom selection (UndoEvent::Redo) and deselection (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom selection - atom id = %i", targetId_);
		m->deselectAtom(i);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom selection - atom id = %i", targetId_);
		m->selectAtom(i);
	}
	Messenger::exit("AtomSelectEvent::undo");
}

// Print event info
void AtomSelectEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom selection - atom id = %i\n", targetId_);
	else printf("       Atom deselection - atom id = %i\n", targetId_);
}

