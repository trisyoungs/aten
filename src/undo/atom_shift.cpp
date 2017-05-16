/*
	*** Undo Event - Atom Shift
	*** src/undo/atom_shift.cpp
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

#include "undo/atom_shift.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomShiftEvent::AtomShiftEvent() : UndoEvent()
{
}

// Destructor
AtomShiftEvent::~AtomShiftEvent()
{
}

// Set change 
void AtomShiftEvent::set(int id, int delta)
{
	Messenger::enter("AtomShiftEvent::set");
	targetId_ = id;
	delta_ = delta;
	Messenger::exit("AtomShiftEvent::set");
}

// Undo stored change
void AtomShiftEvent::undo(Model* m)
{
	Messenger::enter("AtomShiftEvent::undo");
	// Atom list position change - -data[1] (UndoEvent::Undo) or +data[1] places in list (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom shift - atom %i moves %i places", targetId_+delta_, -delta_);
		m->moveAtom(targetId_+delta_, -delta_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Performing atom shift - atom %i moves %i places", targetId_, delta_);
		m->moveAtom(targetId_, delta_);
	}
	Messenger::exit("AtomShiftEvent::undo");
}

// Print event info
void AtomShiftEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom shift - atom %i moves %i places\n", targetId_+delta_, -delta_);
	else printf("       Atom shift - atom %i moves %i places\n", targetId_, delta_);
}

