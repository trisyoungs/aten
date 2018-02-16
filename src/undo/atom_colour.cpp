/*
	*** Undo Event - Atom Colour
	*** src/undo/atom_colour.cpp
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

#include "undo/atom_colour.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomColourEvent::AtomColourEvent() : UndoEvent()
{
}

// Destructor
AtomColourEvent::~AtomColourEvent()
{
}

// Set change 
void AtomColourEvent::set(int id, double oldr, double oldg, double oldb, double olda, double newr, double newg, double newb, double newa)
{
	Messenger::enter("AtomColourEvent::set");
	targetId_ = id;
	oldColour_[0] = oldr;
	oldColour_[1] = oldg;
	oldColour_[2] = oldb;
	oldColour_[3] = olda;
	newColour_[0] = newr;
	newColour_[1] = newg;
	newColour_[2] = newb;
	newColour_[3] = newa;
	Messenger::exit("AtomColourEvent::set");
}

// Undo stored change
void AtomColourEvent::undo(Model* m)
{
	Messenger::enter("AtomColourEvent::undo");
	// Atom charge change - from realData[1] to realData[0] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	Atom* i, **modelatoms = m->atomArray();
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom charge change - atom %i, from (%f,%f,%f,%f) to (%f,%f,%f,%f)\n", targetId_, newColour_[0], newColour_[1], newColour_[2], newColour_[3], oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3]);
		i->setColour(oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3]);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom charge change - atom %i, from (%f,%f,%f,%f) to (%f,%f,%f,%f)\n", targetId_, oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3], newColour_[0], newColour_[1], newColour_[2], newColour_[3]);
		i->setColour(newColour_[0], newColour_[1], newColour_[2], newColour_[3]);
	}
	Messenger::exit("AtomColourEvent::undo");
}

// Print event info
void AtomColourEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom charge change - atom %i, from (%f,%f,%f,%f) to (%f,%f,%f,%f)\n", targetId_, newColour_[0], newColour_[1], newColour_[2], newColour_[3], oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3]);
	else printf("       Atom charge change - atom %i, from (%f,%f,%f,%f) to (%f,%f,%f,%f)\n", targetId_, oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3], newColour_[0], newColour_[1], newColour_[2], newColour_[3]);
}

