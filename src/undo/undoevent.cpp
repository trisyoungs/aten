/*
	*** Undo Event - Base Class
	*** src/undo/undoevent.cpp
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

#include "undo/undoevent.h"
#include "base/messenger.h"

ATEN_USING_NAMESPACE

// Constructor
UndoEvent::UndoEvent() : ListItem<UndoEvent>()
{
	// Private variables
	direction_ = UndoEvent::Undo;
}

// Destructor
UndoEvent::~UndoEvent()
{
}

// Redo stored change
void UndoEvent::redo(Model* m)
{
	Messenger::enter("UndoEvent::redo");

	// Reverse the direction of the change, then run the stored events
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);

	undo(m);

	// Set direction back to its previous value
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);

	Messenger::exit("UndoeEvent::redo");
}
