/*
	*** Undo Event - Cell Set
	*** src/undo/cell_set.cpp
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

#include "undo/cell_set.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
CellSetEvent::CellSetEvent() : UndoEvent()
{
}

// Destructor
CellSetEvent::~CellSetEvent()
{
}

// Set change 
void CellSetEvent::set(Matrix oldaxes, Matrix newaxes, bool ohs, bool nhs)
{
	Messenger::enter("CellSetEvent::set");
	oldAxes_ = oldaxes;
	newAxes_ = newaxes;
	oldHasCell_ = ohs;
	newHasCell_ = nhs;
	Messenger::exit("CellSetEvent::set");
}

// Undo stored change
void CellSetEvent::undo(Model* m)
{
	Messenger::enter("CellSetEvent::undo");
	// Cell change - from newLengths/Angles to oldLengths/Angles (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing cell change");
		if (!oldHasCell_) m->removeCell();
		else m->setCell(oldAxes_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying cell change");
		if (!newHasCell_) m->removeCell();
		else m->setCell(newAxes_);
	}
	Messenger::exit("CellSetEvent::undo");
}

// Print event info
void CellSetEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Cell change\n");
	else printf("       Cell change\n");
}

