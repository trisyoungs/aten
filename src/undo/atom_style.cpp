/*
	*** Undo Event - Atom Style
	*** src/undo/atom_style.cpp
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

#include "undo/atom_style.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomStyleEvent::AtomStyleEvent() : UndoEvent()
{
}

// Destructor
AtomStyleEvent::~AtomStyleEvent()
{
}

// Set change 
void AtomStyleEvent::set(int id, Prefs::DrawStyle oldstyle, Prefs::DrawStyle newstyle)
{
	Messenger::enter("AtomStyleEvent::set");
	targetId_ = id;
	oldStyle_ = oldstyle;
	newStyle_ = newstyle;
	Messenger::exit("AtomStyleEvent::set");
}

// Undo stored change
void AtomStyleEvent::undo(Model* m)
{
	Messenger::enter("AtomStyleEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom style change - newStyle_ to oldStyle_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom style change - atom %i, old = %i, new = %i", targetId_, newStyle_, oldStyle_);
		m->atomSetStyle(i, oldStyle_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom style change - atom %i, old = %i, new = %i", targetId_, oldStyle_, newStyle_);
		m->atomSetStyle(i, newStyle_);
	}
	Messenger::exit("AtomStyleEvent::undo");
}

// Print event info
void AtomStyleEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom style - atom %i, old = %i, new = %i\n", targetId_, newStyle_, oldStyle_);
	else printf("       Atom style - atom %i, old = %i, new = %i\n", targetId_, oldStyle_, newStyle_);
}

