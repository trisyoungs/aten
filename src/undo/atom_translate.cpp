/*
	*** Undo Event - Atom Translate
	*** src/undo/atom_translate.cpp
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

#include "undo/atom_translate.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
AtomTranslateEvent::AtomTranslateEvent() : UndoEvent()
{
}

// Destructor
AtomTranslateEvent::~AtomTranslateEvent()
{
}

// Set change 
void AtomTranslateEvent::set(int id, Vec3<double> delta)
{
	Messenger::enter("AtomTranslateEvent::set");
	targetId_ = id;
	delta_ = delta;
	Messenger::exit("AtomTranslateEvent::set");
}

// Undo stored change
void AtomTranslateEvent::undo(Model* m)
{
	Messenger::enter("AtomTranslateEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom position change - add (UndoEvent::Undo) or subtract (UndoEvent::Redo) delta_.
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom translation - atom %i, subtracting %f %f %f", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() -= delta_;
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom translation - atom %i, adding %f %f %f", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() += delta_;
	}
	Messenger::exit("AtomTranslateEvent::undo");
}

// Print event info
void AtomTranslateEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom translation - atom %i, subtracting %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
	else printf("       Atom translation - atom %i, adding %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
}

