/*
	*** Undo Event - Atom Label
	*** src/undo/atom_label.cpp
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

#include "undo/atom_label.h"
#include "model/model.h"
#include "base/prefs.h"

ATEN_USING_NAMESPACE

// Constructor
AtomLabelEvent::AtomLabelEvent() : UndoEvent()
{
}

// Destructor
AtomLabelEvent::~AtomLabelEvent()
{
}

// Set change 
void AtomLabelEvent::set(int id, int oldlabels, int newlabels)
{
	Messenger::enter("AtomLabelEvent::set");
	targetId_ = id;
	oldLabels_ = oldlabels;
	newLabels_ = newlabels;
	Messenger::exit("AtomLabelEvent::set");
}

// Undo stored change
void AtomLabelEvent::undo(Model* m)
{
	Messenger::enter("AtomLabelEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom label change - from data[2] to data[1] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom label change - atom %i, from %i to %i", targetId_, newLabels_, oldLabels_);
		i->setLabels(oldLabels_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom label change - atom %i, from %i to %i", targetId_, oldLabels_, newLabels_);
		i->setLabels(newLabels_);
	}
	Messenger::exit("AtomLabelEvent::undo");
}

// Print event info
void AtomLabelEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Reversing atom label change - atom %i, from %i to %i\n", targetId_, newLabels_, oldLabels_);
	else printf("       Replaying atom label change - atom %i, from %i to %i\n", targetId_, oldLabels_, newLabels_);
}

