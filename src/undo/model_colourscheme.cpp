/*
	*** Undo Event - Model ColourScheme
	*** src/undo/model_colourscheme.cpp
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

#include "undo/model_colourscheme.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
ModelColourSchemeEvent::ModelColourSchemeEvent() : UndoEvent()
{
}

// Destructor
ModelColourSchemeEvent::~ModelColourSchemeEvent()
{
}

// Set change 
void ModelColourSchemeEvent::set(Prefs::ColouringScheme oldScheme, Prefs::ColouringScheme newScheme)
{
	Messenger::enter("ModelColourSchemeEvent::set");
	oldScheme_ = oldScheme;
	newScheme_ = newScheme;
	Messenger::exit("ModelColourSchemeEvent::set");
}

// Undo stored change
void ModelColourSchemeEvent::undo(Model* m)
{
	Messenger::enter("ModelColourSchemeEvent::undo");
	// Model colourscheme change, to oldScheme_ (UndoEvent::Undo) or newScheme_ (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing model colourscheme - to %s", Prefs::colouringScheme(oldScheme_));
		m->setColourScheme(oldScheme_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying model colourscheme - to %s", Prefs::colouringScheme(newScheme_));
		m->setColourScheme(newScheme_);
	}
	Messenger::exit("ModelColourSchemeEvent::undo");
}

// Print event info
void ModelColourSchemeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Model colourscheme from %s to %s\n", Prefs::colouringScheme(newScheme_), Prefs::colouringScheme(oldScheme_));
	else printf("       Model colourscheme from %s to %s\n",  Prefs::colouringScheme(oldScheme_), Prefs::colouringScheme(newScheme_));
}

