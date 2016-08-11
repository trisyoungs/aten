/*
	*** Undo Event - Model DrawStyle
	*** src/undo/model_drawstyle.cpp
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

#include "undo/model_drawstyle.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
ModelDrawStyleEvent::ModelDrawStyleEvent() : UndoEvent()
{
}

// Destructor
ModelDrawStyleEvent::~ModelDrawStyleEvent()
{
}

// Set change 
void ModelDrawStyleEvent::set(Prefs::DrawStyle oldStyle, Prefs::DrawStyle newStyle)
{
	Messenger::enter("ModelDrawStyleEvent::set");
	oldStyle_ = oldStyle;
	newStyle_ = newStyle;
	Messenger::exit("ModelDrawStyleEvent::set");
}

// Undo stored change
void ModelDrawStyleEvent::undo(Model* m)
{
	Messenger::enter("ModelDrawStyleEvent::undo");
	// Model drawStyle change, to oldStyle_ (UndoEvent::Undo) or newStyle_ (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing model drawstyle - to %s", Prefs::drawStyle(oldStyle_));
		m->setDrawStyle(oldStyle_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying model drawstyle - to %s", Prefs::drawStyle(newStyle_));
		m->setDrawStyle(newStyle_);
	}
	Messenger::exit("ModelDrawStyleEvent::undo");
}

// Print event info
void ModelDrawStyleEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Model drawstyle from %s to %s\n", Prefs::drawStyle(newStyle_), Prefs::drawStyle(oldStyle_));
	else printf("       Model drawstyle from %s to %s\n",  Prefs::drawStyle(oldStyle_), Prefs::drawStyle(newStyle_));
}

