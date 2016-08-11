/*
	*** Undo Event - Model ColourScheme
	*** src/undo/model_colourscheme.h
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

#ifndef ATEN_UNDOEVENT_MODELCOLOURSCHEME_H
#define ATEN_UNDOEVENT_MODELCOLOURSCHEME_H

#include "undo/undoevent.h"
#include "base/prefs.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// Model ColourScheme Event
class ModelColourSchemeEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	ModelColourSchemeEvent();
	~ModelColourSchemeEvent();
	
	private:
	// Change data
	Prefs::ColouringScheme oldScheme_, newScheme_;

	public:
	// Set change data
	void set(Prefs::ColouringScheme oldScheme, Prefs::ColouringScheme newScheme);
	// Undo stored change
	void undo(Model* m);
	// Print change information
	void print();
};

ATEN_END_NAMESPACE

#endif
