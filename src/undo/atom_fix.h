/*
	*** Undo Event - Atom Fix
	*** src/undo/atom_fix.h
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

#ifndef ATEN_UNDOEVENT_ATOMFIX_H
#define ATEN_UNDOEVENT_ATOMFIX_H

#include "undo/undoevent.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// Atom Fix Event
class AtomFixEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	AtomFixEvent();
	~AtomFixEvent();

	private:
	// Change data
	int targetId_;

	public:
	// Set change data
	void set(bool fix, int id);
	// Undo stored change
	void undo(Model* m);
	// Print change information
	void print();
};

ATEN_END_NAMESPACE

#endif
