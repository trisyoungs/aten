/*
	*** Undo Event - Base Class
	*** src/undo/undoevent.h
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

#ifndef ATEN_UNDOEVENT_H
#define ATEN_UNDOEVENT_H

#include "templates/list.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// UndoEvent Base Class
class UndoEvent : public ListItem<UndoEvent>
{
	public:
	// Constructor / Destructor
	UndoEvent();
	virtual ~UndoEvent();
	// State change directions
	enum EventDirection { Undo, Redo };


	/*
	 * Data
	 */
	protected:
	// Direction of change
	EventDirection direction_;


	/*
	 * Actions
	 */
	public:
	// Undo stored change
	virtual void undo(Model* m) = 0;
	// Redo stored change
	void redo(Model* m);
	// Print change information
	virtual void print() = 0;
};

ATEN_END_NAMESPACE

#endif
