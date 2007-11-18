/*
	*** Undo level storage
	*** src/classes/undo.h
	Copyright T. Youngs 2007

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef H_UNDOLEVEL_H
#define H_UNDOLEVEL_H

// State change targets
enum undo_event { UE_ADDATOM, UE_DELETEATOM, UE_ADDBOND, UE_DELETEBOND, UE_ADDGEOMETRY, UE_DELETEGEOMETRY, UE_TRANSMUTE, UE_NITEMS };

#include "classes/atom.h"
#include "classes/dnchar.h"
#include "templates/list.h"

// Single change
class change
{
	public:
	// Constructor / destructor
	change();
	~change();
	// List pointers
	change *prev, *next;

	/*
	// Data
	*/
	private:
	// Type of change
	undo_event type;
	// Data (atom ids etc.) describing the object before the change
	atom olddata;
	// Data (atom ids etc.) describing the object after the change
	atom newdata;

	public:
	// Set change data
	void set(undo_event ec, atom *old, atom *nu);

	/*
	// Actions
	*/
	public:
	// Perform stored state change
	void perform();
	// Reverse stored state change
	void reverse();
};

// Undo state
class undostate
{
	public:
	// Constructor / destructor
	undostate();
	~undostate();
	// List pointers
	undostate *prev, *next;

	/*
	// Changelist
	*/
	private:
	// Short text describing the change
	dnchar text;

	public:
	// List of atomic changes for this level
	list<change> changes;
	// Set the text associated with the current undo state
	void set_text(const char *s) { text = s; }
	// Return the current text associated with the state
	const char *get_text() { return text.get(); }
	// Revert (undo) the changes specified in the state
	void revert();
	// Perform (redo) the changes specified in the state
	void perform();
};

#endif
