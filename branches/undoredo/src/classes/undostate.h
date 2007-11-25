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

// State change events
enum undo_event { UE_NONE, UE_ATOM, UE_BOND, UE_GEOMETRY, UE_SELECT, UE_TRANSMUTE, UE_BONDORDER };

// State change directions
enum undo_dir { UD_PLUS, UD_MINUS };

#include "classes/dnchar.h"
#include "templates/list.h"
#include "templates/matrix3.h"

// Forward declarations
class model;
class atom;

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
	// Direction of change
	undo_dir direction;
	// Atom data describing the change
	atom *atomdata[2];
	// Matrix data describing the change
	mat3<double> *matrixdata[2];
	// Generally-applicable data
	int data[4];

	public:
	// Set change data (atoms)
	void set(int ec, atom *i, atom *j = NULL);
	// Set change data (integers)
	void set(int ec, int i, int j = 0, int k = 0, int l = 0);

	/*
	// Actions
	*/
	public:
	// Reverse (undo) stored change
	void reverse(model *m);
	// Perform (redo) stored change
	void perform(model *m);
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
	// Reverse (undo) the changes specified in the state
	void reverse(model *m);
	// Perform (redo) the changes specified in the state
	void perform(model *m);
};

#endif
