/*
	*** Undo level storage
	*** src/classes/undo.h
	Copyright T. Youngs 2007,2008

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

// Change logs
enum change_log { LOG_STRUCTURE, LOG_COORDS, LOG_VISUAL, LOG_SELECTION, LOG_CAMERA, LOG_TOTAL, LOG_NITEMS };

// State change events
enum undo_event { UE_NONE, UE_ATOM, UE_BOND, UE_MEASUREMENT, UE_SELECT, UE_TRANSMUTE, UE_BONDORDER, UE_CELL, UE_LABEL, UE_TRANSLATE };

// State change directions
enum undo_dir { UD_REVERSE, UD_FORWARDS };

#include "model/model.h"
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
	// Vector data describing the change
	vec3<double> *vecdata[4];
	// Generally-applicable data
	int data[5];

	public:
	// Set change data (atoms)
	void set(int ec, atom *i, atom *j = NULL);
	// Set change data (integers)
	void set(int ec, int i, int j = -1, int k = -1, int l = -1, int m = -1);
	// Set change data (matrices)
	void set(int ec, mat3<double> *m1, mat3<double> *m2 = NULL);
	// Set change data (vector)
	void set(int ec, vec3<double> *v1, vec3<double> *v2 = NULL, vec3<double> *v3 = NULL, vec3<double> *v4 = NULL);

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
	// Logs at start of state
	int startlogs[LOG_NITEMS];
	// Logs at end of state
	int endlogs[LOG_NITEMS];

	public:
	// List of atomic changes for this level
	list<change> changes;
	// Set log point at start of state
	void set_startlog(change_log log, int value) { startlogs[log] = value; }
	// Get structure log point at start of state
	int get_startlog(change_log log) { return startlogs[log]; }
	// Set log point at end of state
	void set_endlog(change_log log, int value) { endlogs[log] = value; }
	// Get structure log point at end of state
	int get_endlog(change_log log) { return endlogs[log]; }
	// Check difference between LOG_STRUCTURE and LOG_COORDS between start/end points
	bool logs_differ();
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
