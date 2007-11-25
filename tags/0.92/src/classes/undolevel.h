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
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef H_UNDOLEVEL_H
#define H_UNDOLEVEL_H

// Undo Actions
enum edit_change { EC_ADDATOM, EC_DELETEATOM, EC_ADDBOND, EC_DELETEBOND, EC_NITEMS };

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
};

// Undo level
class undolevel
{
	public:
	// Constructor / destructor
	undolevel();
	~undolevel();
	// List pointers
	undolevel *prev, *next;

	/*
	// Description
	*/
	private:
	// Short text describing the change
	dnchar text;
	// List of atomic changes for this level
	list<change> changes;
};

#endif
