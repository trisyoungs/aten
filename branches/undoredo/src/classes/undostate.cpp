/*
	*** Undo state storage
	*** src/classes/undostate.cpp
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

#include "classes/undostate.h"

// Constructors
change::change()
{
	prev = NULL;
	next = NULL;
	type = UE_NITEMS;
}

undostate::undostate()
{
	prev = NULL;
	next = NULL;
}

// Destructors
change::~change()
{
}

undostate::~undostate()
{
}

// Set change (by passed variable types)
void change::set(undo_event ue, atom *old, atom *nu)
{
	dbg_begin(DM_CALLS,"change::set[atom,atom]");
	type = ue;
	// Copy atom data from source atoms, unless it is NULL
	if (old == NULL) olddata.reset();
	else olddata.copy(old);
	if (nu == NULL) newdata.reset();
	else newdata.copy(nu);
	dbg_end(DM_CALLS,"change::set[atom,atom]");
}

// Revert (undo) changes detailed in state
void undostate::revert()
{
}

// Perform (redo) changes detailed in state
void undostate::perform()
{
}
