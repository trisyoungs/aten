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
#include "model/model.h"

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
	else
	{
		olddata.copy(old);
		olddata.set_id(old->get_id());
	}
	if (nu == NULL) newdata.reset();
	else
	{
		newdata.copy(nu);
		newdata.set_id(nu->get_id());
	}
	dbg_end(DM_CALLS,"change::set[atom,atom]");
}

// Set change (general)
void change::set(undo_event ue, int i, int j, int k)
{
	dbg_begin(DM_CALLS,"change::set[int]");
	type = ue;
	data[0] = i;
	data[1] = j;
	data[2] = k;
	dbg_end(DM_CALLS,"change::set[atom,atom]");
}

// Revert (undo) stored change
void change::revert(model *m)
{
	dbg_begin(DM_CALLS,"change::revert");
	atom **staticatoms = m->get_staticatoms();
	int id;
	switch (type)
	{
		// Undo atom creation
		case (UE_ADDATOM):
			// We delete the atom at the position referenced by the ID in the atom
			id = newdata.get_id();
			msg(DM_VERBOSE,"Undoing atom creation - atom id = %i\n",id);
			m->delete_atom(staticatoms[id]);
			break;
		// Undo atom deletion
		case (UE_DELETEATOM):
			// Insert a new atom at the position before the stored atom id
			id = newdata.get_id();
			msg(DM_VERBOSE,"Undoing atom deletion - atom id = %i\n",id);
			if (id == 0) m->add_copy(NULL, &newdata);
			else m->add_copy(staticatoms[id-1], &newdata);
			break;
		// Undo bond creation
		case (UE_ADDBOND):
			// Delete bond between stored atom ids
			msg(DM_VERBOSE,"Undoing bond creation - atom ids = %i %i\n",data[0],data[1]);
			m->unbond_atoms(staticatoms[data[0]],staticatoms[data[1]]);
			break;
		default:
			printf("Don't know how to undo change (type = %i)\n",type);
			break;
	}
	dbg_end(DM_CALLS,"change::revert");
}

// Perform (redo) stored change
void change::perform(model *m)
{
	dbg_begin(DM_CALLS,"change::perform");
	atom **staticatoms = m->get_staticatoms();
	atom *i;
	int id;
	switch (type)
	{
		// Redo atom creation
		case (UE_ADDATOM):
			// Insert a new atom at the position before the stored atom id
			id = newdata.get_id();
			msg(DM_VERBOSE,"Redoing atom creation - atom id = %i\n",id);
			if (id == 0) i = m->add_copy(NULL, &newdata);
			else i = m->add_copy(staticatoms[id-1], &newdata);
			m->project_atom(i);
			break;
		// Redo atom deletion
		case (UE_DELETEATOM):
			// We delete the atom at the position referenced by the ID in the atom
			id = newdata.get_id();
			msg(DM_VERBOSE,"Redoing atom deletion - atom id = %i\n",id);
			m->delete_atom(staticatoms[id]);
			break;
		// Redo bond creation
		case (UE_ADDBOND):
			// Delete bond between stored atom ids
			msg(DM_VERBOSE,"Redoing bond creation - atom ids = %i %i\n",data[0],data[1]);
			m->bond_atoms(staticatoms[data[0]],staticatoms[data[1]],(bond_type) data[2]);
			break;
		default:
			printf("Don't know how to undo change (type = %i)\n",type);
			break;
	}
	dbg_end(DM_CALLS,"change::perform");
}

// Revert (undo) changes detailed in state
void undostate::revert(model *m)
{
	dbg_begin(DM_CALLS,"undostate::revert");
	// Undo the changes stored in the change list
	for (change* c = changes.last(); c != NULL; c = c->prev) c->revert(m);
	dbg_end(DM_CALLS,"undostate::revert");
}

// Perform (redo) changes detailed in state
void undostate::perform(model *m)
{
	dbg_begin(DM_CALLS,"undostate::perform");
	for (change* c = changes.first(); c != NULL; c = c->next) c->perform(m);
	dbg_end(DM_CALLS,"undostate::perform");
}
