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
	atomdata[0] = NULL;
	atomdata[1] = NULL;
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
	if (atomdata[0] != NULL) delete atomdata[0];
	if (atomdata[1] != NULL) delete atomdata[1];
}

undostate::~undostate()
{
}

// Set change (by passed variable types)
void change::set(undo_event ue, atom *i, atom *j)
{
	dbg_begin(DM_CALLS,"change::set[atom,atom]");
	type = ue;
	// Copy atom data from source atoms, unless they are NULL
	if (i != NULL)
	{
		atomdata[0] = new atom;
		atomdata[0]->copy(i);
		atomdata[0]->set_id(i->get_id());
	}
	if (j != NULL)
	{
		atomdata[1] = new atom;
		atomdata[0]->copy(j);
		atomdata[0]->set_id(j->get_id());
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

// Reverse (undo) stored change
void change::reverse(model *m)
{
	dbg_begin(DM_CALLS,"change::reverse");
	atom **staticatoms = m->get_staticatoms();
	int id;
	switch (type)
	{
		// Undo atom creation
		case (UE_ADDATOM):
			// We delete the atom at the position referenced by the ID in the atom
			id = atomdata[0]->get_id();
			msg(DM_VERBOSE,"Reversing atom creation - atom id = %i\n",id);
			m->delete_atom(staticatoms[id]);
			break;
		// Undo atom deletion
		case (UE_DELETEATOM):
			// Insert a new atom at the position before the stored atom id
			id = atomdata[0]->get_id();
			msg(DM_VERBOSE,"Reversing atom deletion - atom id = %i\n",id);
			if (id == 0) m->add_copy(NULL, atomdata[0]);
			else m->add_copy(staticatoms[id-1], atomdata[0]);
			break;
		// Undo bond creation
		case (UE_ADDBOND):
			// Delete bond between stored atom ids
			msg(DM_VERBOSE,"Reversing bond creation - atom ids = %i %i\n",data[0],data[1]);
			m->unbond_atoms(staticatoms[data[0]],staticatoms[data[1]]);
			break;
		// Undo bond deletion
		case (UE_DELETEBOND):
			// Add bond between stored atom ids
			msg(DM_VERBOSE,"Reversing bond deletion - atom ids = %i %i\n", data[0], data[1]);
			m->bond_atoms(staticatoms[data[0]],staticatoms[data[1]],(bond_type) data[2]);
			break;
		// Undo atom selection
		case (UE_SELECTATOM):
			msg(DM_VERBOSE,"Reversing atom selection - atom id = %i\n", data[0]);
			m->deselect_atom(staticatoms[data[0]]);
			break;
		// Undo atom deselection
		case (UE_DESELECTATOM):
			msg(DM_VERBOSE,"Reversing atom deselection - atom id = %i\n", data[0]);
			m->select_atom(staticatoms[data[0]]);
			break;
		default:
			printf("Don't know how to reverse change (type = %i)\n", type);
			break;
	}
	dbg_end(DM_CALLS,"change::reverse");
}

// Perform (redo) stored change
void change::perform(model *m)
{
	dbg_begin(DM_CALLS,"change::perform");
	// Re-use the commands in change::revert, performing the opposite of the stored change.
	// Store the old event and convert it into its opposite
	undo_event oldevent = type;
	type = (undo_event) (type > UE_INVERTER ? type - (UE_INVERTER + 1) : type + (UE_INVERTER + 1));
	// Now just call reverse instead, and then set the old type back at the end
	reverse(m);
	type = oldevent;
	dbg_end(DM_CALLS,"change::perform");
}

// Revert (undo) changes detailed in state
void undostate::reverse(model *m)
{
	dbg_begin(DM_CALLS,"undostate::reverse");
	// Undo the changes stored in the change list
	for (change* c = changes.last(); c != NULL; c = c->prev) c->reverse(m);
	dbg_end(DM_CALLS,"undostate::reverse");
}

// Perform (redo) changes detailed in state
void undostate::perform(model *m)
{
	dbg_begin(DM_CALLS,"undostate::perform");
	for (change* c = changes.first(); c != NULL; c = c->next) c->perform(m);
	dbg_end(DM_CALLS,"undostate::perform");
}
