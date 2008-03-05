/*
	*** Undo state storage
	*** src/classes/undostate.cpp
	Copyright T. Youngs 2007,2008

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
	vecdata[0] = NULL;
	vecdata[1] = NULL;
	vecdata[2] = NULL;
	vecdata[3] = NULL;
	type = UE_NONE;
	direction = UD_REVERSE;
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
	if (vecdata[0] != NULL) delete vecdata[0];
	if (vecdata[1] != NULL) delete vecdata[1];
	if (vecdata[2] != NULL) delete vecdata[2];
	if (vecdata[3] != NULL) delete vecdata[3];
}

undostate::~undostate()
{
}

// Set change (by passed variable types)
void change::set(int ue, atom *i, atom *j)
{
	dbg_begin(DM_CALLS,"change::set[atom]");
	direction = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type = (undo_event) abs(ue);
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
		atomdata[1]->copy(j);
		atomdata[1]->set_id(j->get_id());
	}
	dbg_end(DM_CALLS,"change::set[atom]");
}

// Set change (general)
void change::set(int ue, int i, int j, int k, int l, int m)
{
	dbg_begin(DM_CALLS,"change::set[int]");
	direction = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type = (undo_event) abs(ue);
	data[0] = i;
	data[1] = j;
	data[2] = k;
	data[3] = l;
	data[4] = m;
	dbg_end(DM_CALLS,"change::set[int]");
}

// Set change (by passed variable types)
void change::set(int ue, vec3<double> *v1, vec3<double> *v2, vec3<double> *v3, vec3<double> *v4)
{
	dbg_begin(DM_CALLS,"change::set[vector]");
	direction = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type = (undo_event) abs(ue);
	// Copy data from source vectors, unless they are NULL
	if (v1 != NULL)
	{
		vecdata[0] = new vec3<double>;
		*vecdata[0] = *v1;
	}
	if (v2 != NULL)
	{
		vecdata[1] = new vec3<double>;
		*vecdata[1] = *v2;
	}
	if (v3 != NULL)
	{
		vecdata[2] = new vec3<double>;
		*vecdata[2] = *v3;
	}
	if (v4 != NULL)
	{
		vecdata[3] = new vec3<double>;
		*vecdata[3] = *v4;
	}
	dbg_end(DM_CALLS,"change::set[vector]");
}

// Reverse (undo) stored change
void change::reverse(model *m)
{
	dbg_begin(DM_CALLS,"change::reverse");
	atom **modelatoms = m->get_atomarray();
	int id;
	atom *i, *j, *k, *l;
	bond *b;
	switch (type)
	{
		// Atom creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_ATOM):
			if (direction == UD_REVERSE)
			{
				// We delete the atom at the position referenced by the ID in the atom
				id = atomdata[0]->get_id();
				msg(DM_VERBOSE,"Reversing atom creation - atom id = %i\n",id);
				m->delete_atom(modelatoms[id]);
			}
			else
			{
				// Insert a new atom at the position before the stored atom id
				id = atomdata[0]->get_id();
				msg(DM_VERBOSE,"Replaying atom deletion - atom id = %i\n",id);
				if (id == 0) m->add_copy(NULL, atomdata[0]);
				else m->add_copy(modelatoms[id-1], atomdata[0]);
			}
			break;
		// Bond creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_BOND):
			i = modelatoms[data[0]];
			j = modelatoms[data[1]];
			if (direction == UD_REVERSE)
			{
				// Delete bond between stored atom ids
				msg(DM_VERBOSE,"Reversing bond creation - atom ids = %i %i\n", data[0], data[1]);
				m->unbond_atoms(i,j);
			}
			else
			{
				// Add bond between stored atom ids
				msg(DM_VERBOSE,"Replaying bond deletion - atom ids = %i %i\n", data[0], data[1]);
				m->bond_atoms(i,j,(bond_type) data[2]);
			}
			break;
		// Atom selection (UD_REVERSE) and deselection (UD_FORWARDS)
		case (UE_SELECT):
			i = modelatoms[data[0]];
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom selection - atom id = %i\n", data[0]);
				m->deselect_atom(i);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom deselection - atom id = %i\n", data[0]);
				m->select_atom(i);
			}
			break;
		// Bond order change - from data[3] to data[2] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_BONDORDER):
			i = modelatoms[data[0]];
			j = modelatoms[data[1]];
			b = i->find_bond(j);
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing bond order change - atoms %i-%i, old = %i, new = %i\n", i->get_id(), j->get_id(), data[3], data[2]);
				m->change_bond(b,(bond_type) data[2]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying bond order change - atoms %i-%i, old = %i, new = %i\n", i->get_id(), j->get_id(), data[2], data[3]);
				m->change_bond(b,(bond_type) data[3]);
			}
			break;
		// Geometry measurement creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_MEASUREMENT):
			i = modelatoms[data[1]];
			j = modelatoms[data[2]];
			k = modelatoms[data[3]];
			l = modelatoms[data[4]];
			if (direction == UD_REVERSE)
			{
				geom_type gt = (geom_type) data[0];
				msg(DM_VERBOSE,"Reversing measurement - type = %i\n", gt);
				measurement *me = m->find_measurement(gt, i, j, k, l);
				if (me != NULL) m->remove_measurement(me);
			}
			else
			{
				geom_type gt = (geom_type) data[0];
				msg(DM_VERBOSE,"Replaying measurement - type = %i\n", gt);
				m->add_measurement(gt, i, j, k, l);
			}
			break;
		// Atom transmute - from data[2] to data[1] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_TRANSMUTE):
			i = modelatoms[data[0]];
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom transmute - atom %i, old = %i, new = %i\n", i->get_id(), data[2], data[1]);
				m->transmute_atom(i,data[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom transmute - atom %i, old = %i, new = %i\n", i->get_id(), data[1], data[2]);
				m->transmute_atom(i,data[2]);
			}
			break;
		// Cell change - from matrix[1] to matrix[0] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_CELL):
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing cell change\n");
				if (vecdata[0] == NULL) m->remove_cell();
				else m->set_cell(*vecdata[0], *vecdata[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying cell change\n");
				if (vecdata[2] == NULL) m->remove_cell();
				else m->set_cell(*vecdata[2], *vecdata[3]);
			}
			break;
		// Atom label change - from data[2] to data[1] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_LABEL):
			i = modelatoms[data[0]];
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom label change - atom %i, from %i to %i\n",data[0],data[2],data[1]);
				i->set_labels(data[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom label change - atom %i, from %i to %i\n",data[0],data[1],data[2]);
				i->set_labels(data[2]);
			}
			break;
		// Atom position change - add (UD_REVERSE) or subtract (UD_FORWARDS) vecdata[0]
		case (UE_TRANSLATE):
			i = modelatoms[data[0]];
			if (direction == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom translation - atom %i, adding %f %f %f\n", data[0], vecdata[0]->x, vecdata[0]->y, vecdata[0]->z);
				i->r() -= *vecdata[0];
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom translation - atom %i, subtracting %f %f %f\n", data[0], vecdata[0]->x, vecdata[0]->y, vecdata[0]->z);
				i->r() += *vecdata[0];
			}
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
	// Re-use the commands in change::revert, performing the change in the opposite direction
	direction = (direction == UD_REVERSE ? UD_FORWARDS : UD_REVERSE);
	// Now just call reverse instead, and then set the old direction back at the end
	reverse(m);
	direction = (direction == UD_REVERSE ? UD_FORWARDS : UD_REVERSE);
	dbg_end(DM_CALLS,"change::perform");
}

// Revert (undo) changes detailed in state
void undostate::reverse(model *m)
{
	dbg_begin(DM_CALLS,"undostate::reverse");
	// Undo the changes stored in the change list
	for (change* c = changes.last(); c != NULL; c = c->prev) c->reverse(m);
	// Set model logs to the old values
	m->copy_logs(startlogs);
	dbg_end(DM_CALLS,"undostate::reverse");
}

// Perform (redo) changes detailed in state
void undostate::perform(model *m)
{
	dbg_begin(DM_CALLS,"undostate::perform");
	for (change* c = changes.first(); c != NULL; c = c->next) c->perform(m);
	// Set model logs to the new values
	m->copy_logs(endlogs);
	dbg_end(DM_CALLS,"undostate::perform");
}

// Check differences between LOG_STRUCTURE and LOG_COORDS for start/end points
bool undostate::logs_differ()
{
	if (startlogs[LOG_STRUCTURE] != endlogs[LOG_STRUCTURE]) return TRUE;
	if (startlogs[LOG_COORDS] != endlogs[LOG_COORDS]) return TRUE;
	if (startlogs[LOG_SELECTION] != endlogs[LOG_SELECTION]) return TRUE;
	return FALSE;
}
