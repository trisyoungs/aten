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
Change::Change()
{
	// Private variables
	atomData_[0] = NULL;
	atomData_[1] = NULL;
	vecData_[0] = NULL;
	vecData_[1] = NULL;
	vecData_[2] = NULL;
	vecData_[3] = NULL;
	type_ = UE_NONE;
	direction_ = UD_REVERSE;
	// Public variables
	prev = NULL;
	next = NULL;
}

Undostate::Undostate()
{
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructors
Change::~Change()
{
	if (atomData_[0] != NULL) delete atomData_[0];
	if (atomData_[1] != NULL) delete atomData_[1];
	if (vecData_[0] != NULL) delete vecData_[0];
	if (vecData_[1] != NULL) delete vecData_[1];
	if (vecData_[2] != NULL) delete vecData_[2];
	if (vecData_[3] != NULL) delete vecData_[3];
}

// Set change (by passed variable types)
void Change::set(int ue, Atom *i, Atom *j)
{
	dbgBegin(DM_CALLS,"Change::set[Atom]");
	direction_ = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type_ = (UndoEvent) abs(ue);
	// Copy atom data from source atoms, unless they are NULL
	if (i != NULL)
	{
		atomData_[0] = new Atom;
		atomData_[0]->copy(i);
		atomData_[0]->setId(i->id());
	}
	if (j != NULL)
	{
		atomData_[1] = new Atom;
		atomData_[1]->copy(j);
		atomData_[1]->setId(j->id());
	}
	dbgEnd(DM_CALLS,"Change::set[atom]");
}

// Set change (general)
void Change::set(int ue, int i, int j, int k, int l, int m)
{
	dbgBegin(DM_CALLS,"Change::set[int]");
	direction_ = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type_ = (UndoEvent) abs(ue);
	data_[0] = i;
	data_[1] = j;
	data_[2] = k;
	data_[3] = l;
	data_[4] = m;
	dbgEnd(DM_CALLS,"Change::set[int]");
}

// Set change (by passed variable types)
void Change::set(int ue, Vec3<double> *v1, Vec3<double> *v2, Vec3<double> *v3, Vec3<double> *v4)
{
	dbgBegin(DM_CALLS,"Change::set[vector]");
	direction_ = (ue > 0 ? UD_REVERSE : UD_FORWARDS);
	type_ = (UndoEvent) abs(ue);
	// Copy data from source vectors, unless they are NULL
	if (v1 != NULL)
	{
		vecData_[0] = new Vec3<double>;
		*vecData_[0] = *v1;
	}
	if (v2 != NULL)
	{
		vecData_[1] = new Vec3<double>;
		*vecData_[1] = *v2;
	}
	if (v3 != NULL)
	{
		vecData_[2] = new Vec3<double>;
		*vecData_[2] = *v3;
	}
	if (v4 != NULL)
	{
		vecData_[3] = new Vec3<double>;
		*vecData_[3] = *v4;
	}
	dbgEnd(DM_CALLS,"Change::set[vector]");
}

// Reverse (undo) stored change
void Change::reverse(Model *m)
{
	dbgBegin(DM_CALLS,"Change::reverse");
	Atom **modelatoms = m->atomArray();
	int id;
	Atom *i, *j, *k, *l;
	Bond *b;
	switch (type_)
	{
		// Atom creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_ATOM):
			if (direction_ == UD_REVERSE)
			{
				// We delete the atom at the position referenced by the ID in the atom
				id = atomData_[0]->id();
				msg(DM_VERBOSE,"Reversing atom creation - atom id = %i\n",id);
				m->deleteAtom(modelatoms[id]);
			}
			else
			{
				// Insert a new atom at the position before the stored atom id
				id = atomData_[0]->id();
				msg(DM_VERBOSE,"Replaying atom deletion - atom id = %i\n",id);
				if (id == 0) m->addCopy(NULL, atomData_[0]);
				else m->addCopy(modelatoms[id-1], atomData_[0]);
			}
			break;
		// Bond creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_BOND):
			i = modelatoms[data_[0]];
			j = modelatoms[data_[1]];
			if (direction_ == UD_REVERSE)
			{
				// Delete bond between stored atom ids
				msg(DM_VERBOSE,"Reversing bond creation - atom ids = %i %i\n", data_[0], data_[1]);
				m->unbondAtoms(i,j);
			}
			else
			{
				// Add bond between stored atom ids
				msg(DM_VERBOSE,"Replaying bond deletion - atom ids = %i %i\n", data_[0], data_[1]);
				m->bondAtoms(i,j,(Bond::BondType) data_[2]);
			}
			break;
		// Atom selection (UD_REVERSE) and deselection (UD_FORWARDS)
		case (UE_SELECT):
			i = modelatoms[data_[0]];
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom selection - atom id = %i\n", data_[0]);
				m->deselectAtom(i);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom deselection - atom id = %i\n", data_[0]);
				m->selectAtom(i);
			}
			break;
		// Bond order change - from data[3] to data[2] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_BONDORDER):
			i = modelatoms[data_[0]];
			j = modelatoms[data_[1]];
			b = i->findBond(j);
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[3], data_[2]);
				m->changeBond(b,(Bond::BondType) data_[2]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[2], data_[3]);
				m->changeBond(b,(Bond::BondType) data_[3]);
			}
			break;
		// Geometry measurement creation (UD_REVERSE) and deletion (UD_FORWARDS)
		case (UE_MEASUREMENT):
			i = modelatoms[data_[1]];
			j = modelatoms[data_[2]];
			k = modelatoms[data_[3]];
			l = modelatoms[data_[4]];
			if (direction_ == UD_REVERSE)
			{
				GeometryType gt = (GeometryType) data_[0];
				msg(DM_VERBOSE,"Reversing measurement - type = %i\n", gt);
				Measurement *me = m->findMeasurement(gt, i, j, k, l);
				if (me != NULL) m->removeMeasurement(me);
			}
			else
			{
				GeometryType gt = (GeometryType) data_[0];
				msg(DM_VERBOSE,"Replaying measurement - type = %i\n", gt);
				m->addMeasurement(gt, i, j, k, l);
			}
			break;
		// Atom transmute - from data[2] to data[1] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_TRANSMUTE):
			i = modelatoms[data_[0]];
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[2], data_[1]);
				m->transmuteAtom(i,data_[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[1], data_[2]);
				m->transmuteAtom(i,data_[2]);
			}
			break;
		// Cell change - from matrix[1] to matrix[0] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_CELL):
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing cell change\n");
				if (vecData_[0] == NULL) m->removeCell();
				else m->setCell(*vecData_[0], *vecData_[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying cell change\n");
				if (vecData_[2] == NULL) m->removeCell();
				else m->setCell(*vecData_[2], *vecData_[3]);
			}
			break;
		// Atom label change - from data[2] to data[1] (UD_REVERSE) or vice versa (UD_FORWARDS)
		case (UE_LABEL):
			i = modelatoms[data_[0]];
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom label change - atom %i, from %i to %i\n",data_[0],data_[2],data_[1]);
				i->setLabels(data_[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom label change - atom %i, from %i to %i\n",data_[0],data_[1],data_[2]);
				i->setLabels(data_[2]);
			}
			break;
		// Atom position change - add (UD_REVERSE) or subtract (UD_FORWARDS) vecData_[0]
		case (UE_TRANSLATE):
			i = modelatoms[data_[0]];
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom translation - atom %i, subtracting %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
				i->r() -= *vecData_[0];
			}
			else
			{
				msg(DM_VERBOSE,"Replaying atom translation - atom %i, adding %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
				i->r() += *vecData_[0];
			}
			break;
		// Atom list position change - -data[1] (UD_REVERSE) or +data[1] places in list (UD_FORWARDS)
		case (UE_SHIFT):
			if (direction_ == UD_REVERSE)
			{
				msg(DM_VERBOSE,"Reversing atom shift - atom %i moves %i places\n", data_[0]+data_[1], -data_[1]);
				m->atoms_.move(data_[0]+data_[1], -data_[1]);
			}
			else
			{
				msg(DM_VERBOSE,"Performing atom shift - atom %i moves %i places\n", data_[0], data_[1]);
				m->atoms_.move(data_[0], data_[1]);
			}
			m->renumberAtoms();
			break;
		default:
			printf("Don't know how to reverse change (type = %i)\n", type_);
			break;
	}
	dbgEnd(DM_CALLS,"Change::reverse");
}

// Perform (redo) stored change
void Change::perform(Model *m)
{
	dbgBegin(DM_CALLS,"Change::perform");
	// Re-use the commands in Change::revert, performing the change in the opposite direction
	direction_ = (direction_ == UD_REVERSE ? UD_FORWARDS : UD_REVERSE);
	// Now just call reverse instead, and then set the old direction back at the end
	reverse(m);
	direction_ = (direction_ == UD_REVERSE ? UD_FORWARDS : UD_REVERSE);
	dbgEnd(DM_CALLS,"Change::perform");
}

/*
// Undostate
*/

// Set log point at start of state
void Undostate::setStartLog(ChangeLog log, int value)
{
	startLogs_[log] = value;
}

// Get structure log point at start of state
int Undostate::startLog(ChangeLog log)
{
	return startLogs_[log];
}

// Set log point at end of state
void Undostate::setEndLog(ChangeLog log, int value)
{
	endLogs_[log] = value;
}

// Get structure log point at end of state
int Undostate::endLog(ChangeLog log)
{
	return endLogs_[log];
}

// Set the text associated with the current undo state
void Undostate::setDescription(const char *s)
{
	description_ = s;
}

// Return the current text associated with the state
const char *Undostate::description()
{
	return description_.get();
}

// Add change to undostate
Change *Undostate::addChange()
{
	return changes_.add();
}

// Return number of changes in list
int Undostate::nChanges()
{
	return changes_.nItems();
}

// Revert (undo) changes detailed in state
void Undostate::reverse(Model *m)
{
	dbgBegin(DM_CALLS,"Undostate::reverse");
	// Undo the changes stored in the change list
	for (Change* c = changes_.last(); c != NULL; c = c->prev) c->reverse(m);
	// Set model logs to the old values
	m->copyLogs(startLogs_);
	dbgEnd(DM_CALLS,"Undostate::reverse");
}

// Perform (redo) changes detailed in state
void Undostate::perform(Model *m)
{
	dbgBegin(DM_CALLS,"Undostate::perform");
	for (Change* c = changes_.first(); c != NULL; c = c->next) c->perform(m);
	// Set model logs to the new values
	m->copyLogs(endLogs_);
	dbgEnd(DM_CALLS,"Undostate::perform");
}

// Check differences between LOG_STRUCTURE and LOG_COORDS for start/end points
bool Undostate::doLogsDiffer()
{
	if (startLogs_[LOG_STRUCTURE] != endLogs_[LOG_STRUCTURE]) return TRUE;
	if (startLogs_[LOG_COORDS] != endLogs_[LOG_COORDS]) return TRUE;
	if (startLogs_[LOG_SELECTION] != endLogs_[LOG_SELECTION]) return TRUE;
	return FALSE;
}
