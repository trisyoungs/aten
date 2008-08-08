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
	realData_[0] = 0.0;
	realData_[1] = 0.0;
	type_ = Change::NoEvent;
	direction_ = Change::Undo;

	// Public variables
	prev = NULL;
	next = NULL;
}

UndoState::UndoState()
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
	msg.enter("Change::set[Atom]");
	direction_ = (ue > 0 ? Change::Undo : Change::Redo);
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
	msg.exit("Change::set[atom]");
}

// Set change (general)
void Change::set(int ue, int i, int j, int k, int l, int m)
{
	msg.enter("Change::set[int]");
	direction_ = (ue > 0 ? Change::Undo : Change::Redo);
	type_ = (UndoEvent) abs(ue);
	data_[0] = i;
	data_[1] = j;
	data_[2] = k;
	data_[3] = l;
	data_[4] = m;
	msg.exit("Change::set[int]");
}

// Set change (double)
void Change::set(int ue, double a, double b)
{
	msg.enter("Change::set[double]");
	direction_ = (ue > 0 ? Change::Undo : Change::Redo);
	type_ = (UndoEvent) abs(ue);
	realData_[0] = a;
	realData_[0] = b;
	msg.exit("Change::set[double]");
}

// Set change (by passed variable types)
void Change::set(int ue, Vec3<double> *v1, Vec3<double> *v2, Vec3<double> *v3, Vec3<double> *v4)
{
	msg.enter("Change::set[vector]");
	direction_ = (ue > 0 ? Change::Undo : Change::Redo);
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
	msg.exit("Change::set[vector]");
}

// Undo stored change
void Change::undo(Model *m)
{
	msg.enter("Change::undo");
	Atom **modelatoms = m->atomArray();
	int id;
	Atom *i, *j, *k, *l;
	Bond *b;
	switch (type_)
	{
		// Atom creation (Change::Redo) and deletion (Change::Undo)
		case (Change::AtomEvent):
			if (direction_ == Change::Undo)
			{
				// We delete the atom at the position referenced by the ID in the atom
				id = atomData_[0]->id();
				msg.print(Messenger::Verbose,"Reversing atom creation - atom id = %i\n",id);
				m->deleteAtom(modelatoms[id]);
			}
			else
			{
				// Insert a new atom at the position before the stored atom id
				id = atomData_[0]->id();
				msg.print(Messenger::Verbose,"Replaying atom deletion - atom id = %i\n",id);
				if (id == 0) m->addCopy(NULL, atomData_[0]);
				else m->addCopy(modelatoms[id-1], atomData_[0]);
			}
			break;
		// Bond creation (Change::Redo) and deletion (Change::Undo)
		case (Change::BondEvent):
			i = modelatoms[data_[0]];
			j = modelatoms[data_[1]];
			if (direction_ == Change::Undo)
			{
				// Delete bond between stored atom ids
				msg.print(Messenger::Verbose,"Reversing bond creation - atom ids = %i %i\n", data_[0], data_[1]);
				m->unbondAtoms(i,j);
			}
			else
			{
				// Add bond between stored atom ids
				msg.print(Messenger::Verbose,"Replaying bond deletion - atom ids = %i %i\n", data_[0], data_[1]);
				m->bondAtoms(i,j,(Bond::BondType) data_[2]);
			}
			break;
		// Atom selection (Change::Redo) and deselection (Change::Undo)
		case (Change::SelectEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom selection - atom id = %i\n", data_[0]);
				m->deselectAtom(i);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom selection - atom id = %i\n", data_[0]);
				m->selectAtom(i);
			}
			break;
		// Bond order change - from data[3] to data[2] (Change::Undo) or vice versa (Change::Redo)
		case (Change::BondOrderEvent):
			i = modelatoms[data_[0]];
			j = modelatoms[data_[1]];
			b = i->findBond(j);
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[3], data_[2]);
				m->changeBond(b,(Bond::BondType) data_[2]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[2], data_[3]);
				m->changeBond(b,(Bond::BondType) data_[3]);
			}
			break;
		// Geometry measurement creation (Change::Undo) and deletion (Change::Redo)
		case (Change::MeasurementEvent):
			i = modelatoms[data_[1]];
			j = modelatoms[data_[2]];
			k = modelatoms[data_[3]];
			l = modelatoms[data_[4]];
			if (direction_ == Change::Undo)
			{
				Measurement::MeasurementType mt = (Measurement::MeasurementType) data_[0];
				msg.print(Messenger::Verbose,"Reversing measurement - type = %i\n", mt);
				Measurement *me = m->findMeasurement(mt, i, j, k, l);
				if (me != NULL) m->removeMeasurement(me);
			}
			else
			{
				Measurement::MeasurementType mt = (Measurement::MeasurementType) data_[0];
				msg.print(Messenger::Verbose,"Replaying measurement - type = %i\n", mt);
				m->addMeasurement(mt, i, j, k, l);
			}
			break;
		// Atom transmute - from data[2] to data[1] (Change::Undo) or vice versa (Change::Redo)
		case (Change::TransmuteEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[2], data_[1]);
				m->transmuteAtom(i,data_[1]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[1], data_[2]);
				m->transmuteAtom(i,data_[2]);
			}
			break;
		// Cell change - from matrix[1] to matrix[0] (Change::Undo) or vice versa (Change::Redo)
		case (Change::CellEvent):
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing cell change\n");
				if (vecData_[0] == NULL) m->removeCell();
				else m->setCell(*vecData_[0], *vecData_[1]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying cell change\n");
				if (vecData_[2] == NULL) m->removeCell();
				else m->setCell(*vecData_[2], *vecData_[3]);
			}
			break;
		// Atom label change - from data[2] to data[1] (Change::Undo) or vice versa (Change::Redo)
		case (Change::LabelEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom label change - atom %i, from %i to %i\n",data_[0],data_[2],data_[1]);
				i->setLabels(data_[1]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom label change - atom %i, from %i to %i\n",data_[0],data_[1],data_[2]);
				i->setLabels(data_[2]);
			}
			break;
		// Atom position change - add (Change::Undo) or subtract (Change::Redo) vecData_[0]
		case (Change::TranslateEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom translation - atom %i, subtracting %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
				i->r() -= *vecData_[0];
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom translation - atom %i, adding %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
				i->r() += *vecData_[0];
			}
			break;
		// Atom list position change - -data[1] (Change::Undo) or +data[1] places in list (Change::Redo)
		case (Change::ShiftEvent):
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom shift - atom %i moves %i places\n", data_[0]+data_[1], -data_[1]);
				m->atoms_.move(data_[0]+data_[1], -data_[1]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Performing atom shift - atom %i moves %i places\n", data_[0], data_[1]);
				m->atoms_.move(data_[0], data_[1]);
			}
			m->renumberAtoms();
			break;
		// Atom charge change - from realData[1] to realData[0] (Change::Undo) or vice versa (Change::Redo)
		case (Change::ChargeEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom charge change - atom %i, from %i to %i\n",data_[0],realData_[1],data_[0]);
				i->setCharge(realData_[0]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom charge change - atom %i, from %i to %i\n",data_[0],realData_[0],data_[1]);
				i->setCharge(realData_[1]);
			}
			break;
		// Glyph creation (Change::Redo) or deletion (Change::Undo)
		case (Change::GlyphEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo)
			{
				msg.print(Messenger::Verbose,"Reversing atom charge change - atom %i, from %i to %i\n",data_[0],realData_[1],data_[0]);
				i->setCharge(realData_[0]);
			}
			else
			{
				msg.print(Messenger::Verbose,"Replaying atom charge change - atom %i, from %i to %i\n",data_[0],realData_[0],data_[1]);
				i->setCharge(realData_[1]);
			}
			break;
		default:
			printf("Don't know how to reverse change (type = %i)\n", type_);
			break;
	}
	msg.exit("Change::undo");
}

// Redo stored change
void Change::redo(Model *m)
{
	msg.enter("Change::redo");
	// Re-use the commands in Change::undo, performing the change in the opposite direction
	direction_ = (direction_ == Change::Undo ? Change::Redo : Change::Undo);
	// Now just call reverse instead, and then set the old direction back at the end
	undo(m);
	direction_ = (direction_ == Change::Undo ? Change::Redo : Change::Undo);
	msg.exit("Change::redo");
}

// Print change info
void Change::print(Model *m)
{
	msg.enter("Change::print");
	Atom **modelatoms = m->atomArray();
	Atom *i, *j;
	switch (type_)
	{
		// Atom creation 
		case (Change::AtomEvent):
			if (direction_ == Change::Undo) printf("       Atom creation - atom id = %i\n",atomData_[0]->id());
			else printf("       Atom deletion - atom id = %i\n",atomData_[0]->id());
			break;
		// Bond creation
		case (Change::BondEvent):
			if (direction_ == Change::Undo)	printf("       Bond creation - atom ids = %i %i\n", data_[0], data_[1]);
			else printf("       Bond deletion - atom ids = %i %i\n", data_[0], data_[1]);
			break;
		// Atom selection
		case (Change::SelectEvent):
			if (direction_ == Change::Undo) printf("       Atom selection - atom id = %i\n", data_[0]);
			else printf("       Atom deselection - atom id = %i\n", data_[0]);
			break;
		// Bond order change
		case (Change::BondOrderEvent):
			i = modelatoms[data_[0]];
			j = modelatoms[data_[1]];
			if (direction_ == Change::Undo) printf("       Bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[3], data_[2]);
			else printf("       Bond order change - atoms %i-%i, old = %i, new = %i\n", i->id(), j->id(), data_[2], data_[3]);
			break;
		// Geometry measurement creation (Change::Undo) and deletion (Change::Redo)
		case (Change::MeasurementEvent):
			if (direction_ == Change::Undo) printf("       Measurement deletion, type = %i\n", data_[0]);
			else printf("       Measurement creation - type = %i\n", data_[0]);
			break;
		// Atom transmute - from data[2] to data[1] (Change::Undo) or vice versa (Change::Redo)
		case (Change::TransmuteEvent):
			i = modelatoms[data_[0]];
			if (direction_ == Change::Undo) printf("       Atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[2], data_[1]);
			else printf("       Atom transmute - atom %i, old = %i, new = %i\n", i->id(), data_[1], data_[2]);
			break;
		// Cell change - from matrix[1] to matrix[0] (Change::Undo) or vice versa (Change::Redo)
		case (Change::CellEvent):
			if (direction_ == Change::Undo) printf("       Cell change\n");
			else printf("       Cell change\n");
			break;
		// Atom label change - from data[2] to data[1] (Change::Undo) or vice versa (Change::Redo)
		case (Change::LabelEvent):
			if (direction_ == Change::Undo) printf("       Atom label change - atom %i, from %i to %i\n",data_[0],data_[2],data_[1]);
			else printf("       Atom label change - atom %i, from %i to %i\n",data_[0],data_[1],data_[2]);
			break;
		// Atom position change - add (Change::Undo) or subtract (Change::Redo) vecData_[0]
		case (Change::TranslateEvent):
			if (direction_ == Change::Undo) printf("       Atom translation - atom %i, subtracting %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
			else printf("       Atom translation - atom %i, adding %f %f %f\n", data_[0], vecData_[0]->x, vecData_[0]->y, vecData_[0]->z);
			break;
		// Atom list position change - -data[1] (Change::Undo) or +data[1] places in list (Change::Redo)
		case (Change::ShiftEvent):
			if (direction_ == Change::Undo) printf("       Atom shift - atom %i moves %i places\n", data_[0]+data_[1], -data_[1]);
			else printf("       Atom shift - atom %i moves %i places\n", data_[0], data_[1]);
			break;
		// Atom charge change - from realData[1] to realData[0] (Change::Undo) or vice versa (Change::Redo)
		case (Change::ChargeEvent):
			if (direction_ == Change::Undo) printf("       Atom charge change - atom %i, from %i to %i\n",data_[0],realData_[1],data_[0]);
			else printf("       Atom charge change - atom %i, from %i to %i\n",data_[0],realData_[0],data_[1]);
			break;
		// Glyph creation (Change::Redo) or deletion (Change::Undo)
		case (Change::GlyphEvent):
			break;
		default:
			printf("       UNKNOWN CHANGE (type = %i)\n", type_);
			break;
	}
	msg.exit("Change::print");
}

/*
// Undostate
*/

// Set log point at start of state
void UndoState::setStartLog(Change::ChangeLog log, int value)
{
	startLogs_[log] = value;
}

// Get structure log point at start of state
int UndoState::startLog(Change::ChangeLog log)
{
	return startLogs_[log];
}

// Set log point at end of state
void UndoState::setEndLog(Change::ChangeLog log, int value)
{
	endLogs_[log] = value;
}

// Get structure log point at end of state
int UndoState::endLog(Change::ChangeLog log)
{
	return endLogs_[log];
}

// Set the text associated with the current undo state
void UndoState::setDescription(const char *s)
{
	description_ = s;
}

// Return the current text associated with the state
const char *UndoState::description()
{
	return description_.get();
}

// Add change to undostate
Change *UndoState::addChange()
{
	return changes_.add();
}

// Return number of changes in list
int UndoState::nChanges()
{
	return changes_.nItems();
}

// Undo changes detailed in state
void UndoState::undo(Model *m)
{
	msg.enter("UndoState::Undo");
	// Undo the changes stored in the change list
	for (Change* c = changes_.last(); c != NULL; c = c->prev) c->undo(m);
	// Set model logs to the old values
	m->copyLogs(startLogs_);
	msg.exit("UndoState::Undo");
}

// Redo changes detailed in state
void UndoState::redo(Model *m)
{
	msg.enter("UndoState::redo");
	for (Change* c = changes_.first(); c != NULL; c = c->next) c->redo(m);
	// Set model logs to the new values
	m->copyLogs(endLogs_);
	msg.exit("UndoState::redo");
}

// Check differences between logs for start/end points
bool UndoState::doLogsDiffer()
{
	if (startLogs_[Change::StructureLog] != endLogs_[Change::StructureLog]) return TRUE;
	if (startLogs_[Change::CoordinateLog] != endLogs_[Change::CoordinateLog]) return TRUE;
	if (startLogs_[Change::SelectionLog] != endLogs_[Change::SelectionLog]) return TRUE;
	return FALSE;
}

// Print changes in state
void UndoState::print(Model *m)
{
	for (Change *c = changes_.first(); c != NULL; c = c->next) c->print(m);
}
