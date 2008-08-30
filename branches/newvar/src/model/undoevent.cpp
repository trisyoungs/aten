/*
	*** UndoEvents
	*** src/model/undoevent.cpp
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

#include "model/undoevent.h"
#include "model/model.h"

// Constructor
UndoEvent::UndoEvent()
{
	// Private variables
	direction_ = UndoEvent::Undo;

	// Public variables
	prev = NULL;
	next = NULL;
}

/*
// AtomEvent
*/

// Set change 
void AtomEvent::set(bool creation, Atom *i)
{
	msg.enter("AtomEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	// Copy atom data from source atoms, unless they are NULL
	if (i != NULL)
	{
		//atomData_ = new Atom;
		atomData_.copy(i);
		atomData_.setId(i->id());
	}
	else printf("Null pointer passed to AtomEvent::set()!\n");
	msg.exit("AtomEvent::set");
}

// Undo stored change
void AtomEvent::undo(Model *m)
{
	msg.enter("AtomEvent::undo");
	Atom **modelatoms = m->atomArray();
	int id;
	// Atom creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	if (direction_ == UndoEvent::Undo)
	{
		// We delete the atom at the position referenced by the ID in the atom
		id = atomData_.id();
		msg.print(Messenger::Verbose,"Reversing atom creation - atom id = %i\n", id);
		m->deleteAtom(modelatoms[id]);
	}
	else
	{
		Atom *i;
		// Insert a new atom at the position before the stored atom id
		id = atomData_.id();
		msg.print(Messenger::Verbose,"Replaying atom creation - atom id = %i\n", id);
		if (id == 0) i = m->addCopy(NULL, &atomData_);
		else i = m->addCopy(modelatoms[id-1], &atomData_);
		m->projectAtom(i);
	}
}

// Print event info
void AtomEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom creation - atom id = %i\n", atomData_.id());
	else printf("       Atom deletion - atom id = %i\n", atomData_.id());
}

/*
// Bond Event
*/

// Set change 
void BondEvent::set(bool creation, int id1, int id2, Bond::BondType bondtype)
{
	msg.enter("BondEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	targetId1_ = id1;
	targetId2_ = id2;
	bondType_ = bondtype;
	msg.exit("BondEvent::set");
}

// Undo stored change
void BondEvent::undo(Model *m)
{
	msg.enter("BondEvent::undo");
	Atom *i, *j, **modelatoms = m->atomArray();
	// Bond creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	if (direction_ == UndoEvent::Undo)
	{
		// Delete bond between stored atom ids
		msg.print(Messenger::Verbose,"Reversing bond creation - atom ids = %i %i\n", targetId1_, targetId2_);
		m->unbondAtoms(i,j);
	}
	else
	{
		// Add bond between stored atom ids
		msg.print(Messenger::Verbose,"Replaying bond deletion - atom ids = %i %i\n", targetId1_, targetId2_);
		m->bondAtoms(i,j,bondType_);
	}
	msg.exit("BondEvent::undo");
}

// Print event info
void BondEvent::print()
{
	if (direction_ == UndoEvent::Undo)	printf("       Bond creation - atom ids = %i %i\n", targetId1_, targetId2_);
	else printf("       Bond deletion - atom ids = %i %i\n", targetId1_, targetId2_);
}

/*
// BondType Event
*/

// Set change 
void BondTypeEvent::set(int id1, int id2, Bond::BondType oldbondtype, Bond::BondType newbondtype)
{
	msg.enter("BondTypeEvent::set");
	targetId1_ = id1;
	targetId2_ = id2;
	oldBondType_ = oldbondtype;
	newBondType_ = newbondtype;
	msg.exit("BondTypeEvent::set");
}

// Undo stored change
void BondTypeEvent::undo(Model *m)
{
	msg.enter("BondTypeEvent::undo");
	Atom *i, *j, **modelatoms = m->atomArray();
	// Bond order change - from newBondType_ to oldBondType_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	Bond *b = i->findBond(j);
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing bond order change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, newBondType_, oldBondType_);
		m->changeBond(b, oldBondType_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying bond order change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, oldBondType_, newBondType_);
		m->changeBond(b, newBondType_);
	}
	msg.exit("BondTypeEvent::undo");
}

// Print event info
void BondTypeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, newBondType_, oldBondType_);
	else printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, oldBondType_, newBondType_);
}

/*
// Cell Event
*/

// Set change 
void CellEvent::set(Vec3<double> oldangles, Vec3<double> oldlengths, Vec3<double> newangles, Vec3<double> newlengths, bool ohs, bool nhs)
{
	msg.enter("CellEvent::set");
	oldAngles_ = oldangles;
	oldLengths_ = oldlengths;
	newAngles_ = newangles;
	newLengths_ = newlengths;
	oldHasCell_ = ohs;
	newHasCell_ = nhs;
	msg.exit("CellEvent::set");
}

// Undo stored change
void CellEvent::undo(Model *m)
{
	msg.enter("CellEvent::undo");
	Atom *i, **modelatoms = m->atomArray();
	// Cell change - from newLengths/Angles to oldLengths/Angles (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing cell change\n");
		if (oldHasCell_) m->removeCell();
		else m->setCell(oldLengths_, oldAngles_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying cell change\n");
		if (newHasCell_) m->removeCell();
		else m->setCell(newLengths_, newAngles_);
	}
	msg.exit("CellEvent::undo");
}

// Print event info
void CellEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Cell change\n");
	else printf("       Cell change\n");
}

/*
// Charge Event
*/

// Set change 
void ChargeEvent::set(int id, double oldcharge, double newcharge)
{
	msg.enter("ChargeEvent::set");
	targetId_ = id;
	oldCharge_ = oldcharge;
	newCharge_ = newcharge;
	msg.exit("ChargeEvent::set");
}

// Undo stored change
void ChargeEvent::undo(Model *m)
{
	msg.enter("ChargeEvent::undo");
	// Atom charge change - from realData[1] to realData[0] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	Atom *i, **modelatoms = m->atomArray();
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom charge change - atom %i, from %i to %i\n", targetId_, newCharge_, oldCharge_);
		i->setCharge(oldCharge_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying atom charge change - atom %i, from %i to %i\n", targetId_, oldCharge_, newCharge_);
		i->setCharge(newCharge_);
	}
	msg.exit("ChargeEvent::undo");
}

// Print event info
void ChargeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, newCharge_, oldCharge_);
	else printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, oldCharge_, newCharge_);
}

/*
// Glyph Event
*/

// Set change 
void GlyphEvent::set(bool creation, Glyph *g)
{
	msg.enter("GlyphEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	msg.exit("GlyphEvent::set");
}

// Undo stored change
void GlyphEvent::undo(Model *m)
{
	msg.enter("GlypheEvent::undo");
	// Glyph creation (UndoEvent::Redo) or deletion (UndoEvent::Undo)
	msg.exit("GlypheEvent::undo");
}

// Print event info
void GlyphEvent::print()
{
}

/*
// IdShift Event
*/

// Set change 
void IdShiftEvent::set(int id, int delta)
{
	msg.enter("IdShiftEvent::set");
	targetId_ = id;
	delta_ = delta;
	msg.exit("IdShiftEvent::set");
}

// Undo stored change
void IdShiftEvent::undo(Model *m)
{
	msg.enter("IdShiftEvent::undo");
	// Atom list position change - -data[1] (UndoEvent::Undo) or +data[1] places in list (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom shift - atom %i moves %i places\n", targetId_+delta_, -delta_);
		m->atoms_.move(targetId_+delta_, delta_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Performing atom shift - atom %i moves %i places\n", targetId_, delta_);
		m->atoms_.move(targetId_, delta_);
	}
	m->renumberAtoms();
	msg.exit("IdShiftEvent::undo");
}

// Print event info
void IdShiftEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom shift - atom %i moves %i places\n", targetId_+delta_, -delta_);
	else printf("       Atom shift - atom %i moves %i places\n", targetId_, delta_);
}

/*
// Label Event
*/

// Set change 
void LabelEvent::set(int id, int oldlabels, int newlabels)
{
	msg.enter("LabelEvent::set");
	targetId_ = id;
	oldLabels_ = oldlabels;
	newLabels_ = newlabels;
	msg.exit("LabelEvent::set");
}

// Undo stored change
void LabelEvent::undo(Model *m)
{
	msg.enter("LabelEvent::undo");
	Atom *i, **modelatoms = m->atomArray();
	// Atom label change - from data[2] to data[1] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom label change - atom %i, from %i to %i\n", targetId_, newLabels_, oldLabels_);
		i->setLabels(oldLabels_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying atom label change - atom %i, from %i to %i\n", targetId_, oldLabels_, newLabels_);
		i->setLabels(newLabels_);
	}
	msg.exit("LabelEvent::undo");
}

// Print event info
void LabelEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Reversing atom label change - atom %i, from %i to %i\n", targetId_, newLabels_, oldLabels_);
	else printf("       Replaying atom label change - atom %i, from %i to %i\n", targetId_, oldLabels_, newLabels_);
}

/*
// Measurement Event
*/

// Set change 
void MeasurementEvent::set(bool creation, Measurement::MeasurementType mt, int id1, int id2, int id3, int id4)
{
	msg.enter("MeasurementEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	type_ = mt;
	targetId_[0] = id1;
	targetId_[1] = id2;
	targetId_[2] = id3;
	targetId_[3] = id4;
	msg.exit("MeasurementEvent::set");
}

// Undo stored change
void MeasurementEvent::undo(Model *m)
{
	msg.enter("MeasurementEvent::undo");
	Atom *i, *j, *k,*l, **modelatoms = m->atomArray();
	// Measurement creation (UndoEvent::Undo) and deletion (UndoEvent::Redo)
	i = modelatoms[targetId_[0]];
	j = modelatoms[targetId_[1]];
	k = modelatoms[targetId_[2]];
	l = modelatoms[targetId_[3]];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing measurement - type = %i\n", type_);
		Measurement *me = m->findMeasurement(type_, i, j, k, l);
		if (me != NULL) m->removeMeasurement(me);
		else printf("Couldn't find measurement in UndoEvent.\n");
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying measurement - type = %i\n", type_);
		m->addMeasurement(type_, i, j, k, l);
	}
	msg.exit("MeasurementEvent::undo");
}

// Print event info
void MeasurementEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Measurement deletion, type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
	else printf("       Measurement creation - type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
}

/*
// Model Rename Event
*/

// Set change 
void ModelRenameEvent::set(const char *oldname, const char *newname)
{
	msg.enter("ModelRenameEvent::set");
	oldName_ = oldname;
	newName_ = newname;
	msg.exit("ModelRenameEvent::set");
}

// Undo stored change
void ModelRenameEvent::undo(Model *m)
{
	msg.enter("ModelRenameEvent::undo");
	// Model Rename, to oldName_ (UndoEvent::Undo) or newName_ (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing model rename - to %i\n", oldName_.get());
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying model rename - to %i\n", newName_.get());
	}
	msg.exit("ModelRenameEvent::undo");
}

// Print event info
void ModelRenameEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Model rename from %s to %s\n", newName_.get(), oldName_.get());
	else printf("       Model rename from %s to %s\n", oldName_.get(), newName_.get());
}

/*
// Selection Event
*/

// Set change 
void SelectEvent::set(bool select, int id)
{
	msg.enter("SelectionEvent::set");
	direction_ = (select ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	msg.exit("SelectionEvent::set");
}

// Undo stored change
void SelectEvent::undo(Model *m)
{
	msg.enter("SelectEvent::undo");
	Atom *i, **modelatoms = m->atomArray();
	// Atom selection (UndoEvent::Redo) and deselection (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom selection - atom id = %i\n", targetId_);
		m->deselectAtom(i);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying atom selection - atom id = %i\n", targetId_);
		m->selectAtom(i);
	}
	msg.exit("SelectEvent::undo");
}

// Print event info
void SelectEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom selection - atom id = %i\n", targetId_);
	else printf("       Atom deselection - atom id = %i\n", targetId_);
}

/*
// Translate Event
*/

// Set change 
void TranslateEvent::set(int id, Vec3<double> delta)
{
	msg.enter("TranslateEvent::set");
	targetId_ = id;
	delta_ = delta;
	msg.exit("TranslateEvent::set");
}

// Undo stored change
void TranslateEvent::undo(Model *m)
{
	msg.enter("TranslateEvent::undo");
	Atom *i, **modelatoms = m->atomArray();
	// Atom position change - add (UndoEvent::Undo) or subtract (UndoEvent::Redo) delta_.
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom translation - atom %i, subtracting %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() -= delta_;
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying atom translation - atom %i, adding %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() += delta_;
	}
	msg.exit("TranslateEvent::undo");
}

// Print event info
void TranslateEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom translation - atom %i, subtracting %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
	else printf("       Atom translation - atom %i, adding %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
}

/*
// Transmute Event
*/

// Set change 
void TransmuteEvent::set(int id, int oldel, int newel)
{
	msg.enter("TransmuteEvent::set");
	targetId_ = id;
	oldEl_ = oldel;
	newEl_ = newel;
	msg.exit("TransmuteEvent::set");
}

// Undo stored change
void TransmuteEvent::undo(Model *m)
{
	msg.enter("TransmuteEvent::undo");
	Atom *i, **modelatoms = m->atomArray();
	// Atom transmute - newEl_ to oldEl_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		msg.print(Messenger::Verbose,"Reversing atom transmute - atom %i, old = %i, new = %i\n", targetId_, newEl_, oldEl_);
		m->transmuteAtom(i, oldEl_);
	}
	else
	{
		msg.print(Messenger::Verbose,"Replaying atom transmute - atom %i, old = %i, new = %i\n", targetId_, oldEl_, newEl_);
		m->transmuteAtom(i, newEl_);
	}
	msg.exit("TransmuteEvent::undo");
}

// Print event info
void TransmuteEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, newEl_, oldEl_);
	else printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, oldEl_, newEl_);
}

/*
// Base UndoEvent
*/

// Redo stored change
void UndoEvent::redo(Model *m)
{
	msg.enter("UndoEvent::redo");
	// Re-use the commands in Change::undo, performing the change in the opposite direction
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);
	// Now just call reverse instead, and then set the old direction back at the end
	undo(m);
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);
	msg.exit("UndoeEvent::redo");
}
