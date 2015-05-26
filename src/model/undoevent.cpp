/*
	*** UndoEvents
	*** src/model/undoevent.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/prefs.h"

ATEN_USING_NAMESPACE

// Constructor
UndoEvent::UndoEvent() : ListItem<UndoEvent>()
{
	// Private variables
	direction_ = UndoEvent::Undo;
}

// Destructor
UndoEvent::~UndoEvent()
{
}

/*
 * AtomEvent
 */

// Constructor
AtomEvent::AtomEvent() : UndoEvent()
{
}

// Destructor
AtomEvent::~AtomEvent()
{
}

// Set change 
void AtomEvent::set(bool creation, Atom* i)
{
	Messenger::enter("AtomEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	// Copy atom data from source atoms, unless they are NULL
	if (i != NULL)
	{
		//atomData_ = new Atom;
		atomData_.copy(i);
		atomData_.setId(i->id());
	}
	else printf("Null pointer passed to AtomEvent::set()!\n");
	Messenger::exit("AtomEvent::set");
}

// Undo stored change
void AtomEvent::undo(Model* m)
{
	Messenger::enter("AtomEvent::undo");
	Atom** modelatoms = m->atomArray();
	int id;
	// Atom creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	if (direction_ == UndoEvent::Undo)
	{
		// We delete the atom at the position referenced by the ID in the atom
		id = atomData_.id();
		Messenger::print(Messenger::Verbose, "Reversing atom creation - atom id = %i", id);
		m->deleteAtom(modelatoms[id]);
	}
	else
	{
		Atom* i;
		// Insert a new atom at the position before the stored atom id
		id = atomData_.id();
		Messenger::print(Messenger::Verbose, "Replaying atom creation - atom id = %i", id);
		if (id == 0) i = m->addCopy(NULL, &atomData_);
		else i = m->addCopy(modelatoms[id-1], &atomData_);
	}
}

// Print event info
void AtomEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom creation - atom id = %i\n", atomData_.id());
	else printf("       Atom deletion - atom id = %i\n", atomData_.id());
}

/*
 * Bond Event
 */

// Constructor
BondEvent::BondEvent() : UndoEvent()
{
}

// Destructor
BondEvent::~BondEvent()
{
}

// Set change 
void BondEvent::set(bool creation, int id1, int id2, Bond::BondType bondtype)
{
	Messenger::enter("BondEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	targetId1_ = id1;
	targetId2_ = id2;
	bondType_ = bondtype;
	Messenger::exit("BondEvent::set");
}

// Undo stored change
void BondEvent::undo(Model* m)
{
	Messenger::enter("BondEvent::undo");
	Atom* i, *j, **modelatoms = m->atomArray();
	// Bond creation (UndoEvent::Redo) and deletion (UndoEvent::Undo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	if (direction_ == UndoEvent::Undo)
	{
		// Delete bond between stored atom ids
		Messenger::print(Messenger::Verbose, "Reversing bond creation - atom ids = %i %i", targetId1_, targetId2_);
		m->unbondAtoms(i,j);
	}
	else
	{
		// Add bond between stored atom ids
		Messenger::print(Messenger::Verbose, "Reversing bond deletion - atom ids = %i %i", targetId1_, targetId2_);
		m->bondAtoms(i,j,bondType_);
	}
	Messenger::exit("BondEvent::undo");
}

// Print event info
void BondEvent::print()
{
	if (direction_ == UndoEvent::Undo)	printf("       Bond creation - atom ids = %i %i\n", targetId1_, targetId2_);
	else printf("       Bond deletion - atom ids = %i %i\n", targetId1_, targetId2_);
}

/*
 * BondType Event
 */

// Constructor
BondTypeEvent::BondTypeEvent() : UndoEvent()
{
}

// Destructor
BondTypeEvent::~BondTypeEvent()
{
}

// Set change 
void BondTypeEvent::set(int id1, int id2, Bond::BondType oldbondtype, Bond::BondType newbondtype)
{
	Messenger::enter("BondTypeEvent::set");
	targetId1_ = id1;
	targetId2_ = id2;
	oldBondType_ = oldbondtype;
	newBondType_ = newbondtype;
	Messenger::exit("BondTypeEvent::set");
}

// Undo stored change
void BondTypeEvent::undo(Model* m)
{
	Messenger::enter("BondTypeEvent::undo");
	Atom* i, *j, **modelatoms = m->atomArray();
	// Bond order change - from newBondType_ to oldBondType_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId1_];
	j = modelatoms[targetId2_];
	Bond* b = i->findBond(j);
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing bond order change - atoms %i-%i, old = %i, new = %i", targetId1_, targetId2_, newBondType_, oldBondType_);
		m->changeBond(b, oldBondType_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying bond order change - atoms %i-%i, old = %i, new = %i", targetId1_, targetId2_, oldBondType_, newBondType_);
		m->changeBond(b, newBondType_);
	}
	Messenger::exit("BondTypeEvent::undo");
}

// Print event info
void BondTypeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, newBondType_, oldBondType_);
	else printf("       Bond type change - atoms %i-%i, old = %i, new = %i\n", targetId1_, targetId2_, oldBondType_, newBondType_);
}

/*
 * Cell Event
 */

// Constructor
CellEvent::CellEvent() : UndoEvent()
{
}

// Destructor
CellEvent::~CellEvent()
{
}

// Set change 
void CellEvent::set(Matrix oldaxes, Matrix newaxes, bool ohs, bool nhs)
{
	Messenger::enter("CellEvent::set");
	oldAxes_ = oldaxes;
	newAxes_ = newaxes;
	oldHasCell_ = ohs;
	newHasCell_ = nhs;
	Messenger::exit("CellEvent::set");
}

// Undo stored change
void CellEvent::undo(Model* m)
{
	Messenger::enter("CellEvent::undo");
	// Cell change - from newLengths/Angles to oldLengths/Angles (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing cell change");
		if (!oldHasCell_) m->removeCell();
		else m->setCell(oldAxes_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying cell change");
		if (!newHasCell_) m->removeCell();
		else m->setCell(newAxes_);
	}
	Messenger::exit("CellEvent::undo");
}

// Print event info
void CellEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Cell change\n");
	else printf("       Cell change\n");
}

/*
 * Charge Event
 */

// Constructor
ChargeEvent::ChargeEvent() : UndoEvent()
{
}

// Destructor
ChargeEvent::~ChargeEvent()
{
}

// Set change 
void ChargeEvent::set(int id, double oldcharge, double newcharge)
{
	Messenger::enter("ChargeEvent::set");
	targetId_ = id;
	oldCharge_ = oldcharge;
	newCharge_ = newcharge;
	Messenger::exit("ChargeEvent::set");
}

// Undo stored change
void ChargeEvent::undo(Model* m)
{
	Messenger::enter("ChargeEvent::undo");
	// Atom charge change - from realData[1] to realData[0] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	Atom* i, **modelatoms = m->atomArray();
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom charge change - atom %i, from %i to %i", targetId_, newCharge_, oldCharge_);
		i->setCharge(oldCharge_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom charge change - atom %i, from %i to %i", targetId_, oldCharge_, newCharge_);
		i->setCharge(newCharge_);
	}
	Messenger::exit("ChargeEvent::undo");
}

// Print event info
void ChargeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, newCharge_, oldCharge_);
	else printf("       Atom charge change - atom %i, from %f to %f\n", targetId_, oldCharge_, newCharge_);
}

/*
 * Glyph Event
 */

// Constructor
GlyphEvent::GlyphEvent() : UndoEvent()
{
}

// Destructor
GlyphEvent::~GlyphEvent()
{
}

// Set change 
void GlyphEvent::set(bool creation, Glyph* g)	// TODO Need a separate event for changes to glyph data - keep all changes in one event somehow?
{
	Messenger::enter("GlyphEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	// Copy Glyph data
	if (g != NULL)
	{
		glyphData_ = *g;
		for (int i=0; i<glyphData_.nGlyphData(g->type()); ++i) atomIDs_[i] = (glyphData_.data(i)->atom() == NULL ? -1 : glyphData_.data(i)->atom()->id());
	}
	else printf("Null pointer passed to GlyphEvent::set()!\n");
	Messenger::exit("GlyphEvent::set");
}

// Undo stored change
void GlyphEvent::undo(Model* m)
{
	Messenger::enter("GlypheEvent::undo");
	// Glyph creation (UndoEvent::Redo) or deletion (UndoEvent::Undo)
	Messenger::exit("GlypheEvent::undo");
}

// Print event info
void GlyphEvent::print()
{
}

/*
 * Colour Atom Event
 */

// Constructor
ColourEvent::ColourEvent() : UndoEvent()
{
}

// Destructor
ColourEvent::~ColourEvent()
{
}

// Set change 
void ColourEvent::set(int id, double oldr, double oldg, double oldb, double olda, double newr, double newg, double newb, double newa)
{
	Messenger::enter("ColourEvent::set");
	targetId_ = id;
	oldColour_[0] = oldr;
	oldColour_[1] = oldg;
	oldColour_[2] = oldb;
	oldColour_[3] = olda;
	newColour_[0] = newr;
	newColour_[1] = newg;
	newColour_[2] = newb;
	newColour_[3] = newa;
	Messenger::exit("ColourEvent::set");
}

// Undo stored change
void ColourEvent::undo(Model* m)
{
	Messenger::enter("ColourEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom hide (UndoEvent::Redo) and show (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom colour - atom id = %i", targetId_);
		m->atomSetColour(i, oldColour_[0], oldColour_[1], oldColour_[2], oldColour_[3]);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom colour - atom id = %i", targetId_);
		m->atomSetColour(i, newColour_[0], newColour_[1], newColour_[2], newColour_[3]);
	}
	Messenger::exit("ColourEvent::undo");
}

// Print event info
void ColourEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom colour undo - atom id = %i\n", targetId_);
	else printf("       Atom colour redo - atom id = %i\n", targetId_);
}

/*
// Fix/Free Event
*/

// Constructor
FixFreeEvent::FixFreeEvent() : UndoEvent()
{
}

// Destructor
FixFreeEvent::~FixFreeEvent()
{
}

// Set change 
void FixFreeEvent::set(bool fix, int id)
{
	Messenger::enter("FixFreeEvent::set");
	direction_ = (fix ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("FixFreeEvent::set");
}

// Undo stored change
void FixFreeEvent::undo(Model* m)
{
	Messenger::enter("FixFreeEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom hide (UndoEvent::Redo) and show (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom fix - atom id = %i", targetId_);
		m->atomSetFixed(i, false);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom fix - atom id = %i", targetId_);
		m->atomSetFixed(i, true);
	}
	Messenger::exit("FixFreeEvent::undo");
}

// Print event info
void FixFreeEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom fix - atom id = %i\n", targetId_);
	else printf("       Atom free - atom id = %i\n", targetId_);
}

/*
 * Hide Event
 */

// Constructor
HideEvent::HideEvent() : UndoEvent()
{
}

// Destructor
HideEvent::~HideEvent()
{
}

// Set change 
void HideEvent::set(bool hide, int id)
{
	Messenger::enter("HideEvent::set");
	direction_ = (hide ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("HideEvent::set");
}

// Undo stored change
void HideEvent::undo(Model* m)
{
	Messenger::enter("HideEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom hide (UndoEvent::Redo) and show (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom hide - atom id = %i", targetId_);
		m->atomSetHidden(i, false);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom hide - atom id = %i", targetId_);
		m->atomSetHidden(i, true);
	}
	Messenger::exit("HideEvent::undo");
}

// Print event info
void HideEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom hide - atom id = %i\n", targetId_);
	else printf("       Atom show - atom id = %i\n", targetId_);
}

/*
 * IdShift Event
 */

// Constructor
IdShiftEvent::IdShiftEvent() : UndoEvent()
{
}

// Destructor
IdShiftEvent::~IdShiftEvent()
{
}

// Set change 
void IdShiftEvent::set(int id, int delta)
{
	Messenger::enter("IdShiftEvent::set");
	targetId_ = id;
	delta_ = delta;
	Messenger::exit("IdShiftEvent::set");
}

// Undo stored change
void IdShiftEvent::undo(Model* m)
{
	Messenger::enter("IdShiftEvent::undo");
	// Atom list position change - -data[1] (UndoEvent::Undo) or +data[1] places in list (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom shift - atom %i moves %i places", targetId_+delta_, -delta_);
		m->moveAtom(targetId_+delta_, -delta_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Performing atom shift - atom %i moves %i places", targetId_, delta_);
		m->moveAtom(targetId_, delta_);
	}
	Messenger::exit("IdShiftEvent::undo");
}

// Print event info
void IdShiftEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom shift - atom %i moves %i places\n", targetId_+delta_, -delta_);
	else printf("       Atom shift - atom %i moves %i places\n", targetId_, delta_);
}

/*
 * IdSwap Event
 */

// Constructor
IdSwapEvent::IdSwapEvent() : UndoEvent()
{
}

// Destructor
IdSwapEvent::~IdSwapEvent()
{
}

// Set change 
void IdSwapEvent::set(int id1, int id2)
{
	Messenger::enter("IdSwapEvent::set");
	firstId_ = id1;
	secondId_ = id2;
	Messenger::exit("IdSwapEvent::set");
}

// Undo stored change
void IdSwapEvent::undo(Model* m)
{
	Messenger::enter("IdSwapEvent::undo");
	// Atom swap change - same regardless of direction
	Messenger::print(Messenger::Verbose, "Applying atom swap - atoms %i and %i", firstId_, secondId_);

	m->swapAtoms(firstId_, secondId_);

	Messenger::exit("IdSwapEvent::undo");
}

// Print event info
void IdSwapEvent::print()
{
	printf("       Atom swap - atom %i with atom %i\n", firstId_, secondId_);
}

/*
 * Label Event
 */

// Constructor
LabelEvent::LabelEvent() : UndoEvent()
{
}

// Destructor
LabelEvent::~LabelEvent()
{
}

// Set change 
void LabelEvent::set(int id, int oldlabels, int newlabels)
{
	Messenger::enter("LabelEvent::set");
	targetId_ = id;
	oldLabels_ = oldlabels;
	newLabels_ = newlabels;
	Messenger::exit("LabelEvent::set");
}

// Undo stored change
void LabelEvent::undo(Model* m)
{
	Messenger::enter("LabelEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom label change - from data[2] to data[1] (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom label change - atom %i, from %i to %i", targetId_, newLabels_, oldLabels_);
		i->setLabels(oldLabels_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom label change - atom %i, from %i to %i", targetId_, oldLabels_, newLabels_);
		i->setLabels(newLabels_);
	}
	Messenger::exit("LabelEvent::undo");
}

// Print event info
void LabelEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Reversing atom label change - atom %i, from %i to %i\n", targetId_, newLabels_, oldLabels_);
	else printf("       Replaying atom label change - atom %i, from %i to %i\n", targetId_, oldLabels_, newLabels_);
}

/*
 * Measurement Event
 */

// Constructor
MeasurementEvent::MeasurementEvent() : UndoEvent()
{
}

// Destructor
MeasurementEvent::~MeasurementEvent()
{
}

// Set change 
void MeasurementEvent::set(bool creation, Measurement::MeasurementType mt, int id1, int id2, int id3, int id4)
{
	Messenger::enter("MeasurementEvent::set");
	direction_ = (creation ? UndoEvent::Undo : UndoEvent::Redo);
	type_ = mt;
	targetId_[0] = id1;
	targetId_[1] = id2;
	targetId_[2] = id3;
	targetId_[3] = id4;
	Messenger::exit("MeasurementEvent::set");
}

// Undo stored change
void MeasurementEvent::undo(Model* m)
{
	Messenger::enter("MeasurementEvent::undo");
	Measurement* me = NULL;
	Atom* i, *j, *k,*l, **modelatoms = m->atomArray();
	// Measurement creation (UndoEvent::Undo) and deletion (UndoEvent::Redo)
	i = modelatoms[targetId_[0]];
	j = modelatoms[targetId_[1]];
	k = modelatoms[targetId_[2]];
	l = modelatoms[targetId_[3]];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing measurement - type = %i", type_);
		if (type_ == Measurement::DistanceMeasurement) me = m->findDistanceMeasurement(i, j);
		if (type_ == Measurement::AngleMeasurement) me = m->findAngleMeasurement(i, j, k);
		if (type_ == Measurement::TorsionMeasurement) me = m->findTorsionMeasurement(i, j, k, l);
		if (me != NULL) m->removeMeasurement(me);
		else printf("Couldn't find measurement in UndoEvent.\n");
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying measurement - type = %i", type_);
		m->addMeasurement(type_, i, j, k, l);
	}
	Messenger::exit("MeasurementEvent::undo");
}

// Print event info
void MeasurementEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Measurement deletion, type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
	else printf("       Measurement creation - type = %i, atoms = %i %i %i %i\n", type_, targetId_[0], targetId_[1], targetId_[2], targetId_[3]);
}

/*
 * Model Rename Event
 */

// Constructor
ModelRenameEvent::ModelRenameEvent() : UndoEvent()
{
}

// Destructor
ModelRenameEvent::~ModelRenameEvent()
{
}

// Set change 
void ModelRenameEvent::set(QString oldName, QString newName)
{
	Messenger::enter("ModelRenameEvent::set");
	oldName_ = oldName;
	newName_ = newName;
	Messenger::exit("ModelRenameEvent::set");
}

// Undo stored change
void ModelRenameEvent::undo(Model* m)
{
	Messenger::enter("ModelRenameEvent::undo");
	// Model Rename, to oldName_ (UndoEvent::Undo) or newName_ (UndoEvent::Redo)
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing model rename - to %i", qPrintable(oldName_));
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying model rename - to %i", qPrintable(newName_));
	}
	Messenger::exit("ModelRenameEvent::undo");
}

// Print event info
void ModelRenameEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Model rename from %s to %s\n", qPrintable(newName_), qPrintable(oldName_));
	else printf("       Model rename from %s to %s\n",  qPrintable(oldName_), qPrintable(newName_));
}

/*
 * Selection Event
 */

// Constructor
SelectEvent::SelectEvent() : UndoEvent()
{
}

// Destructor
SelectEvent::~SelectEvent()
{
}

// Set change 
void SelectEvent::set(bool select, int id)
{
	Messenger::enter("SelectionEvent::set");
	direction_ = (select ? UndoEvent::Undo : UndoEvent::Redo);
	targetId_ = id;
	Messenger::exit("SelectionEvent::set");
}

// Undo stored change
void SelectEvent::undo(Model* m)
{
	Messenger::enter("SelectEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom selection (UndoEvent::Redo) and deselection (UndoEvent::Undo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom selection - atom id = %i", targetId_);
		m->deselectAtom(i);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom selection - atom id = %i", targetId_);
		m->selectAtom(i);
	}
	Messenger::exit("SelectEvent::undo");
}

// Print event info
void SelectEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom selection - atom id = %i\n", targetId_);
	else printf("       Atom deselection - atom id = %i\n", targetId_);
}

/*
 * Translate Event
 */

// Constructor
TranslateEvent::TranslateEvent() : UndoEvent()
{
}

// Destructor
TranslateEvent::~TranslateEvent()
{
}

// Set change 
void TranslateEvent::set(int id, Vec3<double> delta)
{
	Messenger::enter("TranslateEvent::set");
	targetId_ = id;
	delta_ = delta;
	Messenger::exit("TranslateEvent::set");
}

// Undo stored change
void TranslateEvent::undo(Model* m)
{
	Messenger::enter("TranslateEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom position change - add (UndoEvent::Undo) or subtract (UndoEvent::Redo) delta_.
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom translation - atom %i, subtracting %f %f %f", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() -= delta_;
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom translation - atom %i, adding %f %f %f", targetId_, delta_.x, delta_.y, delta_.z);
		i->r() += delta_;
	}
	Messenger::exit("TranslateEvent::undo");
}

// Print event info
void TranslateEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom translation - atom %i, subtracting %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
	else printf("       Atom translation - atom %i, adding %f %f %f\n", targetId_, delta_.x, delta_.y, delta_.z);
}

/*
 * Style Event
 */

// Constructor
StyleEvent::StyleEvent() : UndoEvent()
{
}

// Destructor
StyleEvent::~StyleEvent()
{
}

// Set change 
void StyleEvent::set(int id, Prefs::DrawStyle oldstyle, Prefs::DrawStyle newstyle)
{
	Messenger::enter("StyleEvent::set");
	targetId_ = id;
	oldStyle_ = oldstyle;
	newStyle_ = newstyle;
	Messenger::exit("StyleEvent::set");
}

// Undo stored change
void StyleEvent::undo(Model* m)
{
	Messenger::enter("StyleEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom style change - newStyle_ to oldStyle_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom style change - atom %i, old = %i, new = %i", targetId_, newStyle_, oldStyle_);
		m->atomSetStyle(i, oldStyle_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom style change - atom %i, old = %i, new = %i", targetId_, oldStyle_, newStyle_);
		m->atomSetStyle(i, newStyle_);
	}
	Messenger::exit("StyleEvent::undo");
}

// Print event info
void StyleEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom style - atom %i, old = %i, new = %i\n", targetId_, newStyle_, oldStyle_);
	else printf("       Atom style - atom %i, old = %i, new = %i\n", targetId_, oldStyle_, newStyle_);
}

/*
 * Transmute Event
 */

// Constructor
TransmuteEvent::TransmuteEvent() : UndoEvent()
{
}

// Destructor
TransmuteEvent::~TransmuteEvent()
{
}

// Set change 
void TransmuteEvent::set(int id, int oldel, int newel)
{
	Messenger::enter("TransmuteEvent::set");
	targetId_ = id;
	oldEl_ = oldel;
	newEl_ = newel;
	Messenger::exit("TransmuteEvent::set");
}

// Undo stored change
void TransmuteEvent::undo(Model* m)
{
	Messenger::enter("TransmuteEvent::undo");
	Atom* i, **modelatoms = m->atomArray();
	// Atom transmute - newEl_ to oldEl_ (UndoEvent::Undo) or vice versa (UndoEvent::Redo)
	i = modelatoms[targetId_];
	if (direction_ == UndoEvent::Undo)
	{
		Messenger::print(Messenger::Verbose, "Reversing atom transmute - atom %i, old = %i, new = %i", targetId_, newEl_, oldEl_);
		m->transmuteAtom(i, oldEl_);
	}
	else
	{
		Messenger::print(Messenger::Verbose, "Replaying atom transmute - atom %i, old = %i, new = %i", targetId_, oldEl_, newEl_);
		m->transmuteAtom(i, newEl_);
	}
	Messenger::exit("TransmuteEvent::undo");
}

// Print event info
void TransmuteEvent::print()
{
	if (direction_ == UndoEvent::Undo) printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, newEl_, oldEl_);
	else printf("       Atom transmute - atom %i, old = %i, new = %i\n", targetId_, oldEl_, newEl_);
}

/*
 * Base UndoEvent
 */

// Redo stored change
void UndoEvent::redo(Model* m)
{
	Messenger::enter("UndoEvent::redo");
	// Re-use the commands in Change::undo, performing the change in the opposite direction
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);
	// Now just call reverse instead, and then set the old direction back at the end
	undo(m);
	direction_ = (direction_ == UndoEvent::Undo ? UndoEvent::Redo : UndoEvent::Undo);
	Messenger::exit("UndoeEvent::redo");
}
