/*
	*** Single event in UndoState
	*** src/model/undoevent.h
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

#ifndef ATEN_UNDOEVENT_H
#define ATEN_UNDOEVENT_H

#include "base/matrix.h"
#include "base/dnchar.h"
#include "base/atom.h"
#include "base/bond.h"
#include "base/glyph.h"
#include "base/measurement.h"

// Forward declarations
class Model;

// UndoEvent Base Class
class UndoEvent : public ListItem<UndoEvent>
{
	public:
	// Constructor / Destructor
	UndoEvent();
	virtual ~UndoEvent();
	// State change directions
	enum EventDirection { Undo, Redo };


	/*
	// Data
	*/
	protected:
	// Direction of change
	EventDirection direction_;


	/*
	// Actions
	*/
	public:
	// Undo stored change
	virtual void undo(Model *m)=0;
	// Redo stored change
	void redo(Model *m);
	// Print change information
	virtual void print()=0;
};

// Atom (Creation/Deletion) Event
class AtomEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	AtomEvent();
	~AtomEvent();
	
	private:
	// Change data
	Atom atomData_;

	public:
	// Set change data
	void set(bool creation, Atom *i);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Bond (Creation/Deletion) Event
class BondEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	BondEvent();
	~BondEvent();

	private:
	// Change data
	int targetId1_, targetId2_;
	Bond::BondType bondType_;

	public:
	// Set change data
	void set(bool creation, int id1, int id2, Bond::BondType bondtype);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Bond Type Event
class BondTypeEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	BondTypeEvent();
	~BondTypeEvent();
	
	private:
	// Change data
	int targetId1_, targetId2_;
	Bond::BondType oldBondType_, newBondType_;

	public:
	// Set change data
	void set(int id1, int id2, Bond::BondType oldbondtype, Bond::BondType newbondtype);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Cell Change Event
class CellEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	CellEvent();
	~CellEvent();
	
	private:
	// Change data
	Matrix oldAxes_, newAxes_;
	bool oldHasCell_, newHasCell_;

	public:
	// Set change data
	void set(Matrix oldaxes, Matrix newaxes, bool ohs, bool nhs);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Charge Event
class ChargeEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	ChargeEvent();
	~ChargeEvent();

	private:
	// Change data
	int targetId_;
	double oldCharge_, newCharge_;

	public:
	// Set change data
	void set(int id, double oldcharge, double newcharge);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Colour (atom) Event
class ColourEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	ColourEvent();
	~ColourEvent();
	
	private:
	// Change data
	int targetId_;
	double oldColour_[4], newColour_[4];

	public:
	// Set change data
	void set(int id, double oldr, double oldg, double oldb, double olda, double newr, double newg, double newb, double newa);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Fix/Free Event
class FixFreeEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	FixFreeEvent();
	~FixFreeEvent();

	private:
	// Change data
	int targetId_;

	public:
	// Set change data
	void set(bool fix, int id);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Glyph (Creation/Deletion) Event
class GlyphEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	GlyphEvent();
	~GlyphEvent();
	
	private:
	// Change data
	Glyph glyphData_;
	int atomIDs_[4];

	public:
	// Set change data
	void set(bool creation, Glyph *g);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Hide/Show Event
class HideEvent : public UndoEvent
{
public:
	// Constructor / Destructor
	HideEvent();
	~HideEvent();
	
	private:
	// Change data
	int targetId_;

	public:
	// Set change data
	void set(bool hide, int id);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Atom ID Shift Event
class IdShiftEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	IdShiftEvent();
	~IdShiftEvent();
	
	private:
	// Change data
	int targetId_, delta_;

	public:
	// Set change data
	void set(int id, int delta);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Atom ID Swap Event
class IdSwapEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	IdSwapEvent();
	~IdSwapEvent();
	
	private:
	// Change data
	int firstId_, secondId_;

	public:
	// Set change data
	void set(int id1, int id2);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Label Event
class LabelEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	LabelEvent();
	~LabelEvent();
	
	private:
	// Change data
	int targetId_, oldLabels_, newLabels_;

	public:
	// Set change data
	void set(int id, int oldlabels, int newlabels);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Measurement (Creation/Deletion) Event
class MeasurementEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	MeasurementEvent();
	~MeasurementEvent();
	
	private:
	// Change data
	Measurement::MeasurementType type_;
	int targetId_[4];

	public:
	// Set change data
	void set(bool creation, Measurement::MeasurementType, int id1, int id2, int id3 = 0, int id4 = 0);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Model Rename Event
class ModelRenameEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	ModelRenameEvent();
	~ModelRenameEvent();
	
	private:
	// Change data
	Dnchar oldName_, newName_;

	public:
	// Set change data
	void set(const char *oldname, const char *newname);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Select/Deselect Event
class SelectEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	SelectEvent();
	~SelectEvent();	

	private:
	// Change data
	int targetId_;

	public:
	// Set change data
	void set(bool select, int id);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Atom Translation Event
class TranslateEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	TranslateEvent();
	~TranslateEvent();
	
	private:
	// Change data
	int targetId_;
	Vec3<double> delta_;

	public:
	// Set change data
	void set(int id, Vec3<double> delta);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Atom Style Change
class StyleEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	StyleEvent();
	~StyleEvent();
	
	private:
	// Change data
	int targetId_;
	Atom::DrawStyle oldStyle_, newStyle_;

	public:
	// Set change data
	void set(int id, Atom::DrawStyle oldstyle, Atom::DrawStyle newstyle);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Atom Transmute Change
class TransmuteEvent : public UndoEvent
{
	public:
	// Constructor / Destructor
	TransmuteEvent();
	~TransmuteEvent();
	
	private:
	// Change data
	int targetId_, oldEl_, newEl_;

	public:
	// Set change data
	void set(int id, int oldel, int newel);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

#endif
