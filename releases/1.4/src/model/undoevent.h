/*
	*** Single event in UndoState
	*** src/model/undoevent.h
	Copyright T. Youngs 2007-2009

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUE ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ATEN_UNDOEVENT_H
#define ATEN_UNDOEVENT_H

#include "base/dnchar.h"
#include "base/atom.h"
#include "base/bond.h"
#include "base/glyph.h"
#include "base/measurement.h"

// Forward declarations
class Model;

// UndoEvent Base Class
class UndoEvent
{
	public:
	// Constructor
	UndoEvent();
	// List pointers
	UndoEvent *prev, *next;
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
	private:
	// Change data
	Vec3<double> oldAngles_, oldLengths_, newAngles_, newLengths_;
	bool oldHasCell_, newHasCell_;

	public:
	// Set change data
	void set(Vec3<double> oldangles, Vec3<double> oldlengths, Vec3<double> newangles, Vec3<double> newlengths, bool ohs, bool nhs);
	// Undo stored change
	void undo(Model *m);
	// Print change information
	void print();
};

// Charge Event
class ChargeEvent : public UndoEvent
{
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

// Fix/Free Event
class FixFreeEvent : public UndoEvent
{
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
	private:
	// Change data
	Glyph glyphData_;

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

// Label Event
class LabelEvent : public UndoEvent
{
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
