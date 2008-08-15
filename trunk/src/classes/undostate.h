/*
	*** Undo level storage
	*** src/classes/undo.h
	Copyright T. Youngs 2007,2008

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

#ifndef ATEN_UNDOLEVEL_H
#define ATEN_UNDOLEVEL_H

#include "classes/dnchar.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "templates/matrix3.h"

// Forward declarations
class Model;
class Atom;

// Single change
class Change
{
	public:
	// Constructor / destructor
	Change();
	~Change();
	// List pointers
	Change *prev, *next;
	// Change logs
	// Change::StructureLog  : create/destroy atoms/bonds, change elements
	// Change::CoordinateLog : atomic coordinates
	// Change::VisualLog     : visual changes that require re-rendering
	// Change::SelectionLog  : atom selection
	// Change::CameraLog     : view (mainly used to flag reprojection)
	// Change::GlyphLog      : glyphs
	// Change::TotalLog      : sum of all changes
	enum ChangeLog { StructureLog, CoordinateLog, VisualLog, SelectionLog, CameraLog, GlyphLog, TotalLog, nChangeLogs };
	// State change events
	enum UndoEvent { NoEvent, AtomEvent, BondEvent, MeasurementEvent, SelectEvent, TransmuteEvent, BondOrderEvent, CellEvent, LabelEvent, TranslateEvent, ShiftEvent, ChargeEvent, GlyphEvent };
	// State change directions
	enum ChangeDirection { Undo, Redo };

	/*
	// Data
	*/
	private:
	// Type of change
	UndoEvent type_;
	// Direction of change
	ChangeDirection direction_;
	// Atom data describing the change
	Atom *atomData_[2];
	// Vector data describing the change
	Vec3<double> *vecData_[4];
	// Double data describing the change
	double realData_[2];
	// Generally-applicable data
	int data_[5];

	public:
	// Set change data (atoms)
	void set(int ec, Atom *i, Atom *j = NULL);
	// Set change data (integers)
	void set(int ec, int i, int j = -1, int k = -1, int l = -1, int m = -1);
	// Set change data (double)
	void set(int ec, double a, double b);
	// Set change data (matrices)
	void set(int ec, Mat3<double> *m1, Mat3<double> *m2 = NULL);
	// Set change data (vector)
	void set(int ec, Vec3<double> *v1, Vec3<double> *v2 = NULL, Vec3<double> *v3 = NULL, Vec3<double> *v4 = NULL);

	/*
	// Actions
	*/
	public:
	// Undo stored change
	void undo(Model *m);
	// Redo stored change
	void redo(Model *m);
	// Print change information
	void print(Model *m);
};

// Undo state
class UndoState
{
	public:
	// Constructor
	UndoState();
	// List pointers
	UndoState *prev, *next;

	/*
	// Changelist
	*/
	private:
	// List of atomic changes for this level
	List<Change> changes_;
	// Short text describing the change
	Dnchar description_;
	// Logs at start of state
	int startLogs_[Change::nChangeLogs];
	// Logs at end of state
	int endLogs_[Change::nChangeLogs];

	public:
	// Add change to undostate
	Change *addChange();
	// Return number of changes in list
	int nChanges();
	// Set log point at start of state
	void setStartLog(Change::ChangeLog log, int value);
	// Get structure log point at start of state
	int startLog(Change::ChangeLog log);
	// Set log point at end of state
	void setEndLog(Change::ChangeLog log, int value);
	// Get structure log point at end of state
	int endLog(Change::ChangeLog log);
	// Check difference between Change::StructureLog and Change::CoordinateLog between start/end points
	bool doLogsDiffer();
	// Set the text associated with the current undo state
	void setDescription(const char *s);
	// Return the current text associated with the state
	const char *description();
	// Undo the changes specified in the state
	void undo(Model *m);
	// Redo the changes specified in the state
	void redo(Model *m);
	// Print changes captured in state
	void print(Model *m);
};

#endif
