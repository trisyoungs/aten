/*
	*** Geometry Measurement
	*** src/base/measurement.h
	Copyright T. Youngs 2007-2017

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

#ifndef ATEN_MEASUREMENT_H
#define ATEN_MEASUREMENT_H

#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class UnitCell;

// Measurement
class Measurement : public ListItem<Measurement>
{
	public:
	// Constructor
	Measurement();
	// Geometry types
	enum MeasurementType { NoMeasurement, DistanceMeasurement, AngleMeasurement, TorsionMeasurement, nMeasurementTypes };
	static int nMeasurementAtoms(MeasurementType);

	/*
	 * Measurement Data
	 */
	private:
	// Type of Measurement
	MeasurementType type_;
	// Atoms involved Measurement
	Atom* atoms_[4];
	// Minimum image value of measurement
	double value_;
	// Literal (coordinate) value of Measurement
	double literalValue_;

	public:
	// Set type of Measurement
	void setType(MeasurementType gt);
	// Return type of Measurement
	MeasurementType type() const;
	// Calculate Measurement value
	void calculate(UnitCell* cell);
	// Return proper, MIM'd value of the Measurement
	double value() const;
	// Return literal coordinate value of the Measurement
	double literalValue() const;
	// Set atom
	void setAtom(int n, Atom* i);
	// Return whether the specified atom is used in this measurement
	bool involvesAtom(Atom* i) const;
	// Return specific atom
	Atom* atom(int index);
	// Return atoms array
	Atom** atoms();
	// Print
	void print() const;
};

ATEN_END_NAMESPACE

#endif
