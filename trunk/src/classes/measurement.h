/*
	*** Geometry Measurement
	*** src/classes/measurement.h
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

#ifndef ATEN_MEASUREMENT_H
#define ATEN_MEASUREMENT_H

// Forward Declarations
class Atom;
class Cell;

// Measurement
class Measurement
{
	public:
	// Constructor
	Measurement();
	// List pointers
	Measurement *next, *prev;
	// Geometry types
	enum MeasurementType { NoMeasurement, DistanceMeasurement, AngleMeasurement, TorsionMeasurement, nMeasurementTypes };
	static int nMeasurementAtoms(MeasurementType);

	/*
	// Measurement Data
	*/
	private:
	// Type of Measurement
	MeasurementType type_;
	// Atoms involved Measurement
	Atom* atoms_[4];
	// Value of Measurement
	double value_;

	public:
	// Set type of Measurement
	void setType(MeasurementType gt);
	// Return type of Measurement
	MeasurementType type();
	// Calculate Measurement value
	void calculate(Cell *cell);
	// Return value of the Measurement
	double value();
	// Set atom
	void setAtom(int n, Atom *i);
	// Return atoms array
	Atom **atoms();
};

#endif
