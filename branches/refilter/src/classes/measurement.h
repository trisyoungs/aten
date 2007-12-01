/*
	*** Geometry measurement
	*** src/classes/measurement.h
	Copyright T. Youngs 2007

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

#ifndef H_MEASUREMENT_H
#define H_MEASUREMENT_H

// Geometry types
enum geom_type { GT_NONE, GT_DISTANCE, GT_ANGLE, GT_TORSION, GT_NITEMS };
int natoms_from_GT(geom_type);

// Forward Declarations
class atom;
class unitcell;

// Measurement
class measurement
{
	public:
	// Constructor / Destructor
	measurement();
	~measurement();
	// List pointers
	measurement *next, *prev;

	/*
	// Measurement Data
	*/
	private:
	// Type of measurement
	geom_type type;
	// Atoms involved measurement
	atom* atoms[4];
	// Value of measurement
	double value;

	public:
	// Set type of measurement
	void set_type(geom_type gt) { type = gt; }
	// Return type of measurement
	geom_type get_type() { return type; }
	// Calculate measurement value
	void calculate(unitcell *cell);
	// Return value of the measurement
	double get_value() { return value; }
	// Set atom
	void set_atom(int n, atom *i) { atoms[n] = i; }
	// Return atoms array
	atom **get_atoms() { return atoms; }
};

#endif
