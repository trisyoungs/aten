/*
	*** Eigenvector class
	*** src/base/eigenvector.h
	Copyright T. Youngs 2007-2012

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

#ifndef ATEN_EIGENVECTOR_H
#define ATEN_EIGENVECTOR_H

#include "base/dnchar.h"

// Eigenvector (as for MO)
class Eigenvector
{
	public:
	// Constructor / Destructor
	Eigenvector();
	~Eigenvector();
	// List pointers
	Eigenvector *prev, *next;

	/*
	// Data
	*/
	private:
	// Name (typically symmetry type) of eigenvector
	Dnchar name_;
	// Whether the coefficients correspond to spherical (TRUE) or cartesian (FALSE) basis functions
	bool isSpherical_;
	// Size of eigenvector when created
	int size_;
	// Eigenvector
	double *eigenvector_;
	// Eigenvalue
	double eigenvalue_;
	// Occupancy
	double occupancy_;

	public:
	// Initialise to be specified size
	void initialise(int size);
	// Return size of eigenvector
	int size() const;
	// Return whether coefficients correspond to spherical or cartesian functions
	bool isSpherical();
	// Set text name of eigenvalue
	void setName(const char *name);
	// Return text 'name' of eigenvalue
	const char *name() const;
	// Set array index
	void setValue(int index, double value);
	// Return array value specified
	double value(int index);
	// Return array pointer
	double *eigenvector();
	// Set eigenvalue
	void setEigenvalue(double d);
	// Return eigenvalue
	double eigenvalue();
	// Set occupancy
	void setOccupancy(double d);
	// Return occupancy
	double occupancy();
};

#endif
