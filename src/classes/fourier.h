/*
	*** Fourier storage (reciprocal space vectors)
	*** src/classes/fourier.h
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

#ifndef H_FOURIER_H
#define H_FOURIER_H

#include "templates/vector3.h"

// Forward declarations
class unitcell;
class model;

// Fourier
struct fourier_data
{
	// Constructor / Destructor
	fourier_data();
	~fourier_data();
	vec3<double> **rcos, **rsin;
	int natoms, kmax;
	vec3<int> kvec;
	unitcell *cell;
	// Parameters used in Ewald sum.
	double alpha, alphasq;
	// Class Functions
	// Delete the vector arrays in the class
	void clear();
	// Create the vector arrays in the class
	void create(int, vec3<int>, int);
	// Calculate all atomic kvectors from the supplied config
	void calculate(model*);
	// Calculate selected range of atomic vectors from supplied config
	void calculate(model*, int, int);
	// Prepares the class with the specifications provided (and 'calculates')
	void prepare(model*, vec3<int>);
};

extern fourier_data fourier;

#endif
