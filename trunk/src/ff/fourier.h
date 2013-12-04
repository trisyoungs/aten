/*
	*** Fourier storage (reciprocal space vectors)
	*** src/ff/fourier.h
	Copyright T. Youngs 2007-2013

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

#ifndef ATEN_FOURIER_H
#define ATEN_FOURIER_H

#include "templates/vector3.h"

// Forward declarations
class UnitCell;
class Model;

// Fourier
class FourierData
{
	public:
	// Constructor / Destructor
	FourierData();
	~FourierData();
	Vec3<double> **rCos, **rSin;
	int nAtoms, kMax;
	Vec3<int> kVec;
	UnitCell *cell;
	// Parameters used in Ewald sum.
	double alpha, alphaSq;
	// Class Functions
	// Delete the vector arrays in the class
	void clear();
	// Create the vector arrays in the class
	void create(int, Vec3<int>, int);
	// Calculate all atomic kvectors from the supplied config
	void calculate(Model*);
	// Calculate selected range of atomic vectors from supplied config
	void calculate(Model*, int, int);
	// Prepares the class with the specifications provided (and 'calculates')
	void prepare(Model*, Vec3<int>);
};

extern FourierData fourier;

#endif
