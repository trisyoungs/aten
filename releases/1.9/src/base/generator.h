/*
	*** Symmetry generator
	*** src/base/generator.h
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

#ifndef ATEN_GENERATOR_H
#define ATEN_GENERATOR_H

#include "base/dnchar.h"
#include "base/matrix.h"
#include "templates/list.h"

// Symmetry generator
class Generator
{
	public:
	// Constructor
	Generator();
	// List pointers
	Generator *prev, *next;

	/*
	// Rotation Matrix and Translation Vector
	*/
	private:
	// Generator text (if any)
	Dnchar name_;
	// Generator matrix
	Matrix matrix_;
	// Set partial element of matrix in specified row
	void setMatrixPart(int row, const char *s);

	public:
	// Set from XYZ-style name
	bool set(const char *xyz);
	// Set rotation matrix row (not including translation vector)
	void setRotationRow(int row, double x, double y, double z);
	// Set translation column
	void setTranslation(double tx, double ty, double tz, double divisor);
	// Return text 'name' of generator
	const char *name() const;
	// Return operator matrix of generator
	 Matrix &matrix();
};

#endif
