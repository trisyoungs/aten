/*
	*** Symmetry generator
	*** src/base/generator.h
	Copyright T. Youngs 2007-2009

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
#include "templates/vector3.h"
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
	Mat4<double> matrix_;
	// Set partial element of matrix
	bool setMatrixPart(int row, const char *s);

	public:
	// Set from XYZ-style name
	bool set(const char *xyz);
	// Set from integer list from sginfo
	bool set(int *elements);
	// Return text 'name' of generator
	const char *name();
	// Negate elements in matrix
	void negateMatrix();
	// Return operator matrix of generator
	Mat4<double> &matrix();
};

#endif
