/*
	*** Geometry measurement
	*** src/classes/measurement.cpp
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

#include "classes/measurement.h"
#include "classes/atom.h"
#include "classes/cell.h"

// Geometry types
int GT_natoms_[GT_NITEMS] = { 0,2,3,4 };
int natoms_from_GT(GeometryType gt)
	{ return GT_natoms_[gt]; }

// Constructor
Measurement::Measurement()
{
	// Private variables
	type_ = GT_NONE;
	for (int n=0; n<4; n++) atoms_[n] = NULL;
	value_ = 0.0;
	// Public variables
	next = NULL;
	prev = NULL;
}

// Set type of Measurement
void Measurement::setType(GeometryType gt)
{
	type_ = gt;
}

// Return type of Measurement
GeometryType Measurement::type()
{
	return type_;
}

// Return value of the Measurement
double Measurement::value()
{
	return value_;
}

// Set atom
void Measurement::setAtom(int n, Atom *i)
{
	atoms_[n] = i;
}

// Return atoms array
Atom **Measurement::atoms()
{
	return atoms_;
}

// Calculate
void Measurement::calculate(Cell *cell)
{
	switch (type_)
	{
		case (GT_DISTANCE):
			value_ = cell->distance(atoms_[0],atoms_[1]);
			break;
		case (GT_ANGLE):
			value_ = cell->angle(atoms_[0],atoms_[1],atoms_[2]) * DEGRAD;
			break;
		case (GT_TORSION):
			value_ = cell->torsion(atoms_[0],atoms_[1],atoms_[2],atoms_[3]) * DEGRAD;
			break;
		default:
			printf("Measurement::calculate <<<< Unrecognised geometry type >>>>\n");
			break;
	}
}
