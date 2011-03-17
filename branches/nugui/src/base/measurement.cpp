/*
	*** Geometry measurement
	*** src/base/measurement.cpp
	Copyright T. Youngs 2007-2011

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

#include "base/measurement.h"
#include "base/cell.h"
#include "base/atom.h"
#include <stdlib.h>

// Geometry types
int MeasurementAtoms[Measurement::nMeasurementTypes] = { 0,2,3,4 };
int Measurement::nMeasurementAtoms(Measurement::MeasurementType mt)
{
	return MeasurementAtoms[mt];
}

// Constructor
Measurement::Measurement()
{
	// Private variables
	type_ = Measurement::None;
	for (int n=0; n<4; n++) atoms_[n] = NULL;
	value_ = 0.0;
	literalValue_ = 0.0;
	
	// Public variables
	next = NULL;
	prev = NULL;
}

// Set type of Measurement
void Measurement::setType(Measurement::MeasurementType gt)
{
	type_ = gt;
}

// Return type of Measurement
Measurement::MeasurementType Measurement::type() const
{
	return type_;
}

// Return MIM'd value of the Measurement
double Measurement::value() const
{
	return value_;
}

// Return literal value of the Measurement
double Measurement::literalValue() const
{
	return literalValue_;
}

// Set atom
void Measurement::setAtom(int n, Atom *i)
{
	atoms_[n] = i;
}

// Return whether the specified atom is used in this measurement
bool Measurement::involvesAtom(Atom *i) const
{
	for (int n=0; n<4; ++n) if (atoms_[n] == i) return TRUE;
	return FALSE;
}

// Return specific atom
Atom *Measurement::atom(int index)
{
	if ((index < 0) || (index > 3)) printf("Index %i out of bounds for measurement.\n", index);
	else return atoms_[index];
	return NULL;
}

// Return atoms array
Atom **Measurement::atoms()
{
	return atoms_;
}

// Calculate
void Measurement::calculate(UnitCell *cell)
{
	switch (type_)
	{
		case (Measurement::Distance):
			literalValue_ = cell->distance(atoms_[0],atoms_[1],FALSE);
			value_ = cell->distance(atoms_[0],atoms_[1]);
			break;
		case (Measurement::Angle):
			literalValue_ = cell->angle(atoms_[0],atoms_[1],atoms_[2],FALSE);
			value_ = cell->angle(atoms_[0],atoms_[1],atoms_[2]);
			break;
		case (Measurement::Torsion):
			literalValue_ = cell->torsion(atoms_[0],atoms_[1],atoms_[2],atoms_[3],FALSE);
			value_ = cell->torsion(atoms_[0],atoms_[1],atoms_[2],atoms_[3]);
			break;
		default:
			printf("Measurement::calculate <<<< Unrecognised geometry type >>>>\n");
			break;
	}
}

// Print
void Measurement::print() const
{
	switch (type_)
	{
		case (Measurement::Distance):
			if (fabs(value_-literalValue_) < 0.0001) msg.print("Distance (%i-%i) = %f\n", atoms_[0]->id()+1, atoms_[1]->id()+1, value_);
			else msg.print("Distance (%i-%i) = %f (literal = %f)\n", atoms_[0]->id()+1, atoms_[1]->id()+1, value_, literalValue_);
			break;
		case (Measurement::Angle):
			if (fabs(value_-literalValue_) < 0.0001) msg.print("Angle (%i-%i-%i) = %f\n", atoms_[0]->id()+1, atoms_[1]->id()+1, atoms_[2]->id()+1, value_);
			else msg.print("Angle (%i-%i-%i) = %f (literal = %f)\n", atoms_[0]->id()+1, atoms_[1]->id()+1, atoms_[2]->id()+1, value_, literalValue_);
			break;
		case (Measurement::Torsion):
			if (fabs(value_-literalValue_) < 0.0001) msg.print("Torsion (%i-%i-%i-%i) = %f\n", atoms_[0]->id()+1, atoms_[1]->id()+1, atoms_[2]->id()+1, atoms_[3]->id()+1, value_);
			else msg.print("Torsion (%i-%i-%i-%i) = %f (literal = %f)\n", atoms_[0]->id()+1, atoms_[1]->id()+1, atoms_[2]->id()+1, atoms_[3]->id()+1, value_, literalValue_);
			break;
		default:
			printf("Measurement::print <<<< Unrecognised geometry type >>>>\n");
			break;
	}
}

