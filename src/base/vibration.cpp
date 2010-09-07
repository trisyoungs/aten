/*
	*** Vibrational mode definition
	*** src/base/vibration.cpp
	Copyright T. Youngs 2007-2010

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

#include "base/vibration.h"

// Constructor
Vibration::Vibration(int natoms, Model *parent)
{
	// Private variables
	parent_ = parent;
	frequency_ = 0.0;
	displacements_ = NULL;
	nDisplacements_ = natoms;

	// Public variables
	prev = NULL;
	next = NULL;

	// Create array for displacement data
	if (nDisplacements_ < 1) printf("Warning - Displacement data array for vibration not created. An invalid number of atoms was passed (%i).\n", nDisplacements_);
	else displacements_ = new Vec3<double>[nDisplacements_];
}


// Destructor
Vibration::~Vibration()
{
	if (displacements_ != NULL) delete[] displacements_;
}

// Return parent model
Model *Vibration::parent() const
{
	return parent_;
}

// Set associated name
void Vibration::setName(const char *name)
{
	name_ = name;
}

// Return associated name of the vibration
const char *Vibration::name() const
{
	return name_.get();
}

// Set frequency of the vibration
void Vibration::setFrequency(double freq)
{
	frequency_ = freq;
}

// Return frequency of the vibration
double Vibration::frequency() const
{
	return frequency_;
}

// Return number of displacements defined in vibration
int Vibration::nDisplacements()
{
	return nDisplacements_;
}

// Set specified displacement data (set of doubles)
void Vibration::setDisplacement(int n, double dx, double dy, double dz)
{
	if ((n < 0) || (n >= nDisplacements_)) msg.print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n].set(dx, dy, dz);
}

// Set specified displacement data (individual component)
void Vibration::setDisplacement(int n, int component, double d)
{
	if ((n < 0) || (n >= nDisplacements_)) msg.print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n].set(component, d);
}

// Set specified displacement data (vector)
void Vibration::setDisplacement(int n, Vec3<double> &v)
{
	if ((n < 0) || (n >= nDisplacements_)) msg.print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n] = v;
}

// Return n'th displacement data
Vec3<double> &Vibration::displacement(int n) const
{
	static Vec3<double> dummyVec;
	if ((n < 0) || (n >= nDisplacements_)) msg.print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else return displacements_[n];
	return dummyVec;
}

