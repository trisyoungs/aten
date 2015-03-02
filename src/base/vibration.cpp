/*
	*** Vibrational mode definition
	*** src/base/vibration.cpp
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

#include "base/vibration.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Constructor
Vibration::Vibration(Model* parent) : ListItem<Vibration>()
{
	// Private variables
	parent_ = parent;
	frequency_ = 0.0;
	nDisplacements_ = 0;
	displacements_ = NULL;
}

// Destructor
Vibration::~Vibration()
{
	if (displacements_ != NULL) delete[] displacements_;
}

// Return parent model
Model* Vibration::parent() const
{
	return parent_;
}

// Initialise to specified size
void Vibration::initialise(Model* parent, int size)
{
	parent_ = parent;
	// Get number of atoms from parent model and create data array, unless a size was specified
	if (size != -1) nDisplacements_ = size;
	else
	{
		if (parent_ == NULL) printf("Warning - When initialising a Vibration without a specified size, the parent model must be valid.\n");
		else nDisplacements_ = parent_->nAtoms();
	}
	if (nDisplacements_ < 1) Messenger::print("Warning - No displacement data array created for vibration.\n");
	else displacements_ = new Vec3<double>[nDisplacements_];
}

// Set associated name
void Vibration::setName(const char* name)
{
	name_ = name;
}

// Return associated name of the vibration
const char* Vibration::name() const
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

// Set intensity of the vibration
void Vibration::setIntensity(double intensity)
{
	intensity_ = intensity;
}

// Return intensity of the vibration
double Vibration::intensity() const
{
	return intensity_;
}

// Set reduced mass of the vibration
void Vibration::setReducedMass(double rmass)
{
	reducedMass_ = rmass;
}

// Return reduced mass of the vibration
double Vibration::reducedMass() const
{
	return reducedMass_;
}

// Return number of displacements defined in vibration
int Vibration::nDisplacements()
{
	return nDisplacements_;
}

// Set specified displacement data (set of doubles)
void Vibration::setDisplacement(int n, double dx, double dy, double dz)
{
	if ((n < 0) || (n >= nDisplacements_)) Messenger::print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n].set(dx, dy, dz);
}

// Set specified displacement data (individual component)
void Vibration::setDisplacement(int n, int component, double d)
{
	if ((n < 0) || (n >= nDisplacements_)) Messenger::print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n].set(component, d);
}

// Set specified displacement data (vector)
void Vibration::setDisplacement(int n, Vec3<double> v)
{
	if ((n < 0) || (n >= nDisplacements_)) Messenger::print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else displacements_[n] = v;
}

// Return displacement array
Vec3<double> *Vibration::displacements()
{
	return displacements_;
}

// Return n'th displacement data
Vec3<double> Vibration::displacement(int n) const
{
	if ((n < 0) || (n >= nDisplacements_)) Messenger::print("Warning - Displacement index %i is out of range for vibration.\n", n);
	else return displacements_[n];
	return Vec3<double>();
}
