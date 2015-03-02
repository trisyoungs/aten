/*
	*** Vibrational mode definition
	*** src/base/vibration.h
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

#ifndef ATEN_VIBRATION_H
#define ATEN_VIBRATION_H

#include "base/dnchar.h"
#include "templates/list.h"
#include "templates/vector3.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;

// Vibrational mode
class Vibration : public ListItem<Vibration>
{
	public:
	// Constructor / Destructor
	Vibration(Model* parent = NULL);
	~Vibration();
	

	/*
	// Data
	*/
	private:
	// Parent model
	Model* parent_;
	// Name of the vibration
	Dnchar name_;
	// Frequency of the vibration
	double frequency_;
	// Intensity of the vibration
	double intensity_;
	// Reduced mass of the vibration
	double reducedMass_;
	// Number of displacements defined in vibration
	int nDisplacements_;
	// Displacement vectors for each associated atom
	Vec3<double> *displacements_;

	public:
	// Return parent model
	Model* parent() const;
	// Initialise to specified size
	void initialise(Model* parent, int size = -1);
	// Set associated name
	void setName(const char* name);
	// Return associated name of the vibration
	const char* name() const;
	// Set frequency of the vibration
	void setFrequency(double freq);
	// Return frequency of the vibration
	double frequency() const;
	// Set intensity of the vibration
	void setIntensity(double freq);
	// Return intensity of the vibration
	double intensity() const;
	// Set reduced mass of the vibration
	void setReducedMass(double freq);
	// Return reduced mass of the vibration
	double reducedMass() const;
	// Return number of displacements defined in vibration
	int nDisplacements();
	// Set specified displacement data (set of doubles)
	void setDisplacement(int n, double dx, double dy, double dz);
	// Set specified displacement data (individual component)
	void setDisplacement(int n, int component, double d);
	// Set specified displacement data (vector)
	void setDisplacement(int n, Vec3<double> v);
	// Return displacements array
	Vec3<double> *displacements();
	// Return n'th displacement data
	Vec3<double> displacement(int n) const;
};

ATEN_END_NAMESPACE

#endif
