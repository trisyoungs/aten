/*
	*** Spacegroup generators
	*** src/base/generator.h
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

#ifndef ATEN_GENERATOR_H
#define ATEN_GENERATOR_H

#include "templates/vector3.h"

// Symmetry generator
class Generator
{
	/*
	// Rotation Matrix and Translation Vector
	*/
	public:
	// Short text 'description'
	const char *description;
	// Rotation matrix
	Mat3<double> rotation;
	// Translation vector
	Vec3<double> translation;
};

// Symmetry generator map
class GeneratorMap
{
	private:
	// Spacegroup generator data
	static Generator generators_[];

	public:
	// Return generator with ID specified
	Generator &generator(int gen) const;
};

extern GeneratorMap generators;

#endif
