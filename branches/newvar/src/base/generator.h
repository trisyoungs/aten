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
#include "templates/list.h"

// Symmetry generator
class Generator
{
	/*
	// Rotation Matrix and Translation Vector
	*/
	public:
	// Integer id
	int id;
	// Short text 'description'
	const char *name;
	// Rotation matrix
	Mat3<double> rotation;
	// Translation vector
	Vec3<double> translation;
	// List pointers
	Generator *prev, *next;

	private:
	// Set partial element of matrix or translation vector
	bool set(int row, const char *s);

	public:
	// Set from plain text string
	bool set(const char *s);
};

// Symmetry generator map
class GeneratorMap
{
	private:
	// Spacegroup generator data
	static Generator generators_[];
	// Extra, manually-defined generators
	List<Generator> extra_;

	public:
	// Return generator with ID specified
	Generator &generator(int gen) const;
	// Find generator from supplied string
	Generator *generator(const char *s);
	// Add new generator to list of extra definitions
	Generator *addGenerator(const char *s);
};

extern GeneratorMap generators;

#endif
