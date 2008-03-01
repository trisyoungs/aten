/*
	*** Spacegroup generator
	*** src/classes/generator.h
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

#ifndef H_GENERATOR_H
#define H_GENERATOR_H

#include "templates/vector3.h"

// Symmetry generator
class generator
{
	public:
	// Destructor
	~generator();

	/*
	// Rotation Matrix and Translation Vector
	*/
	public:
	// Short text 'description'
	const char *description;
	// Rotation matrix
	mat3<double> rotation;
	// Translation vector
	vec3<double> translation;
};

#endif
