/*
	*** Element Definition
	*** src/base/element.h
	Copyright T. Youngs 2007-2018

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

#ifndef ATEN_ELEMENT_H
#define ATEN_ELEMENT_H

#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "templates/vector4.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
/* none */

// Element
class Element
{
	public:
	// Z
	int z;
	// Mass of element
	double atomicMass;
	// Element name
	const char* name;
	// Uppercase element name
	const char* ucName;
	// Element symbol
	const char* symbol;
	// Uppercase Element symbol
	const char* ucSymbol;
	// Group position in periodic table
	int group;
	// Rough elemental radius (for bond calculation etc.)
	double atomicRadius;
	// Element colour
	double colour[4];
	// Numeric measure of 'penalties' for total bond orders 0 - 8
	int bondOrderPenalty[9];
	// Formal charges for bond orders 0 - 8
	int formalCharges[9];


	/*
	 * Data by Z
	 */
	public:
	// Set ambient colour component of element
	void setColour(int rgb, double value);
	// Set ambient colour of element
	void setColour(double r, double g, double b, double a);
	// Copy the ambient colour of the element into the GLfloat array provided
	void copyColour(GLfloat* v) const;
	// Copy the ambient colour of the element into the double array provided
	void copyColour(double* v) const;
	// Copy the ambient colour of the element into the Vec4 provided
	void copyColour(Vec4<GLfloat>& v) const;
};

ATEN_END_NAMESPACE

#endif
