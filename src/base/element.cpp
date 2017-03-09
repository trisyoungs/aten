/*
	*** Element Definition
	*** src/base/element.cpp
	Copyright T. Youngs 2007-2017

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

#include "base/element.h"

ATEN_USING_NAMESPACE

// Set colour component of element
void Element::setColour(int rgb, double value)
{
	colour[rgb] = value;
}

// Set colour component
void Element::setColour(double r, double g, double b, double a)
{
	colour[0] = r;
	colour[1] = g;
	colour[2] = b;
	colour[3] = a;
}

// Copy the ambient colour of the element into the GLfloat array provided
void Element::copyColour(GLfloat* v) const
{
	v[0] = (GLfloat) colour[0];
	v[1] = (GLfloat) colour[1];
	v[2] = (GLfloat) colour[2];
	v[3] = (GLfloat) colour[3];
}

// Copy the ambient colour of the element into the double array provided
void Element::copyColour(double* v) const
{
	v[0] = colour[0];
	v[1] = colour[1];
	v[2] = colour[2];
	v[3] = colour[3];
}

// Copy the ambient colour of the element into the Vec4 provided
void Element::copyColour(Vec4<GLfloat>& v) const
{
	v.x = (GLfloat) colour[0];
	v.y = (GLfloat) colour[1];
	v.z = (GLfloat) colour[2];
	v.w = (GLfloat) colour[3];
}
