/*
	*** Colour scale
	*** src/classes/colourscale.cpp
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

#include "classes/colourscale.h"
#include "classes/grid.h"

// Constructor
ColourScalePoint::ColourScalePoint()
{
	// Private variables
	value_ = 0.0;
	setColour(1.0f, 1.0f, 1.0f, 1.0f);

	// Public variables
	prev = NULL;
	next = NULL;
}

// Return value of scale point
double ColourScalePoint::value()
{
	return value_;
}

// Set value of scale point
void ColourScalePoint::setValue(double d)
{
	value_ = d;
}

// Set colour
void ColourScalePoint::setColour(GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	colour_[0] = r;
	colour_[1] = g;
	colour_[2] = b;
	colour_[3] = a;
}

// Copy colour
void ColourScalePoint::copyColour(GLfloat *target)
{
	target[0] = colour_[0];
	target[1] = colour_[1];
	target[2] = colour_[2];
	target[3] = colour_[3];
}

// Return pointer to colour array
GLfloat *ColourScalePoint::colour()
{
	return colour_;
}

/*
// Colour scale delta
*/

// Constructor
ColourScaleDelta::ColourScaleDelta()
{
	// Private variables
	startValue_ = 0.0;
	deltaValue_ = 0.0;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Check whether the delta 'contains' the supplied value
bool ColourScaleDelta::containsValue(double d)
{
	if (d < startValue_) return FALSE;
	if (d > (startValue_ + deltaValue_)) return FALSE;
	return TRUE;
}

// Create delta from two existing colours
void ColourScaleDelta::set(ColourScalePoint *point1, ColourScalePoint *point2)
{
	// Copy first colour point
	startValue_ = point1->value_;
	startColour_[0] = point1->colour_[0];
	startColour_[1] = point1->colour_[1];
	startColour_[2] = point1->colour_[2];
	startColour_[3] = point1->colour_[3];
	deltaColour_[0] = point2->colour_[0] - startColour_[0];
	deltaColour_[1] = point2->colour_[1] - startColour_[1];
	deltaColour_[2] = point2->colour_[2] - startColour_[2];
	deltaColour_[3] = point2->colour_[3] - startColour_[3];
	deltaValue_ = point2->value_ - startValue_;
}

// Get colour for value v
void ColourScaleDelta::getColour(double v, GLfloat *target)
{
	// Clamp 'v' to range 0.0 - 1.0 to span range of delta
	double clampv = (v - startValue_) / deltaValue_;
	if (clampv < 0.0) clampv = 0.0;
	else if (clampv > 1.0) clampv = 1.0;
	target[0] = startColour_[0] + deltaColour_[0] * clampv;
	target[1] = startColour_[1] + deltaColour_[1] * clampv;
	target[2] = startColour_[2] + deltaColour_[2] * clampv;
	target[3] = startColour_[3] + deltaColour_[3] * clampv;
}

