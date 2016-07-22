/*
	*** Colour scale
	*** src/base/colourscale.cpp
	Copyright T. Youngs 2007-2016

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

#include "base/colourscale.h"
#include "base/grid.h"

ATEN_USING_NAMESPACE

// Constructor
ColourScalePoint::ColourScalePoint() : ListItem<ColourScalePoint>()
{
	// Private variables
	value_ = 0.0;
	setColour(1.0, 1.0, 1.0, 1.0);
	parent_ = NULL;
}

// Operator!=
bool ColourScalePoint::operator!=(const ColourScalePoint& other)
{
	if (fabs(value_-other.value_) > 1.0e-6) return true;
	if (fabs(colour_[0]-other.colour_[0]) > 1.0e-6) return true;
	if (fabs(colour_[1]-other.colour_[1]) > 1.0e-6) return true;
	if (fabs(colour_[2]-other.colour_[2]) > 1.0e-6) return true;
	if (fabs(colour_[3]-other.colour_[3]) > 1.0e-6) return true;

	return false;
}

// Set parent colourscale
void ColourScalePoint::setParent(ColourScale* cscale)
{
	parent_ = cscale;
}

// Return parent colourscale
ColourScale* ColourScalePoint::parent()
{
	return parent_;
}

// Return value of scale point
double ColourScalePoint::value() const
{
	return value_;
}

// Set value of scale point
void ColourScalePoint::setValue(double d)
{
	value_ = d;
}

// Set colour
void ColourScalePoint::setColour(double r, double g, double b, double a)
{
	colour_[0] = r;
	colour_[1] = g;
	colour_[2] = b;
	colour_[3] = a;
}

// Copy colour
void ColourScalePoint::copyColour(GLfloat* target) const
{
	target[0] = (GLfloat) colour_[0];
	target[1] = (GLfloat) colour_[1];
	target[2] = (GLfloat) colour_[2];
	target[3] = (GLfloat) colour_[3];
}

// Return pointer to colour array
double* ColourScalePoint::colour()
{
	return colour_;
}

// Return colour as QColor
QColor ColourScalePoint::colourAsQColor()
{
	QColor color;
	color.setRgbF(colour_[0], colour_[1], colour_[2], colour_[3]);
	return color;
}

/*
 * Colour scale delta
 */

// Constructor
ColourScaleDelta::ColourScaleDelta() : ListItem<ColourScaleDelta>()
{
	// Private variables
	start_ = 0.0;
	delta_ = 0.0;
}

// Check whether the delta 'contains' the supplied value
bool ColourScaleDelta::containsValue(double d) const
{
	if (d < start_) return false;
	if (d > (start_ + delta_)) return false;
	return true;
}

// Create delta from two existing colours
void ColourScaleDelta::set(ColourScalePoint* point1, ColourScalePoint* point2)
{
	// Copy first colour point
	start_ = point1->value_;
	startColour_[0] = point1->colour_[0];
	startColour_[1] = point1->colour_[1];
	startColour_[2] = point1->colour_[2];
	startColour_[3] = point1->colour_[3];
	deltaColour_[0] = point2->colour_[0] - startColour_[0];
	deltaColour_[1] = point2->colour_[1] - startColour_[1];
	deltaColour_[2] = point2->colour_[2] - startColour_[2];
	deltaColour_[3] = point2->colour_[3] - startColour_[3];
	delta_ = point2->value_ - start_;
}

// Get colour for value v
void ColourScaleDelta::colour(double v, GLfloat* target) const
{
	// Clamp 'v' to range 0.0 - 1.0 to span range of delta
	double clampv = (v - start_) / delta_;
	if (clampv < 0.0) clampv = 0.0;
	else if (clampv > 1.0) clampv = 1.0;
	target[0] = (GLfloat) startColour_[0] + deltaColour_[0] * clampv;
	target[1] = (GLfloat) startColour_[1] + deltaColour_[1] * clampv;
	target[2] = (GLfloat) startColour_[2] + deltaColour_[2] * clampv;
	target[3] = (GLfloat) startColour_[3] + deltaColour_[3] * clampv;
}

// Return the starting value of the range
double ColourScaleDelta::start() const
{
	return start_;
}

// Return the range of the delta
double ColourScaleDelta::delta() const
{
	return delta_;
}
