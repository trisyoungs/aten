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
ColourScale::ColourScale()
{
	// Private variables
	minimum_ = 0.0;
	middle_ = 0.5;
	maximum_ = 1.0;
	range_ = 1.0;
	visible_ = FALSE;
	setColour(ColourScale::MinColour, 1.0f, 1.0f, 1.0f, 1.0f);
	setColour(ColourScale::MidColour, 0.5f, 0.5f, 0.5f, 1.0f);
	setColour(ColourScale::MaxColour, 0.0f, 0.0f, 1.0f, 1.0f);
}

// Set the name of the colourscale
void ColourScale::setName(const char *s)
{
	name_ = s;
}

// Return the name of the colourscale
const char *ColourScale::name()
{
	return name_.get();
}

// Set whether the colourscale is visible
void ColourScale::setVisible(bool v)
{
	visible_ = v;
}

// Return whether the colourscale is visible
bool ColourScale::visible()
{
	return visible_;
}

// Set type of ColourScale
void ColourScale::setType(ColourScale::ScaleOrder so)
{
	type_ = so;
}

// Return type of colourscale
ColourScale::ScaleOrder ColourScale::type()
{
	return type_;
}

// Return minimum value of scale
double ColourScale::minimum()
{
	return minimum_;
}

// Return maximum value of scale
double ColourScale::maximum()
{
	return maximum_;
}

// Return midpoint value of scale
double ColourScale::middle()
{
	return middle_;
}

// Return range of scale
double ColourScale::range()
{
	return range_;
}

// Set colour
void ColourScale::setColour(ScaleColour col, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	int n;
	colours_[col][0] = r;
	colours_[col][1] = g;
	colours_[col][2] = b;
	colours_[col][3] = a;
	// Recalculate colour deltas
	for (n=0; n<4; n++)
	{
		deltaMinMax_[n] = colours_[ColourScale::MaxColour][n] - colours_[ColourScale::MinColour][n];
		deltaMinMid_[n] = colours_[ColourScale::MidColour][n] - colours_[ColourScale::MinColour][n];
		deltaMidMax_[n] = colours_[ColourScale::MaxColour][n] - colours_[ColourScale::MidColour][n];
	}
	// Refresh linked objects
	refreshObjects();
}

// Copy colour
void ColourScale::copyColour(ScaleColour col, GLfloat *target)
{
	target[0] = colours_[col][0];
	target[1] = colours_[col][1];
	target[2] = colours_[col][2];
	target[3] = colours_[col][3];
}

// Set the absolute range of the colour scale (but not the middle value)
void ColourScale::setRange(double min, double max)
{
	minimum_ = min;
	maximum_ = max;
	// Make sure the midpoint is within the new bounds
	if (middle_ < minimum_) middle_ = minimum_;
	else if (middle_ > maximum_) middle_ = maximum_;
	range_ = maximum_ - minimum_;
	refreshObjects();
}

// Set the midpoint of the colour scale
void ColourScale::setMiddle(double middle)
{
	middle_ = middle;
	// Make sure the midpoint is within the new bounds
	if (middle_ < minimum_) middle_ = minimum_;
	else if (middle_ > maximum_) middle_ = maximum_;
	refreshObjects();
}

// Adjust colour scale range to cover supplied value
void ColourScale::adjustRange(double d)
{
	if (d < minimum_) minimum_ = d;
	else if (d > maximum_) maximum_ = d;
	middle_ = (maximum_ - minimum_) * 0.5;
	range_ = maximum_ - minimum_;
}

// Return colour associated with value provided
void ColourScale::colour(double v, GLfloat *target)
{
	double delta;
	if (type_ == ColourScale::TwoPoint)
	{
		delta = (v - minimum_) / range_;
		// Clamp delta to the range [0,1]
		if (delta < 0.0) delta = 0.0;
		else if (delta > 1.0) delta = 1.0;
		target[0] = colours_[ColourScale::MinColour][0] + deltaMinMax_[0] * delta;
		target[1] = colours_[ColourScale::MinColour][1] + deltaMinMax_[1] * delta;
		target[2] = colours_[ColourScale::MinColour][2] + deltaMinMax_[2] * delta;
		target[3] = colours_[ColourScale::MinColour][3] + deltaMinMax_[3] * delta;
	}
	else if (v < middle_)
	{
		delta = (v - minimum_) / (middle_ - minimum_);
		if (delta < 0.0) delta = 0.0;
		else if (delta > 1.0) delta = 1.0;
		target[0] = colours_[ColourScale::MinColour][0] + deltaMinMid_[0] * delta;
		target[1] = colours_[ColourScale::MinColour][1] + deltaMinMid_[1] * delta;
		target[2] = colours_[ColourScale::MinColour][2] + deltaMinMid_[2] * delta;
		target[3] = colours_[ColourScale::MinColour][3] + deltaMinMid_[3] * delta;
	}
	else
	{
		delta = (v - middle_) / (maximum_ - middle_);
		if (delta < 0.0) delta = 0.0;
		else if (delta > 1.0) delta = 1.0;
		target[0] = colours_[ColourScale::MidColour][0] + deltaMidMax_[0] * delta;
		target[1] = colours_[ColourScale::MidColour][1] + deltaMidMax_[1] * delta;
		target[2] = colours_[ColourScale::MidColour][2] + deltaMidMax_[2] * delta;
		target[3] = colours_[ColourScale::MidColour][3] + deltaMidMax_[3] * delta;
	}
}

// Refresh all linked objects
void ColourScale::refreshObjects()
{
	// Go through lists of linked objects and poke their logs...
	for (Refitem<Grid,int> *ri = grids_.first(); ri != NULL; ri = ri->next) ri->item->logChange();
}

// Link grid with colourscale
void ColourScale::addLink(Grid *g)
{
	// Add the grid to the reflist (if it isn't there already
	grids_.addUnique(g);
}

// Break link between grid and colourscale
void ColourScale::breakLink(Grid *g)
{
	// Add the grid to the reflist (if it isn't there already
	grids_.remove(g);
}
