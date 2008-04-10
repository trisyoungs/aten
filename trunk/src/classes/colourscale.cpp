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
	left_ = 1e6;
	middle_ = 0.5;
	right_ = -1.0e6;
	range_ = 2e6;
	type_ = ColourScale::TwoPoint;
	setColour(ColourScale::LeftColour, 1.0, 1.0, 1.0, 1.0);
	setColour(ColourScale::MidColour, 0.5, 0.5, 0.5, 1.0);
	setColour(ColourScale::RightColour, 0.0, 0.0, 1.0, 1.0);
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

// Return leftmost value of scale
double ColourScale::left()
{
	return left_;
}

// Return rightmost value of scale
double ColourScale::right()
{
	return right_;
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
		deltaLeftRight_[n] = colours_[ColourScale::RightColour][n] - colours_[ColourScale::LeftColour][n];
		deltaLeftMid_[n] = colours_[ColourScale::MidColour][n] - colours_[ColourScale::LeftColour][n];
		deltaMidRight_[n] = colours_[ColourScale::RightColour][n] - colours_[ColourScale::MidColour][n];
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

// Set the absolute range of the colour scale
void ColourScale::setRange(double left, double right)
{
	left_ = left;
	right_ = right;
	middle_ = (right - left) * 0.5;
	range_ = right_ - left_;
	refreshObjects();
}

// Set the midpoint of the colour scale
void ColourScale::setMiddle(double middle)
{
	middle_ = middle;
}

// Adjust colour scale range to cover supplied value
void ColourScale::adjustRange(double d)
{
	if (d < left_) left_ = d;
	else if (d > right_) right_ = d;
	middle_ = (right_ - left_) * 0.5;
	range_ = right_ - left_;
}

// Return colour associated with value provided
void ColourScale::colour(double v, GLfloat *target)
{
	double delta;
	// Work out relative position of value 'v' on colour scale
	delta = (v - left_) / range_;
	// Clamp delta to the range [0,1]
	if (delta < 0) delta = 0;
	else if (delta > 1.0) delta = 1.0;
	if (type_ == ColourScale::TwoPoint)
	{
		target[0] = colours_[ColourScale::LeftColour][0] + deltaLeftRight_[0] * delta;
		target[1] = colours_[ColourScale::LeftColour][1] + deltaLeftRight_[1] * delta;
		target[2] = colours_[ColourScale::LeftColour][2] + deltaLeftRight_[2] * delta;
		target[3] = colours_[ColourScale::LeftColour][3] + deltaLeftRight_[3] * delta;
	}
	else if (delta < 0.5)
	{
		target[0] = colours_[ColourScale::LeftColour][0] + deltaLeftMid_[0] * delta;
		target[1] = colours_[ColourScale::LeftColour][1] + deltaLeftMid_[1] * delta;
		target[2] = colours_[ColourScale::LeftColour][2] + deltaLeftMid_[2] * delta;
		target[3] = colours_[ColourScale::LeftColour][3] + deltaLeftMid_[3] * delta;
	}
	else
	{
		delta -= 0.5;
		target[0] = colours_[ColourScale::MidColour][0] + deltaMidRight_[0] * delta;
		target[1] = colours_[ColourScale::MidColour][1] + deltaMidRight_[1] * delta;
		target[2] = colours_[ColourScale::MidColour][2] + deltaMidRight_[2] * delta;
		target[3] = colours_[ColourScale::MidColour][3] + deltaMidRight_[3] * delta;
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
