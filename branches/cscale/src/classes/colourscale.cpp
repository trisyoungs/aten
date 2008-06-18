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
	visible_ = FALSE;
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

// Recalculate colour deltas between points
void ColourScale::calculateDeltas()
{
	dbgBegin(Debug::Calls,"ColourScale::calculateDeltas");
	// Clear old list of deltas
	deltas_.clear();
	ColourScaleDelta *delta;
	for (ColourScalePoint *csp = points_.first(); csp != points_.last(); csp = csp->next)
	{
		delta = deltas_.add();
		delta->set(csp, csp->next);
	}
	dbgEnd(Debug::Calls,"ColourScale::calculateDeltas");
}

// Add point to scale
void ColourScale::addPoint(int position, double value, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	dbgBegin(Debug::Calls,"ColourScale::addPoint");
	// Check position supplied - if it is 0 add point at start. If npoints then add at end.
	if ((position < 0) || (position > points_.nItems()))
	{
		msg(Debug::None, "Position at which to add scale point (%i) is invalid - nItems = %i.\n", position, points_.nItems());
		dbgEnd(Debug::Calls,"ColourScale::addPoint");
		return;
	}
	ColourScalePoint *csp;
	if (position == 0) csp = points_.insert(NULL);
	else if (position == points_.nItems()) csp = points_.add();
	else csp = points_.insert( points_[position-1] );
	// Now, set data in new point
	csp->setColour(r, g, b, a);
	csp->setValue(value);
	// Recalculate colour deltas
	calculateDeltas();
	// Refresh linked objects
	refreshObjects();
	dbgEnd(Debug::Calls,"ColourScale::addPoint");
}


// Return colour associated with value provided
void ColourScale::colour(double v, GLfloat *target)
{
	// Step through points associated to scale and find the two that we are inbetween.
	// Check for no points being defined
	if (points_.nItems() == 0)
	{
		target[0] = 0.0f;
		target[1] = 0.0f;
		target[2] = 0.0f;
		target[3] = 0.0f;
		return;
	}
	// Is supplied value less than the value at the first point?
	ColourScalePoint *first;
	first = points_.first();
	if (v < first->value())
	{
		first->copyColour(target);
		return;
	}
	// Find the correct delta to use
	ColourScaleDelta *delta;
	for (delta = deltas_.first(); delta != NULL; delta = delta->next)
	{
		if (delta->containsValue(v))
		{
			delta->getColour(v, target);
			return;
		}
	}
	// If we get to here then the supplied value is outside the range of all values, so take colour from the endpoint
	points_.last()->copyColour(target);
}

// Adjust range of scale to encompass point supplied
void ColourScale::adjustRange(double value)
{
}

// Return number of points in colourscale
int ColourScale::nPoints()
{
	return points_.nItems();
}

// Return first point in colourscale
ColourScalePoint *ColourScale::points()
{
	return points_.first();
}

// Clear all points in colourscale
void ColourScale::clear()
{
	points_.clear();
}

// Refresh all linked objects
void ColourScale::refreshObjects()
{
	// Go through lists of linked objects and poke their logs...
	for (Refitem<Grid,int> *ri = grids_.first(); ri != NULL; ri = ri->next) ri->item->logChange();
}

// Return number of objects linked to this colourscale
int ColourScale::nLinks()
{
	return grids_.nItems();
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
