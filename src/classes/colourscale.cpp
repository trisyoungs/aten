/*
	*** Colour scale
	*** src/classes/colourscale.cpp
	Copyright T. Youngs 2007-2010

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
	interpolated_ = TRUE;
}

// Set the name of the colourscale
void ColourScale::setName(const char *s)
{
	name_ = s;
}

// Return the name of the colourscale
const char *ColourScale::name() const
{
	return name_.get();
}

// Set whether the colourscale is visible
void ColourScale::setVisible(bool v)
{
	visible_ = v;
}

// Return whether the colourscale is visible
bool ColourScale::visible() const
{
	return visible_;
}

// Set whether the colourscale is interpolated
void ColourScale::setInterpolated(bool b)
{
	interpolated_ = b;
}

// Return whether the colourscale is interpolated
bool ColourScale::interpolated() const
{
	return interpolated_;
}

// Recalculate colour deltas between points
void ColourScale::calculateDeltas()
{
	msg.enter("ColourScale::calculateDeltas");
	// Clear old list of deltas
	deltas_.clear();
	ColourScaleDelta *delta;
	for (ColourScalePoint *csp = points_.first(); csp != points_.last(); csp = csp->next)
	{
		delta = deltas_.add();
		delta->set(csp, csp->next);
	}
	msg.exit("ColourScale::calculateDeltas");
}

// Add point to scale
ColourScalePoint *ColourScale::addPoint(int position, double value, double r, double g, double b, double a)
{
	msg.enter("ColourScale::addPoint");
	// Check position supplied - if it is 0 add point at start. If npoints then add at end.
	if (position < 0) position = 0;
	else if (position > points_.nItems()) position = points_.nItems();
	ColourScalePoint *csp;
	if (position == 0) csp = points_.insert(NULL);
	else if (position == points_.nItems()) csp = points_.add();
	else csp = points_.insert( points_[position-1] );
	// Now, set data in new point
	csp->setParent(this);
	csp->setColour(r, g, b, a);
	csp->setValue(value);
	// Recalculate colour deltas
	calculateDeltas();
	// Refresh linked objects
	refreshObjects();
	msg.exit("ColourScale::addPoint");
	return csp;
}

// Add new point to end of colourscale
ColourScalePoint *ColourScale::addPointAtEnd(double value, double r, double g, double b, double a)
{
	return addPoint(points_.nItems(), value, r, g, b, a);
}

// Set colour and value data for point
void ColourScale::setPoint(int position, double value, double r, double g, double b, double a, bool setval, bool setcol)
{
	msg.enter("ColourScale::setPoint");
	// Check position supplied
	if ((position < 0) || (position >= points_.nItems()))
	{
		msg.print( "Scale point position to set (%i) is invalid - nItems = %i.\n", position, points_.nItems());
		msg.exit("ColourScale::setPoint");
		return;
	}
	if (setval) points_[position]->setValue(value);
	if (setcol) points_[position]->setColour(r, g, b, a);
	// Recalculate colour deltas
	calculateDeltas();
	// Refresh linked objects
	refreshObjects();
	msg.exit("ColourScale::setPoint");
}

// Set only value for point
void ColourScale::setPointValue(int position, double value)
{
	setPoint(position, value, 0.0f, 0.0f, 0.0f, 0.0f, TRUE, FALSE);
}

// Set only colour for point
void ColourScale::setPointColour(int position, double r, double g, double b, double a)
{
	setPoint(position, 0.0f, r, g, b, a, FALSE, TRUE);
}

// Remove old point from colourscale
void ColourScale::removePoint(int position)
{
	msg.enter("ColourScale::removePoint");
	// Check position supplied
	if ((position < 0) || (position >= points_.nItems()))
	{
		msg.print( "Scale point position to set (%i) is invalid - nItems = %i.\n", position, points_.nItems());
		msg.exit("ColourScale::removePoint");
		return;
	}
	points_.remove( points_[position] );
	// Recalculate colour deltas
	calculateDeltas();
	// Refresh linked objects
	refreshObjects();
	msg.exit("ColourScale::removePoint");
}

// Return colour associated with value provided
void ColourScale::colour(double v, GLfloat *target)
{
	// Step through points associated to scale and find the two that we are inbetween.
	// Check for no points being defined
	if (points_.nItems() == 0)
	{
		target[0] = (GLfloat) 0.0;
		target[1] = (GLfloat) 0.0;
		target[2] = (GLfloat) 0.0;
		target[3] = (GLfloat) 0.0;
		return;
	}
	ColourScalePoint *csp = points_.first();
	// Is supplied value less than the value at the first point?
	if (v < csp->value())
	{
		csp->copyColour(target);
		return;
	}
	// Find the correct delta to use
	for (ColourScaleDelta *delta = deltas_.first(); delta != NULL; delta = delta->next)
	{
		if (delta->containsValue(v))
		{
			delta->colour( interpolated_ ? v : delta->start(), target);
			return;
		}
	}
	// If we get to here then the supplied value is outside the range of all values, so take colour from the endpoint
	points_.last()->copyColour(target);
}

// Adjust range of scale to encompass point supplied
void ColourScale::adjustRange(double value)
{
	// TODO
}

// Return number of points in colourscale
int ColourScale::nPoints() const
{
	return points_.nItems();
}

// Return first point in colourscale
ColourScalePoint *ColourScale::firstPoint()
{
	return points_.first();
}

// Return last point in colourscale
ColourScalePoint *ColourScale::lastPoint()
{
	return points_.last();
}

// Return specific point in colourscale
ColourScalePoint *ColourScale::point(int id)
{
	return points_[id];
}

// Return first delta in colourscale
ColourScaleDelta *ColourScale::firstDelta()
{
	return deltas_.first();
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
	for (Refitem<Grid,int> *ri = grids_.first(); ri != NULL; ri = ri->next) if (ri->item->useColourScale()) ri->item->logChange();
}

// Return number of objects linked to this colourscale
int ColourScale::nLinks() const
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
