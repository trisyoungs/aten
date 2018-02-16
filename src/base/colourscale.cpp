/*
	*** Colour scale
	*** src/base/colourscale.cpp
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

#include "base/colourscale.h"
#include "base/grid.h"

ATEN_USING_NAMESPACE

// Constructor
ColourScale::ColourScale() : ListItem<ColourScale>()
{
	// Private variables
	visible_ = false;
	interpolated_ = true;
}

// Copy Constructor
ColourScale::ColourScale(const ColourScale& source)
{
	(*this) = source;
}

// Assignment Operator
ColourScale& ColourScale::operator=(const ColourScale& source)
{
	name_ = source.name_;
	interpolated_ = source.interpolated_;
	visible_ = source.visible_;

	// Copy points and deltas
	points_ = source.points_;
	calculateDeltas();

	// Copy reference objects
	grids_ = source.grids_;
 
	return *this;
}

// Equivalence Operator
bool ColourScale::operator==(const ColourScale& source)
{
	if (name_ != source.name_) return false;
	if (interpolated_ != source.interpolated_) return false;

	if (points_.nItems() != source.points_.nItems()) return false;
	ColourScalePoint* othercsp = source.points_.first();
	for (ColourScalePoint* csp = points_.first(); csp != NULL; csp = csp->next, othercsp = othercsp->next)
	{
		if ((*csp) != (*othercsp)) return false;
	}

	return true;
}

/*
 * General
 */

// Set the name of the colourscale
void ColourScale::setName(QString name)
{
	name_ = name;
}

// Return the name of the colourscale
QString ColourScale::name() const
{
	return name_;
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

/*
 * Points
 */

// Recalculate colour deltas between points
void ColourScale::calculateDeltas()
{
	Messenger::enter("ColourScale::calculateDeltas");

	// Clear old list of deltas
	deltas_.clear();
	ColourScaleDelta* delta;
	for (ColourScalePoint* csp = points_.first(); csp != points_.last(); csp = csp->next)
	{
		delta = deltas_.add();
		delta->set(csp, csp->next);
	}

	Messenger::exit("ColourScale::calculateDeltas");
}

// Add point to scale
ColourScalePoint* ColourScale::addPoint(double value, double r, double g, double b, double a)
{
	Messenger::enter("ColourScale::addPoint");

	// Find position to insert new point at
	ColourScalePoint* newPoint = NULL;
	if (points_.nItems() == 0) newPoint = points_.add();
	else if (value < points_.first()->value()) newPoint = points_.prepend();
	else if (value > points_.last()->value()) newPoint = points_.add();
	else
	{
		for (ColourScalePoint* point = points_.first()->next; point != NULL; point = point->next)
		{
			if (value < point->value())
			{
				newPoint = points_.insertBefore(point);
				break;
			}
		}
	}

	// Double-check we have a new point
	if (!newPoint)
	{
		printf("Internal Error: ColourScale::addPoint() failed to add a point.\n");
		return NULL;
	}

	// Now, set data in new point
	newPoint->setParent(this);
	newPoint->setColour(r, g, b, a);
	newPoint->setValue(value);

	// Recalculate colour deltas
	calculateDeltas();

	// Refresh linked objects
	refreshObjects();

	Messenger::exit("ColourScale::addPoint");
	return newPoint;
}

// Add new point to colourscale
ColourScalePoint* ColourScale::addPoint(double value, QColor colour)
{
	return addPoint(value, colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF());
}
	
// Set colour and value data for point
void ColourScale::setPoint(int position, double value, double r, double g, double b, double a, bool modifyValue, bool modifyColour)
{
	// Get ColourScalePoint for supplied position
	if ((position < 0) || (position >= points_.nItems()))
	{
		Messenger::print("Scale point position to set (%i) is invalid - nItems = %i.", position, points_.nItems());
		Messenger::exit("ColourScale::setPoint");
		return;
	}

	ColourScalePoint* point = points_[position];

	setPoint(point, value, r, g, b, a, modifyValue, modifyColour);
}

// Set colour and value data for point
void ColourScale::setPoint(ColourScalePoint* point, double value, double r, double g, double b, double a, bool modifyValue, bool modifyColour)
{
	Messenger::enter("ColourScale::setPoint");

	// Set value
	if (modifyValue)
	{
		// Set the new value, then adjust position of point as necessary
		point->setValue(value);
		// -- Shuffle up (towards head = lower values)
		while (point->prev)
		{
			if (value < point->prev->value()) points_.shiftUp(point);
			else break;
		}
		// -- Shuffle down (towards tail = higher values)
		while (point->next)
		{
			if (value > point->next->value()) points_.shiftDown(point);
			else break;
		}
	}

	// Set colour
	if (modifyColour) point->setColour(r, g, b, a);

	// Recalculate colour deltas
	calculateDeltas();

	// Refresh linked objects
	refreshObjects();

	Messenger::exit("ColourScale::setPoint");
}

// Set only value for point
void ColourScale::setValue(int position, double value)
{
	setPoint(position, value, 0.0f, 0.0f, 0.0f, 0.0f, true, false);
}

// Set value for point
void ColourScale::setValue(ColourScalePoint* point, double value)
{
	int position = points_.indexOf(point);
	if (position != -1) setPoint(position, value, 0.0f, 0.0f, 0.0f, 0.0f, true, false);
}

// Set colour for point
void ColourScale::setColour(int position, double r, double g, double b, double a)
{
	setPoint(position, 0.0f, r, g, b, a, false, true);
}

// Set colour for point
void ColourScale::setColour(ColourScalePoint* point, QColor colour)
{
	int position = points_.indexOf(point);
	if (position != -1) setPoint(position, 0.0, colour.redF(), colour.greenF(), colour.blueF(), colour.alphaF(), false, true);
}

// Remove old point from colourscale
void ColourScale::removePoint(int position)
{
	Messenger::enter("ColourScale::removePoint");

	// Check position supplied
	if ((position < 0) || (position >= points_.nItems()))
	{
		Messenger::print("Scale point position to set (%i) is invalid - nItems = %i.", position, points_.nItems());
		Messenger::exit("ColourScale::removePoint");
		return;
	}
	points_.remove( points_[position] );

	// Recalculate colour deltas
	calculateDeltas();

	// Refresh linked objects
	refreshObjects();

	Messenger::exit("ColourScale::removePoint");
}

// Remove point from colourscale
void ColourScale::removePoint(ColourScalePoint* point)
{
	int position = points_.indexOf(point);
	if (position != -1) removePoint(position);
}

// Return colour associated with value provided
void ColourScale::colour(double v, GLfloat* target)
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
	ColourScalePoint* csp = points_.first();

	// Is supplied value less than the value at the first point?
	if (v < csp->value())
	{
		csp->copyColour(target);
		return;
	}

	// Find the correct delta to use
	for (ColourScaleDelta* delta = deltas_.first(); delta != NULL; delta = delta->next)
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

// Get colour associated with value supplied
void ColourScale::colour(double value, Vec4<GLfloat>& target)
{
	static GLfloat col[4];
	colour(value, col);
	target.set(col[0], col[1], col[2], col[3]);
}

// Get colour as QColor
QColor ColourScale::colourAsQColor(double value)
{
	static GLfloat col[4];
	colour(value, col);
	QColor qcol;
	qcol.setRgbF(col[0], col[1], col[2], col[3]);
	return qcol;
}

// Adjust range of scale to encompass point supplied
void ColourScale::adjustRange(double value)
{
	if (points_.nItems() == 0) addPoint(value, 1.0, 0.0, 0.0, 1.0);
	else if (value < points_.first()->value()) setValue(0, value);
	else if (value > points_.last()->value())
	{
		// If there is only one point, add another one. If not, adjust the last point in the scale
		if (points_.nItems() == 1) addPoint(value, 0.0, 0.0, 1.0, 1.0);
		else setValue(points_.nItems()-1, value);
	}
}

// Return number of points in colourscale
int ColourScale::nPoints() const
{
	return points_.nItems();
}

// Return first point in colourscale
ColourScalePoint* ColourScale::firstPoint()
{
	return points_.first();
}

// Return last point in colourscale
ColourScalePoint* ColourScale::lastPoint()
{
	return points_.last();
}

// Return specific point in colourscale
ColourScalePoint* ColourScale::point(int id)
{
	return points_[id];
}

// Return first delta in colourscale
ColourScaleDelta* ColourScale::firstDelta()
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
	for (RefListItem<Grid,int>* ri = grids_.first(); ri != NULL; ri = ri->next) if (ri->item->useColourScale()) ri->item->logChange();
}

// Return number of objects linked to this colourscale
int ColourScale::nLinks() const
{
	return grids_.nItems();
}

// Link grid with colourscale
void ColourScale::addLink(Grid* g)
{
	// Add the grid to the reflist (if it isn't there already
	grids_.addUnique(g);
}

// Break link between grid and colourscale
void ColourScale::breakLink(Grid* g)
{
	// Add the grid to the reflist (if it isn't there already
	grids_.remove(g);
}
