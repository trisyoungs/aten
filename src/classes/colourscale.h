/*
	*** Colour scale
	*** src/classes/colourscale.h
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

#ifndef ATEN_COLOURSCALE_H
#define ATEN_COLOURSCALE_H

// Fix Windows.h
#define NOMINMAX

#include <QtOpenGL/QtOpenGL>
#include "templates/list.h"
#include "templates/reflist.h"
#include "classes/dnchar.h"
#include "classes/colourscalepoint.h"

// Forward declarations
class Grid;

// Colour Scale
class ColourScale
{
	public:
	// Constructor
	ColourScale();

	/*
	// Rendering
	*/
	private:
	// Name of the colourscale
	Dnchar name_;
	// Whether the colourscale is currently visible
	bool visible_;

	public:
	// Set the name of the colourscale
	void setName(const char *s);
	// Return the name of the colourscale
	const char *name();
	// Set whether the colourscale is visible
	void setVisible(bool v);
	// Return whether the colourscale is visible
	bool visible();

	/*
	// Data and data range
	*/
	private:
	// List of points in the colourscale
	List<ColourScalePoint> points_;
	// List of colour deltas between points in the colourscale
	List<ColourScaleDelta> deltas_;
	// Calculate colour deltas for current list of points
	void calculateDeltas();

	public:
	// Return number of points in colourscale
	int nPoints();
	// Return first point in colourscale
	ColourScalePoint *points();
	// Return last point in colourscale
	ColourScalePoint *lastPoint();
	// Return specific point in colourscale
	ColourScalePoint *point(int id);
	// Clear all points in colourscale
	void clear();
	// Add new point to colourscale
	void addPoint(int position, double value, GLfloat r, GLfloat g, GLfloat b, GLfloat a = 1.0f);
	// Set colour and value data for point
	void setPoint(int position, double value, GLfloat r, GLfloat g, GLfloat b, GLfloat a = 1.0f);
	// Remove old point from colourscale
	void removePoint(int position);
	// Get colour associated with value supplied
	void colour(double value, GLfloat *target);
	// Adjust range of scale to encompass point supplied
	void adjustRange(double value);

	/*
	// Linked objects
	*/
	private:
	// Grids that use the colour scale
	Reflist<Grid,int> grids_;
	// Refresh all linked objects
	void refreshObjects();

	public:
	// Return number of objects linked to this colourscale
	int nLinks();
	// Link grid with colourscale
	void addLink(Grid *g);
	// Break link between grid and colourscale
	void breakLink(Grid *g);
};

#endif
