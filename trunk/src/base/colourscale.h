/*
	*** Colour scale
	*** src/base/colourscale.h
	Copyright T. Youngs 2007-2015

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

#include "templates/list.h"
#include "templates/reflist.h"
#include "templates/vector4.h"
#include "base/colourscalepoint.h"
#include "base/namespace.h"
#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Grid;

// Colour Scale
class ColourScale : public ListItem<ColourScale>
{
	public:
	// Constructor
	ColourScale();


	/*
	// Rendering
	*/
	private:
	// Name of the colourscale
	QString name_;
	// Whether the colourscale is currently visible
	bool visible_;
	// Whether colours are interpolated between points in the scale
	bool interpolated_;

	public:
	// Set the name of the colourscale
	void setName(QString name);
	// Return the name of the colourscale
	QString name() const;
	// Set whether the colourscale is visible
	void setVisible(bool b);
	// Return whether the colourscale is visible
	bool visible() const;
	// Set whether the colourscale is interpolated
	void setInterpolated(bool b);
	// Return whether the colourscale is interpolated
	bool interpolated() const;


	/*
	// Data and data range
	*/
	private:
	// List of points in the colourscale
	List<ColourScalePoint> points_;
	// List of colour deltas between points in the colourscale
	List<ColourScaleDelta> deltas_;

	public:
	// Return number of points in colourscale
	int nPoints() const;
	// Return first point in colourscale
	ColourScalePoint* firstPoint();
	// Return last point in colourscale
	ColourScalePoint* lastPoint();
	// Return specific point in colourscale
	ColourScalePoint* point(int id);
	// Return first delta in colourscale
	ColourScaleDelta* firstDelta();
	// Clear all points in colourscale
	void clear();
	// Calculate colour deltas for current list of points
	void calculateDeltas();
	// Add new point to colourscale
	ColourScalePoint* addPoint(int position, double value, double r, double g, double b, double a = 1.0f);
	// Add new point to end of colourscale
	ColourScalePoint* addPointAtEnd(double value, double r, double g, double b, double a = 1.0f);
	// Set colour and value data for point
	void setPoint(int position, double value, double r, double g, double b, double a = 1.0f, bool setval = true, bool setcol = true);
	// Set only value for point
	void setPointValue(int position, double value);
	// Set only colour for point
	void setPointColour(int position, double r, double g, double b, double a = 1.0f);
	// Remove old point from colourscale
	void removePoint(int position);
	// Get colour associated with value supplied
	void colour(double value, GLfloat* target);
	// Get colour associated with value supplied
	void colour(double value, Vec4<GLfloat>& target);
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
	int nLinks() const;
	// Link grid with colourscale
	void addLink(Grid* g);
	// Break link between grid and colourscale
	void breakLink(Grid* g);
};

ATEN_END_NAMESPACE

#endif
