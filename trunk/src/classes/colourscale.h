/*
	*** Colour scale
	*** src/classes/ColourScale.h
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
#include "templates/reflist.h"

// Forward declarations
class Grid;

// Colour Scale
class ColourScale
{
	public:
	// Constructor
	ColourScale();
	// Colourscale order
	enum ScaleOrder { TwoPoint, ThreePoint };
	// Colourscale colours
	enum ScaleColour { LeftColour, MidColour, RightColour, nScaleColours };

	/*
	// Data and data range
	*/
	private:
	// Type of ColourScale
	ScaleOrder type_;
	// Left, right, and middle of data range
	double left_, right_, middle_;
	// Range of data
	double range_;

	public:
	// Set type of ColourScale
	void setType(ScaleOrder co);
	// Return type of colourscale
	ScaleOrder type();
	// Set the absolute range of the colour scale
	void setRange(double left, double right);
	// Set the midpoint of the colour scale
	void setMiddle(double middle);
	// Adjust colour scale range to cover supplied value
	void adjustRange(double d);
	// Return leftmost value of scale
	double left();
	// Return rightmost value of scale
	double right();
	// Return midpoint value of scale
	double middle();
	// Return range of scale
	double range();

	/*
	// Colours
	*/
	private:
	// Colours
	GLfloat colours_[nScaleColours][4];
	// Colour deltas
	GLfloat deltaLeftRight_[4], deltaLeftMid_[4], deltaMidRight_[4];

	public:
	// Return colour associated with value provided
	void colour(double v, GLfloat *c);
	// Set colour
	void setColour(ScaleColour col, GLfloat r, GLfloat g, GLfloat b, GLfloat a = 1.0f);
	// Copy colour
	void copyColour(ScaleColour col, GLfloat *target);

	/*
	// Linked objects
	*/
	private:
	// Grids that use the colour scale
	Reflist<Grid,int> grids_;
	// Refresh all linked objects
	void refreshObjects();

	public:
	// Link grid with colourscale
	void addLink(Grid *g);
	// Break link between grid and colourscale
	void breakLink(Grid *g);
};

#endif
