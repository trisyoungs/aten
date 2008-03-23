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

// Colourscale order
enum ColourScaleOrder { CO_TWOPOINT=2, CO_THREEPOINT=3 };

#ifndef ATEN_COLOURSCALE_H
#define ATEN_COLOURSCALE_H

// Colour Scale
class ColourScale
{
	public:
	// Constructor / Destructor
	ColourScale();
	~ColourScale();

	private:
	// Type of ColourScale
	ColourScaleOrder type_;
	// Colours
	GLfloat colours_[3][4];
	// Minimum, maximum, and midpoint of data range
	double minimum_, maximum_, midpoint_;

	public:
	// Set type of ColourScale
	void setType(ColourScaleOrder co) { type_ = co; }

};

