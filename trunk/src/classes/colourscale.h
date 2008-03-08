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

// Colourscale order
enum colourscale_order { CO_TWOPOINT=2, CO_THREEPOINT=3 };

#ifndef H_COLOURSCALE_H
#define H_COLOURSCALE_H

// Colour Scale
class colourscale
{
	public:
	// Constructor / Destructor
	colouscale();
	~colourscale();

	private:
	// Type of colourscale
	colourscale_order type;
	// Colours
	GLint colours[3][4];
	// Minimum, maximum, and midpoint of data range
	double min, max, mid;

	public:
	// Set type of colourscale
	void set_type(colourscale_order co) { type = co; }

};

