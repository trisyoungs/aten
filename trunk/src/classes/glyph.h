/*
	*** Rendering glyph
	*** src/classes/glyph.h
	Copyright T. Youngs 2007

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

#ifndef H_GLYPH_H
#define H_GLYPH_H

#include "templates/vector3.h"
#include "templates/vector4.h"

// Forward declarations
class atom;

// Glyph types
enum glyph_type { GT_ARROW, GT_DIRECTION, GT_ELLIPSOID };

/*
	For GT_ARROW:
		d1 = tail of arrow
		d2 = head of arrow
		d3 = unused
	For GT_DIRECTION:
		d1 = centre of arrow
		d2 = direction of arrow
		d3 = unused
	For GT_ELLIPSOID:
		d1 = centre of ellipsoid
		d2 = point direction 1
		d3 = point direction 2
*/

// Glyph data
struct glyph_data
{
	// Fixed position / coordinate / direction etc
	vec3<double> r;
	// ID of atom from which to get 'r'
	int id;
	// Pointer to atom from which to get 'r'
	atom *i;
	// Whether to make the data a unit vector  TODO enum offering scaled, unit etc?
	bool make_unit;
};

// Glyph
class glyph
{
	private:
	// Style of glyph
	glyph_type type;
	// Data for glyph
	glyph_data r1,r2,r3;
	public:
	// Parse supplied string and set values in structure
	void set_from_string(const char*);
	// List pointers
	glyph *prev, *next;
	// Constructor / Destructor
	glyph();
	~glyph();
};

#endif
