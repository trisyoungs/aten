/*
	*** Rendering glyph
	*** src/classes/glyph.h
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

#ifndef H_GLYPH_H
#define H_GLYPH_H

#include "classes/atom.h"
#include "templates/vector3.h"
#include "templates/vector4.h"

// Glyph style
enum glyph_style { GS_ARROW, GS_VECTOR, GS_SPHERE, GS_CUBE, GS_TRIANGLE, GS_ELLIPSOID, GS_NITEMS };
const char *text_from_GS(glyph_style);
glyph_style GS_from_text(const char*);

/*
	For GS_ARROW:
		d1 = tail of arrow
		d2 = head of arrow
	For GS_DIRECTION:
		d1 = centre of arrow
		d2 = direction of arrow
	For GS_ELLIPSOID:
		d1 = centre of ellipsoid
		d2 = point direction 1
		d3 = point direction 2
*/

#define MAXGLYPHDATA 4

// Glyph data
class glyphdata
{
	public:
	// Constructor
	glyphdata();

	private:
	// Position or direction vector
	vec3<double> r;
	// Pointer to atom from which to get 'r'
	atom *i;
	// Whether last data set was the atom (TRUE) or the vec3 (FALSE)
	bool atomsetlast;

	public:
	// Set the vector data
	void set_vector(double x, double y, double z) { r.set(x,y,z); atomsetlast = FALSE; }
	// Set the atom pointer
	void set_atom(atom *target) { i = target; atomsetlast = TRUE; }
	// Return the atom pointer
	atom *get_atom() { return i; }
	// Return the vector data
	vec3<double> get_vector();
	// Return if the structure contains an atom pointer
	bool has_atom() { return (i == NULL ? FALSE : TRUE); }
};

// Glyph
class glyph
{
	public:
	// Constructor / Destructor
	glyph();
	~glyph();
	// List pointers
	glyph *prev, *next;

	private:
	// Style of glyph
	glyph_style type;

	public:
	// Data for glyph
	glyphdata data[MAXGLYPHDATA];
	// Set style of glyph
	void set_type(glyph_style gt) { type = gt; }
	// Return style of glyph
	glyph_style get_type() { return type; }
};

#endif
