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
enum glyph_style { GS_ARROW, GS_VECTOR, GS_SPHERE, GS_CUBE, GS_TRIANGLE, GS_ELLIPSOID, GS_TETRAHEDRON, GS_NITEMS };
const char *text_from_GS(glyph_style);
glyph_style GS_from_text(const char*);

// Atom data pointer type
enum atom_vector { AV_R, AV_F, AV_V };

#define MAXGLYPHDATA 4

// Glyph data
class glyphdata
{
	public:
	// Constructor
	glyphdata();

	private:
	// Position or direction vector
	vec3<double> vec;
	// Pointer to atom from which to get 'r'
	atom *i;
	// Type of vector data to take from atom (if defined)
	atom_vector idata;
	// Whether last data set was the atom (TRUE) or the vec3 (FALSE)
	bool atomsetlast;
	// Status of data item (whether it has been set or not)
	bool set;

	public:
	// Set the vector data
	void set_vector(double x, double y, double z);
	// Set the atom pointer
	void set_atom(atom *target, atom_vector av);
	// Return the atom pointer
	atom *get_atom() { return i; }
	// Return the type of atom vector pointed to
	atom_vector get_atomdatatype() { return idata; }
	// Return the vector data
	vec3<double> get_vector();
	// Return if the structure contains an atom pointer
	bool has_atom() { return (i == NULL ? FALSE : TRUE); }
	// Returns whether one of either atom* or vecdata have been set
	bool is_set() { return set; }
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
	// Whether glyph should be drawn with filled polygons (where possible)
	bool solid;

	public:
	// Data for glyph
	glyphdata data[MAXGLYPHDATA];
	// Set style of glyph
	void set_type(glyph_style gt);
	// Return style of glyph
	glyph_style get_type() { return type; }
	// Set whether the glyph is solid or not
	void set_solid(bool issolid) { solid = issolid; }
	// Return whether the glyph should be drawn as a solid
	bool is_solid() { return solid; }
};

#endif
