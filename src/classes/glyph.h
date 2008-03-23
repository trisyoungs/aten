/*
	*** Rendering Glyph
	*** src/classes/Glyph.h
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

#ifndef ATEN_GLYPH_H
#define ATEN_GLYPH_H

#include "classes/atom.h"
#include "templates/vector3.h"
#include "templates/vector4.h"

// Glyph style
enum GlyphStyle { GS_ARROW, GS_VECTOR, GS_SPHERE, GS_CUBE, GS_TRIANGLE, GS_ELLIPSOID, GS_TETRAHEDRON, GS_NITEMS };
const char *text_from_GS(GlyphStyle);
GlyphStyle GS_from_text(const char*);

// Atom data pointer type
enum AtomVectorType { AV_R, AV_F, AV_V };

#define MAXGLYPHDATA 4

// Glyph data
class GlyphData
{
	public:
	// Constructor
	GlyphData();

	private:
	// Position or direction vector
	Vec3<double> vector_;
	// Pointer to atom from which to get 'r'
	Atom *atom_;
	// Type of vector data to take from atom (if defined)
	AtomVectorType atomData_;
	// Whether last data set was the atom (TRUE) or the vec3 (FALSE)
	bool atomSetLast_;
	// Status of data item (whether it has been set or not)
	bool set_;

	public:
	// Set the vector data
	void setVector(double x, double y, double z);
	// Set the atom pointer
	void setAtom(Atom *target, AtomVectorType av);
	// Return the atom pointer
	Atom *atom();
	// Return the type of atom vector pointed to
	AtomVectorType atomData();
	// Return the vector data
	Vec3<double> vector();
	// Return if the structure contains an atom pointer
	bool hasAtom();
	// Returns whether one of either atom* or vecdata have been set
	bool isSet();
};

// Glyph
class Glyph
{
	public:
	// Constructor
	Glyph();
	// List pointers
	Glyph *prev, *next;

	private:
	// Style of Glyph
	GlyphStyle type_;
	// Whether Glyph should be drawn with filled polygons (where possible)
	bool solid_;

	public:
	// Data for Glyph
	GlyphData data[MAXGLYPHDATA];
	// Set style of Glyph
	void setType(GlyphStyle gt);
	// Return style of Glyph
	GlyphStyle type();
	// Set whether the Glyph is solid or not
	void setSolid(bool issolid);
	// Return whether the Glyph should be drawn as a solid
	bool isSolid();
};

#endif
