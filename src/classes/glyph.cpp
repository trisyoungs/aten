/*
	*** Rendering glyph
	*** src/classes/glyph.cpp
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

#include "classes/glyph.h"
#include "base/debug.h"
#include "base/sysfunc.h"

// Glyph styles
const char *GS_keywords[GS_NITEMS] = { "arrow", "vector", "sphere", "cube", "triangle", "ellipsoid", "tetrahedron" };
const char *text_from_GS(GlyphStyle gs)
	{ return GS_keywords[gs]; }
GlyphStyle GS_from_text(const char *s)
	{ return (GlyphStyle) enumSearch("glyph style",GS_NITEMS,GS_keywords,s); }

// Constructors
GlyphData::GlyphData()
{
	// Private variables
	atom_ = NULL;
	atomData_ = AV_R;
	atomSetLast_ = FALSE;
	set_ = FALSE;
}

Glyph::Glyph()
{
	// Private variables
	solid_ = TRUE;
	// Public variables
	prev = NULL;
	next = NULL;
}

/*
// GlypData
*/

// Return the atom pointer
Atom *GlyphData::atom()
{
	return atom_;
}

// Return the type of atom vector pointed to
AtomVectorType GlyphData::atomData()
{
	return atomData_;
}

// Return if the structure contains an atom pointer
bool GlyphData::hasAtom()
{
	return (atom_ == NULL ? FALSE : TRUE);
}

// Returns whether one of either atom* or vecdata have been set
bool GlyphData::isSet()
{
	return set_;
}

// Set the vector data
void GlyphData::setVector(double x, double y, double z)
{
	vector_.set(x,y,z);
	atomSetLast_ = FALSE;
	set_ = TRUE;
}

// Set the atom pointer
void GlyphData::setAtom(Atom *target, AtomVectorType av)
{
	atom_ = target;
	atomData_ = av;
	atomSetLast_ = TRUE;
	set_ = TRUE;
}

// Return the vector data
Vec3<double> GlyphData::vector()
{
	if (atomSetLast_)
	{
		if (atom_ == NULL)
		{
			printf("Atom was apparently set last in glyph, but pointer is NULL.\n");
			return vector_;
		}
		switch (atomData_)
		{
			case (AV_R): return atom_->r();
			case (AV_F): return atom_->f();
			case (AV_V): return atom_->v();
		}
	}
	else return vector_;
}

/*
// Glyph
*/

// Return style of Glyph
GlyphStyle Glyph::type()
{
	return type_;
}

// Set whether the Glyph is solid or not
void Glyph::setSolid(bool issolid)
{
	solid_ = issolid;
}

// Return whether the Glyph should be drawn as a solid
bool Glyph::isSolid()
{
	return solid_;
}

// Set style of glyph (and set data vectors to default values)
void Glyph::setType(GlyphStyle gt)
{
	// Add default values, provided they have not already been set...
	switch (gt)
	{
		case (GS_ARROW):
		case (GS_VECTOR):
			if (!data[1].isSet()) data[1].setVector(0.0,1.0,0.0);
			break;
		case (GS_SPHERE):
		case (GS_CUBE):
			if (!data[1].isSet()) data[1].setVector(1.0,1.0,1.0);
			break;
		case (GS_TRIANGLE):
			break;
		case (GS_ELLIPSOID):
			break;
	}
	type_ = gt;
}
