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
const char *GlyphTypeKeywords[Glyph::nGlyphTypes] = { "arrow", "vector", "sphere", "cube", "triangle", "ellipsoid", "tetrahedron", "text" };
const char *Glyph::glyphType(Glyph::GlyphType gs)
{
	return GlyphTypeKeywords[gs];
}
Glyph::GlyphType Glyph::glyphType(const char *s)
{
	return (Glyph::GlyphType) enumSearch("glyph style", Glyph::nGlyphTypes, GlyphTypeKeywords, s);
}

// Constructors
GlyphData::GlyphData()
{
	// Private variables
	atom_ = NULL;
	atomData_ = GlyphData::PositionData;
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
GlyphData::GlyphDataType GlyphData::atomData()
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
void GlyphData::setAtom(Atom *target, GlyphDataType type)
{
	atom_ = target;
	atomData_ = type;
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
			case (GlyphData::PositionData):
				return atom_->r();
			case (GlyphData::ForceData):
				return atom_->f();
			case (GlyphData::VelocityData):
				return atom_->v();
		}
	}
	// Default return value is vector data
	return vector_;
}

// Set text data
void Glyph::setText(const char *s)
{
	text_ = s;
}

// Return text data
const char *Glyph::text()
{
	return text_.get();
}

/*
// Glyph
*/

// Return style of Glyph
Glyph::GlyphType Glyph::type()
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
void Glyph::setType(GlyphType gt)
{
	// Add default values, provided they have not already been set...
	switch (gt)
	{
		case (Glyph::ArrowGlyph):
		case (Glyph::VectorGlyph):
			if (!data[1].isSet()) data[1].setVector(0.0,1.0,0.0);
			break;
		case (Glyph::SphereGlyph):
		case (Glyph::CubeGlyph):
			if (!data[1].isSet()) data[1].setVector(1.0,1.0,1.0);
			break;
		case (Glyph::TriangleGlyph):
			break;
		case (Glyph::EllipsoidGlyph):
			break;
		case (Glyph::TextGlyph):
			break;
	}
	type_ = gt;
}
