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
#include "base/messenger.h"
#include "base/sysfunc.h"
#include "model/model.h"

// Glyph styles
const char *GlyphTypeKeywords[Glyph::nGlyphTypes] = { "arrow", "vector", "svector", "sphere", "cube", "quad", "triangle", "line", "ellipsoid", "tetrahedron", "text", "text3d" };
int GlyphTypeNData[Glyph::nGlyphTypes] = { 2, 2, 3, 2, 2, 4, 3, 2, 3, 4, 1, 1 };
const char *Glyph::glyphType(Glyph::GlyphType gs)
{
	return GlyphTypeKeywords[gs];
}
Glyph::GlyphType Glyph::glyphType(const char *s)
{
	return (Glyph::GlyphType) enumSearch("glyph style", Glyph::nGlyphTypes, GlyphTypeKeywords, s);
}
int Glyph::nGlyphData(Glyph::GlyphType gt)
{
	return GlyphTypeNData[gt];
}

// Constructors
GlyphData::GlyphData()
{
	// Private variables
	atomId_ = -1;
	atomData_ = GlyphData::PositionData;
	atomSetLast_ = FALSE;
	set_ = FALSE;
	colour_[0] = 0.0f;
	colour_[1] = 0.0f;
	colour_[2] = 0.0f;
	colour_[3] = 1.0f;

	// Public variables
	prev = NULL;
	next = NULL;
}

Glyph::Glyph()
{
	// Private variables
	solid_ = TRUE;
	parent_ = NULL;
	lineWidth_ = 1.0f;
	parent_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

/*
// GlyphData
*/

// Return the atom pointer
int GlyphData::atomId()
{
	return atomId_;
}

// Return the type of atom vector pointed to
GlyphData::GlyphDataType GlyphData::atomData()
{
	return atomData_;
}

// Return if the structure contains an atom pointer
bool GlyphData::hasAtom()
{
	return (atomId_ == -1 ? FALSE : TRUE);
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
void GlyphData::setAtomId(int target, GlyphDataType type)
{
	atomId_ = target;
	atomData_ = type;
	atomSetLast_ = TRUE;
	set_ = TRUE;
}

// Return the vector data
Vec3<double> GlyphData::vector(Model *parent)
{
	if (atomSetLast_)
	{
		if (atomId_ == -1)
		{
			msg.print( "Atom was apparently set last in glyph, but stored id is '-1'.\n");
			return vector_;
		}
		Atom *i = parent->atom(atomId_);
		if (i == NULL)
		{
			msg.print( "Atom ID set in glyph (%i) is outside range for model.\n", atomId_);
			return vector_;
		}
		switch (atomData_)
		{
			case (GlyphData::PositionData):
				return i->r();
			case (GlyphData::ForceData):
				return i->f();
			case (GlyphData::VelocityData):
				return i->v();
		}
	}
	// Default return value is vector data
	return vector_;
}

// Set the colour associated to this data point
void GlyphData::setColour(GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	colour_[0] = r;
	colour_[1] = g;
	colour_[2] = b;
	colour_[3] = a;
}

// Return pointer to colour
GLfloat *GlyphData::colour()
{
	return colour_;
}

/*
// Glyph
*/

// Set vector data for glyph
void Glyph::setVector(int i, double x, double y, double z)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set vector %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else data_[i]->setVector(x, y, z);
}

// Set vector data for glyph
void Glyph::setVector(int i, Vec3<double> vec)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set vector %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else data_[i]->setVector(vec.x, vec.y, vec.z);
}

// Set atom data for glyph
void Glyph::setAtom(int i, int atom, GlyphData::GlyphDataType av)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set atom id %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else
	{
		data_[i]->setAtomId(atom, av);
		if (atom == -1) msg.print("Warning - no atom stored in glyph data %i.\n",i);
	}
}

// Returns the atom id of the glyph
int Glyph::atomId(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get atom id %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->atomId();
	return -1;
}

// Returns whether the specified data is to be taken from an atom
bool Glyph::hasAtomId(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to test atom id %i in glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->hasAtom();
	return FALSE;
}

// Return vector data for glyph
Vec3<double> Glyph::vector(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get vector %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->vector(parent_);
	return Vec3<double>();
}

// Set parent model
void Glyph::setParent(Model *parent)
{
	parent_ = parent;
}

// Return parent model
Model *Glyph::parent()
{
	return parent_;
}

// Return style of Glyph
Glyph::GlyphType Glyph::type()
{
	return type_;
}

// Set style of glyph (and set data vectors to default values)
void Glyph::setType(GlyphType gt)
{
	// Create list of GlyphData
	data_.createEmpty( Glyph::nGlyphData(gt) );
	// Add default values, provided they have not already been set...
	switch (gt)
	{
		case (Glyph::ArrowGlyph):
		case (Glyph::VectorGlyph):
			data_[1]->setVector(0.0,1.0,0.0);
			break;
		case (Glyph::SphereGlyph):
		case (Glyph::CubeGlyph):
			data_[1]->setVector(1.0,1.0,1.0);
			break;
		case (Glyph::TriangleGlyph):
			break;
		case (Glyph::EllipsoidGlyph):
			break;
		case (Glyph::TextGlyph):
			break;
		case (Glyph::TextGlyph3D):
			break;
	}
	type_ = gt;
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

// Set i'th colour in glyph
void Glyph::setColour(int i, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set colour %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else data_[i]->setColour(r, g, b, a);
}

// Return i'th colour for glyph
GLfloat *Glyph::colour(int i)
{
	static GLfloat black[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get colour %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->colour();
	return black;
}

/*
// Glyph Style
*/

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

// Set the linewidth of the glyph
void Glyph::setLineWidth(GLfloat width)
{
	lineWidth_ = width;
}

// Return the linewidth of the glyph
GLfloat Glyph::lineWidth()
{
	return lineWidth_;
}
