/*
	*** Rendering glyph
	*** src/base/glyph.cpp
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

#include "base/glyph.h"
#include "base/sysfunc.h"
#include "classes/prefs.h"
#include "model/model.h"

// Glyph styles
const char *GlyphTypeKeywords[Glyph::nGlyphTypes] = { "arrow", "vector", "svector", "sphere", "cube", "quad", "triangle", "line", "ellipsoid", "ellipsoidxyz", "tetrahedron", "text", "text3d" };
int GlyphTypeNData[Glyph::nGlyphTypes] = { 2, 2, 3, 2, 2, 4, 3, 2, 3, 4, 4, 1, 1 };
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
	// Public variables
	atom = NULL;
	atomData = GlyphData::PositionData;
	atomSetLast = FALSE;
	set = FALSE;
	prefs.copyColour(Prefs::GlyphColour, colour);
	prev = NULL;
	next = NULL;
}

Glyph::Glyph()
{
	// Private variables
	solid_ = TRUE;
	lineWidth_ = 1.0f;

	// Public variables
	prev = NULL;
	next = NULL;
}

/*
// Glyph
*/

// Set vector data for glyph
void Glyph::setVector(int i, double x, double y, double z)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set vector %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else
	{
		data_[i]->vector.set(x,y,z);
		data_[i]->set = TRUE;
		data_[i]->atomSetLast = FALSE;
	}
}

// Set vector data for glyph
void Glyph::setVector(int i, Vec3<double> vec)
{
	setVector(i, vec.x, vec.y, vec.z);
}

// Set atom data for glyph
void Glyph::setAtom(int i, Atom *atom, GlyphData::GlyphDataType av)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to set atom id %i for glyph when it has only %i in total.\n", i+1, data_.nItems());
	else
	{
		data_[i]->atom = atom;
		data_[i]->atomData = av;
		data_[i]->set = TRUE;
		data_[i]->atomSetLast = TRUE;
		if (atom == NULL) msg.print("Warning - NULL atom pointer stored in data %i.\n",i);
	}
}

// Returns the atom id of the glyph
Atom *Glyph::atom(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get atom id %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->atom;
	return NULL;
}

// Returns whether the atom was set last for data 'i'
bool Glyph::atomSetLast(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get atomSetLast %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->atomSetLast;
	return FALSE;
}

// Returns the data type for data id 'i'
GlyphData::GlyphDataType Glyph::atomData(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get dataType id %i from glyph when it has only %i data in total.\n", i+1, data_.nItems());
	else return data_[i]->atomData;
	return GlyphData::PositionData;
}

// Returns the atom id of the glyph
Vec3<double> Glyph::vector(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get vector %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else
	{
		if (data_[i]->atomSetLast) return data_[i]->atom->r();
		else return data_[i]->vector;
	}
	return Vec3<double>();
}

// // Returns whether the specified data is to be taken from an atom
// bool Glyph::hasAtomId(int i)
// {
// 	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to test atom id %i in glyph when it has only %i in total.\n", i+1, data_.nItems());
// 	else return (data_[i]->atomId == -1 ? FALSE : TRUE);
// 	return FALSE;
// }

// Returns the number of data set for the Glyph
int Glyph::nData()
{
	return data_.nItems();
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
			setVector(1,0.0,1.0,0.0);
			break;
		case (Glyph::SphereGlyph):
		case (Glyph::CubeGlyph):
			setVector(1,1.0,1.0,1.0);
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
	else
	{
		data_[i]->colour[0] = r;
		data_[i]->colour[1] = g;
		data_[i]->colour[2] = b;
		data_[i]->colour[3] = a;
	}
}

// Return i'th colour for glyph
GLfloat *Glyph::colour(int i)
{
	static GLfloat black[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get colour %i from glyph when it has only %i in total.\n", i+1, data_.nItems());
	else return data_[i]->colour;
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
