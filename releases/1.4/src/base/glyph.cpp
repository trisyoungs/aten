/*
	*** Rendering glyph
	*** src/base/glyph.cpp
	Copyright T. Youngs 2007-2009

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
Glyph::GlyphType Glyph::glyphType(const char *s, bool reporterror)
{
	Glyph::GlyphType gt = (Glyph::GlyphType) enumSearch("glyph style", Glyph::nGlyphTypes, GlyphTypeKeywords, s);
	if ((gt == Glyph::nGlyphTypes) && reporterror) enumPrintValid(Glyph::nGlyphTypes,GlyphTypeKeywords);
	return gt;
}
int Glyph::nGlyphData(Glyph::GlyphType gt)
{
	return GlyphTypeNData[gt];
}

/*
// Glyph
*/

// Constructor
Glyph::Glyph()
{
	// Private variables
	visible_ = TRUE;
	solid_ = TRUE;
	lineWidth_ = 1.0f;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Returns the number of data set for the Glyph
int Glyph::nData()
{
	return data_.nItems();
}

// Return nth data in glyph
GlyphData *Glyph::data(int i)
{
	if ((i < 0) || (i >= data_.nItems())) msg.print( "Tried to get data %i for glyph when it has only %i.\n", i+1, data_.nItems());
	else return data_[i];
	return NULL;
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
		default:
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

// Set whether the Glyph is visible
void Glyph::setVisible(bool isvisible)
{
	visible_ = isvisible;
}

// Return whether the Glyph is visible
bool Glyph::isVisible()
{
	return visible_;
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

/*
// GlyphData
*/

// Constructor
GlyphData::GlyphData()
{
	// Private variables
	atom_ = NULL;
	atomData_ = GlyphData::PositionData;
	atomSetLast_ = FALSE;
	set_ = FALSE;
	colour_[0] = prefs.colour(Prefs::GlyphColour)[0];
	colour_[1] = prefs.colour(Prefs::GlyphColour)[1];
	colour_[2] = prefs.colour(Prefs::GlyphColour)[2];
	colour_[3] = prefs.colour(Prefs::GlyphColour)[3];

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set vector data
void GlyphData::setVector(double x, double y, double z)
{
	vector_.set(x,y,z);
	set_ = TRUE;
	atomSetLast_ = FALSE;
}

// Set vector data
void GlyphData::setVector(Vec3<double> vec)
{
	setVector(vec.x, vec.y, vec.z);
}

// Set component of vector data
void GlyphData::setVector(int i, double d)
{
	vector_.set(i, d);
	set_ = TRUE;
	atomSetLast_ = FALSE;
}

// Set atom data
void GlyphData::setAtom(Atom *atom, GlyphData::GlyphDataType av)
{
	atom_ = atom;
	atomData_ = av;
	set_ = TRUE;
	atomSetLast_ = TRUE;
	if (atom_ == NULL) msg.print("Warning - NULL atom pointer stored in data.\n");
}

// Return the atom pointer
Atom *GlyphData::atom()
{
	return atom_;
}

// Return whether the atom was set last
bool GlyphData::atomSetLast()
{
	return atomSetLast_;
}

// Return the data type 
GlyphData::GlyphDataType GlyphData::atomData()
{
	return atomData_;
}

// Return the position
Vec3<double> GlyphData::vector()
{
	if (!atomSetLast_) return vector_;
	else if (atom_ != NULL) return atom_->r();
	return Vec3<double>();
}

// Set colour
void GlyphData::setColour(double r, double g, double b, double a)
{
	colour_[0] = r;
	colour_[1] = g;
	colour_[2] = b;
	colour_[3] = a;
}

// Set n'th component of colour
void GlyphData::setColour(int n, double d)
{
	if ((n < 0) || (n > 4)) msg.print( "Tried to set component %i for colour in glyphdata which is out of range.\n", n+1);
	else colour_[n] = d;
}

// Return colour
double *GlyphData::colour()
{
	return colour_;
}

// Return i'th colour for glyph
void GlyphData::copyColour(GLfloat *col)
{
	 col[0] = (GLfloat) colour_[0];
	 col[1] = (GLfloat) colour_[1];
	 col[2] = (GLfloat) colour_[2];
	 col[3] = (GLfloat) colour_[3];
}
