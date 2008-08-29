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

#include "base/dnchar.h"
#include "templates/vector3.h"
#include "templates/vector4.h"

// Forward declarations
class Model;

// Glyph data
class GlyphData
{
	public:
	// Constructor
	GlyphData();
	// List pointers
	GlyphData *prev, *next;

	// Atom data pointer type
	enum GlyphDataType { PositionData, ForceData, VelocityData };

	private:
	// Position or direction vector
	Vec3<double> vector_;
	// Integer atom id in the parent model from ehich to get r, f, or v
	int atomId_;
	// Type of vector data to take from atom (if defined)
	GlyphDataType atomData_;
	// Whether last data set was the atom (TRUE) or the vec3 (FALSE)
	bool atomSetLast_;
	// Status of data item (whether it has been set or not)
	bool set_;
	// Colour at this data point
	GLfloat colour_[4];

	public:
	// Set the vector data
	void setVector(double x, double y, double z);
	// Set the atom pointer
	void setAtomId(int target, GlyphDataType av);
	// Return the atom id used
	int atomId();
	// Return the type of atom vector pointed to
	GlyphDataType atomData();
	// Return the vector data
	Vec3<double> vector(Model *parent);
	// Return if the structure contains a valid atom reference
	bool hasAtom();
	// Returns whether one of either atom* or vecdata have been set
	bool isSet();
	// Set the colour associated to this data point
	void setColour(GLfloat r, GLfloat g, GLfloat b, GLfloat a = 1.0f);
	// Return pointer to colour
	GLfloat *colour();
};

// Glyph
class Glyph
{
	public:
	// Constructor
	Glyph();
	// List pointers
	Glyph *prev, *next;
	// Glyph style
	enum GlyphType { ArrowGlyph, VectorGlyph, SenseVectorGlyph, SphereGlyph, CubeGlyph, QuadGlyph, TriangleGlyph, LineGlyph, EllipsoidGlyph, EllipsoidXYZGlyph, TetrahedronGlyph, TextGlyph, TextGlyph3D, nGlyphTypes };
	static const char *glyphType(GlyphType);
	static GlyphType glyphType(const char*);
	static int nGlyphData(GlyphType);

	private:
	// Style of Glyph
	GlyphType type_;
	// Text data
	Dnchar text_;
	// Parent model
	Model *parent_;
	// Data for Glyph
	List<GlyphData> data_;

	public:
	// Set vector data for glyph
	void setVector(int i, Vec3<double> vec);
	void setVector(int i, double x, double y, double z);
	// Set atom data for glyph
	void setAtom(int i, int atom, GlyphData::GlyphDataType av);
	// Returns the atom id of the glyp
	int atomId(int i);
	// Return whether one of the data is set to an atomId
	bool hasAtomId(int i);
	// Return i'th vector data for glyph
	Vec3<double> vector(int i);
	// Set i'th colour in glyph
	void setColour(int i, GLfloat r, GLfloat g, GLfloat b, GLfloat a = 1.0f);
	// Return i'th colour for glyph
	GLfloat *colour(int i);
	// Set style of Glyph
	void setType(GlyphType gt);
	// Return style of Glyph
	GlyphType type();
	// Set text data
	void setText(const char *s);
	// Return text data
	const char *text();
	// Set parent model
	void setParent(Model *parent);
	// Return parent model
	Model *parent();

	/*
	// Style
	*/
	private:
	// Whether Glyph should be drawn with filled polygons (where possible)
	bool solid_;
	// Line width to use when drawing
	GLfloat lineWidth_;

	public:
	// Set whether the Glyph is solid or not
	void setSolid(bool issolid);
	// Return whether the Glyph should be drawn as a solid
	bool isSolid();
	// Set the linewidth of the glyph
	void setLineWidth(GLfloat width);
	// Return the linewidth of the glyph
	GLfloat lineWidth();
};

#endif
