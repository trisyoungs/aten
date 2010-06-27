/*
	*** Rendering Glyph
	*** src/base/glyph.h
	Copyright T. Youngs 2007-2010

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
#include "templates/list.h"
#include "templates/vector3.h"
#include "templates/vector4.h"
#include <QtOpenGL/QtOpenGL>

// Forward declarations
class Model;
class Atom;

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
	// Integer atom in the parent model from which to get r, f, or v
	Atom *atom_;
	// Type of vector data to take from atom (if defined)
	GlyphDataType atomData_;
	// Whether last data set was the atom (TRUE) or the vec3 (FALSE)
	bool atomSetLast_;
	// Status of data item (whether it has been set or not)
	bool set_;
	// Colour at this data point
	double colour_[4];

	public:
	// Set vector data for glyph
	void setVector(Vec3<double> vec);
	void setVector(double x, double y, double z);
	void setVector(int i, double d);
	// Set atom pointer for datapoint
	void setAtom(Atom *atom);
	// Set atom data type for datapoint
	void setAtomData(GlyphData::GlyphDataType av);
	// Set atom pointer and datatype for datapoint
	void setAtom(Atom *atom, GlyphData::GlyphDataType av);
	// Returns the atom associated to the datapoint (if any)
	Atom *atom();
	// Returns whether the atom was set last
	bool atomSetLast();
	// Return the atom data type
	GlyphData::GlyphDataType atomData();
	// Return vector data for datapoint
	Vec3<double> vector();
	// Set colour of datapoint
	void setColour(double r, double g, double b, double a = 1.0f);
	// Set n'th component of colour in datapoint
	void setColour(int n, double d);
	// Return i'th colour
	double *colour();
	// Copy colour for datapoint
	void copyColour(GLfloat *c);
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
	enum GlyphType { ArrowGlyph, CubeGlyph, EllipsoidGlyph, EllipsoidXYZGlyph, LineGlyph, QuadGlyph, SenseVectorGlyph, SphereGlyph, TetrahedronGlyph, TextGlyph, Text3DGlyph, TriangleGlyph, VectorGlyph, nGlyphTypes };
	static const char *glyphType(GlyphType gt);
	static GlyphType glyphType(const char *name, bool reporterror = 0);
	static int nGlyphData(GlyphType gt);
	// Glyph options
	enum GlyphOption { GlyphColourOption, GlyphLineWidthOption, GlyphSolidOption, GlyphTextOption, GlyphWireOption, nGlyphOptions };
	static const char *glyphOption(GlyphOption gt);
	static GlyphOption glyphOption(const char *name, bool reporterror = 0);

	private:
	// Style of Glyph
	GlyphType type_;
	// Text data
	Dnchar text_;
	// Parent model
	Model *parent_;
	// Vector data for Glyph
	List<GlyphData> data_;
	// Rotation matrix for the glyph (NULL if not rotated)
	Mat3<double> *rotation_;

	public:
	// Returns the number of data set for the Glyph
	int nData();
	// Return the n'th datapoint of the glyph
	GlyphData *data(int i);
	// Set style of Glyph
	void setType(GlyphType gt);
	// Return style of Glyph
	GlyphType type();
	// Set text data
	void setText(const char *s);
	// Return text data
	const char *text();
	// Set colour of all datapoints
	void setColour(double r, double g, double b, double a = 1.0f);
	// Return whether glyph has been rotated (whether a rotation matrix exists)
	bool rotated();
	// Return rotation matrix suitable for GL
	double *rotationForGL();
	// Return rotation matrix
	Mat3<double> *rotation();
	// Set element of rotation matrix
	void setRotationElement(int el, double d);
	// Get element of rotation matrix
	double getRotationElement(int el);
	// Reset rotation matrix (and set rotated_ = FALSE)
	void resetRotation();
	// Rotate about arbitrary axis
	void rotate(double x, double y, double z, double angle);
	// Rotate about X axis
	void rotateX(double angle);
	// Rotate about Y axis
	void rotateY(double angle);
	// Rotate about Z axis
	void rotateZ(double angle);
	// Set parent model
	void setParent(Model *parent);
	// Return parent model
	Model *parent();

	/*
	// Style
	*/
	private:
	// Whether glyph is visible
	bool visible_;
	// Whether Glyph should be drawn with filled polygons (where possible)
	bool solid_;
	// Line width to use when drawing
	GLfloat lineWidth_;

	public:
	// Set whether the Glyph is visible
	void setVisible(bool isvisible);
	// Return whether the Glyph is visible
	bool isVisible();
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
