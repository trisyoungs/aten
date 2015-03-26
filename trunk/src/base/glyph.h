/*
	*** Rendering Glyph
	*** src/base/glyph.h
	Copyright T. Youngs 2007-2015

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

#include "templates/list.h"
#include "templates/vector3.h"
#include "templates/vector4.h"
#include "math/matrix.h"
#include "base/namespace.h"
#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Model;
class Atom;

// Glyph data
class GlyphData : public ListItem<GlyphData>
{
	public:
	// Constructor
	GlyphData();
	// Atom data pointer type
	enum GlyphDataType { PositionData, ForceData, VelocityData };

	private:
	// Position or direction vector
	Vec3<double> vector_;
	// Atom in the parent model from which to get r, f, or v
	Atom* atom_;
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
	void setAtom(Atom* atom);
	// Set atom data type for datapoint
	void setAtomData(GlyphData::GlyphDataType av);
	// Set atom pointer and datatype for datapoint
	void setAtom(Atom* atom, GlyphData::GlyphDataType av);
	// Returns the atom associated to the datapoint (if any)
	Atom* atom();
	// Returns whether the atom was set last
	bool atomSetLast() const;
	// Return the atom data type
	GlyphData::GlyphDataType atomData() const;
	// Return vector data for datapoint
	Vec3<double> vector() const;
	// Set colour of datapoint
	void setColour(double r, double g, double b, double a = 1.0f);
	// Set n'th component of colour in datapoint
	void setColour(int n, double d);
	// Return i'th colour
	double* colour();
	// Copy colour for datapoint
	void copyColour(Vec4<GLfloat>& col) const;
};

// Glyph
class Glyph : public ListItem<Glyph>
{
	public:
	// Constructor
	Glyph();
	// Operator =
	void operator=(Glyph &source);
	// Glyph style
	enum GlyphType { ArrowGlyph, CubeGlyph, EllipsoidGlyph, EllipsoidXYZGlyph, LineGlyph, QuadGlyph, SenseVectorGlyph, SphereGlyph, TetrahedronGlyph, TextGlyph, Text3DGlyph, TriangleGlyph, TubeArrowGlyph, TubeVectorGlyph, VectorGlyph, nGlyphTypes };
	static const char* glyphType(GlyphType gt);
	static const char* glyphTypeName(GlyphType gt);
	static GlyphType glyphType(QString s, bool reportError = false);
	static int nGlyphData(GlyphType gt);
	// Glyph options
	enum GlyphOption { GlyphColourOption, GlyphSolidOption, GlyphTextOption, GlyphWireOption, nGlyphOptions };
	static const char* glyphOption(GlyphOption gt);
	static GlyphOption glyphOption(QString s, bool reportError = false);


	/*
	 * Data
	 */
	private:
	// Parent model
	Model* parent_;
	// Style of Glyph
	GlyphType type_;
	// Text data
	QString text_;
	// Vector data for Glyph
	List<GlyphData> data_;
	// Rotation matrix for the glyph (NULL if not rotated)
	Matrix rotation_;
	// Whether rotation matrix has been modified
	bool rotated_;

	public:
	// Set parent model
	void setParent(Model* parent);
	// Return parent model
	Model* parent();
	// Set style of Glyph
	void setType(GlyphType gt);
	// Return style of Glyph
	GlyphType type() const;
	// Set text data
	void setText(QString text);
	// Return text data
	QString text() const;
	// Returns the number of data set for the Glyph
	int nData() const;
	// Return the n'th datapoint of the glyph
	GlyphData* data(int i);
	// Set colour of all datapoints
	void setColour(double r, double g, double b, double a = 1.0f);
	// Apply stored rotation matrix to supplied matrix, if it has been modified
	bool applyRotation(Matrix& A);
	// Return stored rotation matrix
	const Matrix& rotation() const;
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
	// Return whether rotation matrix has been modified
	bool rotated() const;


	/*
	 * Style
	 */
	private:
	// Whether glyph is selected
	bool selected_;
	// Whether glyph is visible
	bool visible_;
	// Whether Glyph should be drawn with filled polygons (where possible)
	bool solid_;

	public:
	// Set whether the Glyph is selected
	void setSelected(bool isselected);
	// Return whether the Glyph is selected
	bool isSelected() const;
	// Set whether the Glyph is visible
	void setVisible(bool isvisible);
	// Return whether the Glyph is visible
	bool isVisible() const;
	// Set whether the Glyph is solid or not
	void setSolid(bool issolid);
	// Return whether the Glyph should be drawn as a solid
	bool isSolid() const;
};

ATEN_END_NAMESPACE

#endif
