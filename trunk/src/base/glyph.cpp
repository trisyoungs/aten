/*
	*** Rendering glyph
	*** src/base/glyph.cpp
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

#include "base/glyph.h"
#include "base/sysfunc.h"
#include "math/matrix.h"
#include "base/prefs.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Glyph styles
const char* GlyphTypeKeywords[Glyph::nGlyphTypes] = { "arrow", "cube", "ellipsoid", "ellipsoidxyz", "line", "quad", "svector", "sphere", "tetrahedron", "text", "text3d", "triangle", "tubearrow", "tubevector", "vector" };
const char* GlyphTypeNames[Glyph::nGlyphTypes] = { "Arrow", "Cube", "Ellipsoid", "EllipsoidXYZ", "Line", "Quad", "SenseVector", "Sphere", "Tetrahedron", "Text", "Text3D", "Triangle", "TubeArrow", "TubeVector", "Vector" };
int GlyphTypeNData[Glyph::nGlyphTypes] = { 2, 2, 3, 4, 2, 4, 3, 2, 4, 1, 1, 3, 2, 2, 2 };
const char* Glyph::glyphType(Glyph::GlyphType gt)
{
	return GlyphTypeKeywords[gt];
}
const char* Glyph::glyphTypeName(Glyph::GlyphType gt)
{
	return GlyphTypeNames[gt];
}
Glyph::GlyphType Glyph::glyphType(const char* s, bool reportError)
{
	Glyph::GlyphType gt = (Glyph::GlyphType) enumSearch("glyph style", Glyph::nGlyphTypes, GlyphTypeKeywords, s);
	if ((gt == Glyph::nGlyphTypes) && reportError) enumPrintValid(Glyph::nGlyphTypes,GlyphTypeKeywords);
	return gt;
}
int Glyph::nGlyphData(Glyph::GlyphType gt)
{
	return GlyphTypeNData[gt];
}

// Glyph options
const char* GlyphOptionKeywords[Glyph::nGlyphOptions] = { "colour", "solid", "text", "wire" };
const char* Glyph::glyphOption(Glyph::GlyphOption go)
{
	return GlyphOptionKeywords[go];
}
Glyph::GlyphOption Glyph::glyphOption(const char* s, bool reportError)
{
	Glyph::GlyphOption go = (Glyph::GlyphOption) enumSearch("glyph option", Glyph::nGlyphOptions, GlyphOptionKeywords, s);
	if ((go == Glyph::nGlyphOptions) && reportError) enumPrintValid(Glyph::nGlyphOptions,GlyphOptionKeywords);
	return go;
}

/*
 * GlyphData
 */

// Constructor
GlyphData::GlyphData() : ListItem<GlyphData>()
{
	// Private variables
	atom_ = NULL;
	atomData_ = GlyphData::PositionData;
	atomSetLast_ = FALSE;
	set_ = FALSE;
	colour_[0] = prefs.colour(Prefs::GlyphDefaultColour)[0];
	colour_[1] = prefs.colour(Prefs::GlyphDefaultColour)[1];
	colour_[2] = prefs.colour(Prefs::GlyphDefaultColour)[2];
	colour_[3] = prefs.colour(Prefs::GlyphDefaultColour)[3];
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
void GlyphData::setAtom(Atom* atom)
{
	atom_ = atom;
	set_ = TRUE;
	if (atom_ == NULL)
	{
		Messenger::print("Info - NULL atom pointer stored in glyph data, so vector data will be used instead.");
		atomSetLast_ = FALSE;
	}
	else atomSetLast_ = TRUE;
}

// Set atom data type for datapoint
void GlyphData::setAtomData(GlyphData::GlyphDataType av)
{
	atomData_ = av;
}

// Set atom pointer and datatype for datapoint
void GlyphData::setAtom(Atom* atom, GlyphData::GlyphDataType av)
{
	setAtom(atom);
	setAtomData(av);
}

// Return the atom pointer
Atom* GlyphData::atom()
{
	return atom_;
}

// Return whether the atom was set last
bool GlyphData::atomSetLast() const
{
	return atomSetLast_;
}

// Return the data type 
GlyphData::GlyphDataType GlyphData::atomData() const
{
	return atomData_;
}

// Return the position
Vec3<double> GlyphData::vector() const
{
	if (!atomSetLast_) return vector_;
	else if (atom_ != NULL) return atom_->r();
	Messenger::print("Warning - Atom pointer is defined NULL *and* glyph data has not been set to use vector data ({0,0,0} returned)...");
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
	if ((n < 0) || (n > 4)) Messenger::print( "Tried to set component %i for colour in glyphdata which is out of range.", n+1);
	else colour_[n] = d;
}

// Return colour
double* GlyphData::colour()
{
	return colour_;
}

// Return i'th colour for glyph
void GlyphData::copyColour(Vec4<GLfloat>& col) const
{
	col.x = (GLfloat) colour_[0];
	col.y = (GLfloat) colour_[1];
	col.z = (GLfloat) colour_[2];
	col.w = (GLfloat) colour_[3];
}

/*
 * Glyph
 */

// Constructor
Glyph::Glyph() : ListItem<Glyph>()
{
	// Private variables
	selected_ = FALSE;
	visible_ = TRUE;
	solid_ = TRUE;
	rotated_ = false;
	type_ = Glyph::nGlyphTypes;
	parent_ = NULL;
}

// Assignment operator
void Glyph::operator=(Glyph &source)
{
	// Simple data first
	type_ = source.type_;
	selected_ = source.selected_;
	visible_ = source.visible_;
	solid_ = source.solid_;
	rotation_ = source.rotation_;

	// Glyph data
	data_.clear();
	data_.createEmpty( Glyph::nGlyphData(type_) );
	for (int n=0; n<Glyph::nGlyphData(type_); ++n) *data_[n] =  *(source.data_[n]);
}	

/*
 * Data
 */

// Set parent model
void Glyph::setParent(Model* parent)
{
	parent_ = parent;
}

// Return parent model
Model* Glyph::parent()
{
	return parent_;
}

// Return style of Glyph
Glyph::GlyphType Glyph::type() const
{
	return type_;
}

// Set style of glyph (and set data vectors to default values)
void Glyph::setType(GlyphType gt)
{
	// Create list of GlyphData and add default values, unless number of data match in new type match current type
	if ((type_ == Glyph::nGlyphTypes) || (Glyph::nGlyphData(gt) != Glyph::nGlyphData(type_)))
	{
		data_.createEmpty( Glyph::nGlyphData(gt) );
		// Set default values for new type
		switch (gt)
		{
			case (Glyph::ArrowGlyph):
			case (Glyph::TubeArrowGlyph):
			case (Glyph::TubeVectorGlyph):
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
	}
	
	// Set new glyph type
	type_ = gt;
}

// Set text data
void Glyph::setText(QString text)
{
	text_ = text;
}

// Return text data
QString Glyph::text() const
{
	return text_;
}

// Returns the number of data set for the Glyph
int Glyph::nData() const
{
	return data_.nItems();
}

// Return nth data in glyph
GlyphData* Glyph::data(int i)
{
	if ((i < 0) || (i >= data_.nItems())) Messenger::print( "Tried to get data %i for glyph when it has only %i.", i+1, data_.nItems());
	else return data_[i];
	return NULL;
}

// Set colour
void Glyph::setColour(double r, double g, double b, double a)
{
	for (GlyphData* gd = data_.first(); gd != NULL; gd = gd->next) gd->setColour(r, g, b, a);
}

// Apply stored rotation matrix to supplied matrix, if there is one
bool Glyph::applyRotation(Matrix& A)
{
	if (rotated_) A *= rotation_;
	else return false;
	return true;
}

// Return stored rotation matrix
const Matrix& Glyph::rotation() const
{
	return rotation_;
}

// Set element of rotation matrix
void Glyph::setRotationElement(int el, double d)
{
	rotation_.matrix()[el] = d;
	rotated_ = true;
}

// Get element of rotation matrix
double Glyph::getRotationElement(int el)
{
	return rotation_.matrix()[el];
}

// Reset current rotation matrix
void Glyph::resetRotation()
{
	rotation_.setIdentity();
	rotated_ = false;
}

// Rotate about X axis
void Glyph::rotateX(double angle)
{
	rotation_.applyRotationAxis(1.0, 0.0, 0.0, angle, FALSE);
	rotated_ = true;
}

// Rotate about Y axis
void Glyph::rotateY(double angle)
{
	rotation_.applyRotationAxis(0.0, 1.0, 0.0, angle, FALSE);
	rotated_ = true;
}

// Rotate about Z axis
void Glyph::rotateZ(double angle)
{
	rotation_.applyRotationAxis(0.0, 0.0, 1.0, angle, FALSE);
	rotated_ = true;
}

// Rotate about arbitrary axis
void Glyph::rotate(double x, double y, double z, double angle)
{
	rotation_.applyRotationAxis(x, y, z, angle, TRUE);
	rotated_ = true;
}

// Return whether rotation matrix has been modified
bool Glyph::rotated() const
{
	return rotated_;
}

/*
 * Style
 */

// Set whether the Glyph is selected
void Glyph::setSelected(bool isselected)
{
	selected_ = isselected;
}

// Return whether the Glyph is selected
bool Glyph::isSelected() const
{
	return selected_;
}

// Set whether the Glyph is visible
void Glyph::setVisible(bool isvisible)
{
	visible_ = isvisible;
	parent_->changeLog.add(Log::Glyphs);
}

// Return whether the Glyph is visible
bool Glyph::isVisible() const
{
	return visible_;
}

// Set whether the Glyph is solid or not
void Glyph::setSolid(bool issolid)
{
	solid_ = issolid;
}

// Return whether the Glyph should be drawn as a solid
bool Glyph::isSolid() const
{
	return solid_;
}

