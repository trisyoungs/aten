/*
	*** Rendering glyph
	*** src/base/glyph.cpp
	Copyright T. Youngs 2007-2011

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
#include "base/matrix.h"
#include "classes/prefs.h"
#include "model/model.h"

// Glyph styles
const char *GlyphTypeKeywords[Glyph::nGlyphTypes] = { "arrow", "cube", "ellipsoid", "ellipsoidxyz", "line", "quad", "svector", "sphere", "tetrahedron", "text", "text3d", "triangle", "tubearrow", "tubevector", "vector" };
const char *GlyphTypeNames[Glyph::nGlyphTypes] = { "Arrow", "Cube", "Ellipsoid", "EllipsoidXYZ", "Line", "Quad", "SenseVector", "Sphere", "Tetrahedron", "Text", "Text3D", "Triangle", "TubeArrow", "TubeVector", "Vector" };
int GlyphTypeNData[Glyph::nGlyphTypes] = { 2, 2, 3, 4, 2, 4, 3, 2, 4, 1, 1, 3, 2, 2, 2 };
const char *Glyph::glyphType(Glyph::GlyphType gt)
{
	return GlyphTypeKeywords[gt];
}
const char *Glyph::glyphTypeName(Glyph::GlyphType gt)
{
	return GlyphTypeNames[gt];
}
Glyph::GlyphType Glyph::glyphType(const char *s, bool reportError)
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
const char *GlyphOptionKeywords[Glyph::nGlyphOptions] = { "colour", "solid", "text", "wire" };
const char *Glyph::glyphOption(Glyph::GlyphOption go)
{
	return GlyphOptionKeywords[go];
}
Glyph::GlyphOption Glyph::glyphOption(const char *s, bool reportError)
{
	Glyph::GlyphOption go = (Glyph::GlyphOption) enumSearch("glyph option", Glyph::nGlyphOptions, GlyphOptionKeywords, s);
	if ((go == Glyph::nGlyphOptions) && reportError) enumPrintValid(Glyph::nGlyphOptions,GlyphOptionKeywords);
	return go;
}

/*
 * // GlyphData
 */

// Constructor
GlyphData::GlyphData()
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
void GlyphData::setAtom(Atom *atom)
{
	atom_ = atom;
	set_ = TRUE;
	if (atom_ == NULL)
	{
		msg.print("Info - NULL atom pointer stored in glyph data, so vector data will be used instead.\n");
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
void GlyphData::setAtom(Atom *atom, GlyphData::GlyphDataType av)
{
	setAtom(atom);
	setAtomData(av);
}

// Return the atom pointer
Atom *GlyphData::atom()
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
	msg.print("Warning - Atom pointer is defined NULL *and* glyph data has not been set to use vector data ({0,0,0} returned)...\n");
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
void GlyphData::copyColour(GLfloat *col) const
{
	col[0] = (GLfloat) colour_[0];
	col[1] = (GLfloat) colour_[1];
	col[2] = (GLfloat) colour_[2];
	col[3] = (GLfloat) colour_[3];
}

/*
// Glyph
*/

// Constructor
Glyph::Glyph()
{
	// Private variables
	selected_ = FALSE;
	visible_ = TRUE;
	solid_ = TRUE;
	rotation_ = NULL;
	type_ = Glyph::nGlyphTypes;
	parent_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Assignment operator
void Glyph::operator=(Glyph &source)
{
	// Simple data first
	type_ = source.type_;
	selected_ = source.selected_;
	visible_ = source.visible_;
	solid_ = source.solid_;
	// Rotation matrix (if it exists)
	if (rotation_ != NULL) delete rotation_;
	if (source.rotation_ == NULL) rotation_ = NULL;
	else
	{
		rotation_ = new Matrix();
		*rotation_ = *source.rotation_;
	}
	// Glyph data
	data_.clear();
	data_.createEmpty( Glyph::nGlyphData(type_) );
	for (int n=0; n<Glyph::nGlyphData(type_); ++n) *data_[n] =  *(source.data_[n]);
}	

// Returns the number of data set for the Glyph
int Glyph::nData() const
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
Glyph::GlyphType Glyph::type() const
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
	type_ = gt;
}

// Set text data
void Glyph::setText(const char *s)
{
	text_ = s;
}

// Return text data
const char *Glyph::text() const
{
	return text_.get();
}

// Set colour
void Glyph::setColour(double r, double g, double b, double a)
{
	for (GlyphData *gd = data_.first(); gd != NULL; gd = gd->next) gd->setColour(r, g, b, a);
}

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

// Return whether glyph has been rotated
bool Glyph::rotated() const
{
	return (rotation_ != NULL);
}

// Return rotation matrix
Matrix *Glyph::matrix()
{
	return rotation_;
}

// Set element of rotation matrix
void Glyph::setRotationElement(int el, double d)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	rotation_->matrix()[el] = d;
}

// Get element of rotation matrix
double Glyph::getRotationElement(int el)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	return rotation_->matrix()[el];
}

// Reset (delete) current rotation matrix
void Glyph::resetRotation()
{
	if (rotation_ != NULL) delete rotation_;
	rotation_ = NULL;
}

// Rotate about X axis
void Glyph::rotateX(double angle)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	rotation_->applyRotationAxis(1.0,0.0,0.0,angle,FALSE);
}

// Rotate about Y axis
void Glyph::rotateY(double angle)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	rotation_->applyRotationAxis(0.0,1.0,0.0,angle,FALSE);
}

// Rotate about Z axis
void Glyph::rotateZ(double angle)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	rotation_->applyRotationAxis(0.0,0.0,1.0,angle,FALSE);
}

// Rotate about arbitrary axis
void Glyph::rotate(double x, double y, double z, double angle)
{
	if (rotation_ == NULL) rotation_ = new Matrix;
	rotation_->applyRotationAxis(x, y, z, angle, TRUE);
}
