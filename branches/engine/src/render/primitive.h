/*
	*** Rendering Primitive
	*** src/render/primitive.h
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

#ifndef ATEN_PRIMITIVE_H
#define ATEN_PRIMITIVE_H

#include <GL/gl.h>
#include "templates/vector3.h"

// Rendering Primitive
class Primitive
{
	public:
	// Constructor / Destructor
	Primitive();
	~Primitive();

	private:
	// Primitive's vertex and scaled vertex array
	GLfloat *vertices_;
	// Primitive's normal array
	GLfloat *normals_;
	// Number of vertices in primitive
	int nVertices_;
	// Number of defined vertices (and normals) so far (when using createEmpty())
	int nDefinedVertices_;
	// GL object drawing method
	GLenum type_;

	public:
	// Clear existing data
	void clear();
	// Create empty data arrays, setting type specified
	void createEmpty(GLenum type, int nvertices);
	// Define next vertex and normal
	void addVertexAndNormal(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz);
	// Create vertices of sphere with specified radius and quality
	void createSphere(double radius, int nstacks, int nslices);
	// Create vertices of cylinder along z with specified radius, length, and quality
	void createCylinder(double startradius, double endradius, double length, int nstacks, int nslices);
	// Create vertices of cross with specified width
	void createCross(double width, int naxes);
	// Send to OpenGL (i.e. render)
	void sendToGL();
};

// Primitive Info
class PrimitiveInfo
{
	public:
	// Constructor
	PrimitiveInfo();
	// List pointer
	PrimitiveInfo *prev, *next;

	private:
	// Target primitive
	Primitive *primitive_;
	// Local coordinates of primitive
	Vec3<double> localCoords_;
	// Local transformation of primitive (if defined)
	GLdouble localTransform_[16];
	// Flag to specify whether local transform of primitive has been defined
	bool transformDefined_;
	// Ambient colour
	GLfloat ambient_[4];
	// Diffuse colour
	GLfloat diffuse_[4];
	
	public:
	// Set primitive info data
	void set(Primitive *prim, GLfloat *ambient, GLfloat *diffuse, Vec3<double> &coords);
	// Set primitive info data, including local rotation
	void set(Primitive *prim, GLfloat *ambient, GLfloat *diffuse, Vec3<double> &coords, GLdouble *transform);
};

// Primitive Group
class PrimitiveGroup
{
	public:
	// Constructor
	PrimitiveGroup();
	~PrimitiveGroup();

	private:
	// Array of Primitives, corresponding to different levels of detail
	Primitive *primitives_;
	// Number of primitives in array (copied from Prefs)
	int nPrimitives_;

	public:
	// Clear old primitives array and allocate new one
	void clear();
	// Return primitive corresponding to level of detail specified
	Primitive &primitive(int lod);
	// Send to OpenGL (i.e. render) at specified level of detail
	void sendToGL(int lod);
};

#endif
