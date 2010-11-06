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

// Rendering Primitive
class Primitive
{
	public:
	// Constructor / Destructor
	Primitive();
	~Primitive();

	private:
	// Primitive's vertex and scaled vertex array
	GLfloat *vertices_, *scaledVertices_;
	// Primitive's normal array
	GLfloat *normals_;
	// Number of vertices in primitive
	int nVertices_;
	// Number of defined vertices (and normals) so far (when using createEmpty())
	int nDefinedVertices_;
	// GL object drawing method
	GLenum type_;
	// Number of vertices per GL object
	int nVerticesPerObject_;

	public:
	// Clear existing data
	void clear();
	// Create empty data arrays, setting type specified
	void createEmpty(GLenum type, int nvertsperobject, int nvertices);
	// Define next vertex and normal
	void addVertexAndNormal(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz);
	// Create vertices of sphere with specified radius and quality
	void createSphere(double radius, int nstacks, int nslices);
	// Send to OpenGL (i.e. render)
	void sendToGL();
	// Send to OpenGL (i.e. render) with all vertex values scaled
	void sendScaledToGL(GLfloat scale);
};

#endif
