/*
	*** Rendering Primitive
	*** src/render/primitive.cpp
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

#include "render/primitive.h"
#include "base/messenger.h"
#include "base/constants.h"
#include "classes/prefs.h"
#include <stdio.h>
#include <math.h>

// Constructor
Primitive::Primitive()
{
	vertices_ = NULL;
	normals_ = NULL;
	nVertices_ = 0;
	nDefinedVertices_ = 0;
	type_ = GL_TRIANGLES;
}

// Destructor
Primitive::~Primitive()
{
	clear();
}

// Clear existing data
void Primitive::clear()
{
	if (vertices_ != NULL) delete[] vertices_;
	vertices_ = NULL;
	if (normals_ != NULL) delete[] normals_;
	normals_ = NULL;
}

// Create empty data arrays, setting type specified
void Primitive::createEmpty(GLenum type, int nvertices)
{
	// Clear old data, if any
	clear();
	nVertices_ = nvertices;
	nDefinedVertices_ = 0;
	type_ = type;
	vertices_ = new GLfloat[nVertices_*3];
	normals_ = new GLfloat[nVertices_*3];
}

// Define next vertex and normal
void Primitive::addVertexAndNormal(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz)
{
	// Define next vertex in primitive
	vertices_[nDefinedVertices_*3] = x;
	vertices_[nDefinedVertices_*3+1] = y;
	vertices_[nDefinedVertices_*3+2] = z;
	// Define normal
	normals_[nDefinedVertices_*3] = nx;
	normals_[nDefinedVertices_*3+1] = ny;
	normals_[nDefinedVertices_*3+2] = nz;
	// Increase vertex counter
	++nDefinedVertices_;
}

// Create vertices of sphere with specified radius and quality
void Primitive::createSphere(double radius, int nstacks, int nslices)
{
	msg.enter("Primitive::createSphere");
	int i, j, count;
	double stack0, stack1, z0, zr0, z1, zr1, slice0, slice1, x0, y0, x1, y1;

	// Clear existing data first (if it exists)
	clear();

	nVertices_ = 3*nstacks*nslices*2;
	vertices_ = new GLfloat[nVertices_*3];
	normals_ = new GLfloat[nVertices_*3];
	type_ = GL_TRIANGLES;
	count = 0;
	for (i = nstacks/2; i < nstacks; i++)
	{
		stack0 = PI * (-0.5 + (double) i / nstacks);
		z0  = sin(stack0);
		zr0 = cos(stack0);

		stack1 = PI * (-0.5 + (double) (i+1) / nstacks);
		z1 = sin(stack1);
		zr1 = cos(stack1);

		for (j = 0; j < nslices; j++)
		{
			slice0 = 2 * PI * (double) j / nslices;
			x0 = cos(slice0);
			y0 = sin(slice0);

			slice1 = 2 * PI * (double) (j+1) / nslices;
			x1 = cos(slice1);
			y1 = sin(slice1);

			// First triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			vertices_[count] = x0 * zr0 * radius;
			normals_[count++] = x0 * zr0;
			vertices_[count] = y0 * zr0 * radius;
			normals_[count++] = y0 * zr0;
			vertices_[count] = z0 * radius;
			normals_[count++] = z0;
			vertices_[count] = x0 * zr1 * radius;
			normals_[count++] = x0 * zr1;
			vertices_[count] = y0 * zr1 * radius;
			normals_[count++] = y0 * zr1;
			vertices_[count] = z1 * radius;
			normals_[count++] = z1;
			vertices_[count] = x1 * zr0 * radius;
			normals_[count++] = x1 * zr0;
			vertices_[count] = y1 * zr0 * radius;
			normals_[count++] = y1 * zr0;
			vertices_[count] = z0 * radius;
			normals_[count++] = z0;
			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			vertices_[count] = x0 * zr1 * radius;
			normals_[count++] = x0 * zr1;
			vertices_[count] = y0 * zr1 * radius;
			normals_[count++] = y0 * zr1;
			vertices_[count] = z1 * radius;
			normals_[count++] = z1;
			vertices_[count] = x1 * zr0 * radius;
			normals_[count++] = x1 * zr0;
			vertices_[count] = y1 * zr0 * radius;
			normals_[count++] = y1 * zr0;
			vertices_[count] = z0 * radius;
			normals_[count++] = z0;
			vertices_[count] = x1 * zr1 * radius;
			normals_[count++] = x1 * zr1;
			vertices_[count] = y1 * zr1 * radius;
			normals_[count++] = y1 * zr1;
			vertices_[count] = z1 * radius;
			normals_[count++] = z1;
			if (count > nVertices_*3) printf("MISCALCULATED!!\n");
		}
	}
	msg.exit("Primitive::createSphere");
}

// Create vertices of cross with specified width
void Primitive::createCross(double width, int naxes)
{
	int i,j,count;
	// Clear existing data first (if it exists)
	clear();

	nVertices_ = 6;
	vertices_ = new GLfloat[nVertices_*3];
	normals_ = new GLfloat[nVertices_*3];
	type_ = GL_LINES;
	count = 0;

	for (i=0; i<max(1,naxes); ++i)
	{
		for (j=0; j<3; ++j)
		{
			vertices_[count] = (i == j ? width : 0.0);
			normals_[count++] = (j == 0 ? 1.0 : 0.0);
		}
		for (j=0; j<3; ++j)
		{
			vertices_[count] = (i == j ? -width : 0.0);
			normals_[count++] = (j == 0 ? 1.0 : 0.0);
		}
	}
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL()
{
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glVertexPointer(3, GL_FLOAT, 0, vertices_);
	glNormalPointer(GL_FLOAT, 0, normals_);
	glDrawArrays(type_, 0, nVertices_);
	glDisableClientState(GL_VERTEX_ARRAY);
}

/*
// Primitive Group
*/

// Constructor
PrimitiveGroup::PrimitiveGroup()
{
	primitives_ = NULL;
	nPrimitives_ = 0;

	clear();
}

PrimitiveGroup::~PrimitiveGroup()
{
	if (primitives_ != NULL) delete[] primitives_;
	nPrimitives_ = 0;
}

// Clear old primitives array and allocate new one
void PrimitiveGroup::clear()
{
	if (primitives_ != NULL) delete[] primitives_;
	nPrimitives_ = prefs.levelsOfDetail();
	primitives_ = new Primitive[nPrimitives_];
}

// Return primitive corresponding to level of detail specified
Primitive &PrimitiveGroup::primitive(int lod)
{
	if ((lod < 0) || (lod >= nPrimitives_)) printf("Level of detail is out of range for PrimitiveGroup\n");
	else return primitives_[lod];
	return primitives_[0];
}

// Send to OpenGL (i.e. render) at specified level of detail
void PrimitiveGroup::sendToGL(int lod)
{
	// Clamp lod to allowable range for this PrimitiveGroup
	if (lod < 0) lod = 0;
	else if (lod >= nPrimitives_) lod = nPrimitives_-1;
	primitives_[lod].sendToGL();
}
