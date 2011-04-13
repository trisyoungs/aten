/*
	*** Vertex Chunk
	*** src/render/vertexchunk.cpp
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

//#define GL_GLEXT_PROTOTYPES
#include "render/vertexchunk.h"
//#include "base/messenger.h"
//#include "base/constants.h"
//#include "classes/prefs.h"
//#include "gui/tcanvas.uih"
//#include <QtOpenGL/QGLWidget>
#include <stdio.h>
#include <math.h>

// Constructor
VertexChunk::VertexChunk()
{
	// Public variables
	next = NULL;
	prev = NULL;
	
	// Private variables
	vertexData_ = NULL;
	centroids_ = NULL;
	verticesPerType_ = 0;
	dataPerVertex_ = 0;
	nDefinedVertices_ = 0;
	maxVertices_ = -1;
	nDefinedTypes_ = 0;
	type_ = GL_TRIANGLES;
}

// Destructor
VertexChunk::~VertexChunk()
{
	if (vertexData_ != NULL) delete[] vertexData_;
	if (centroids_ != NULL) delete[] centroids_;
}

// Update (or finalise) centroid for current primitive type
void VertexChunk::updateCentroid(GLfloat x, GLfloat y, GLfloat z, bool finalise)
{
	// Accumulate centroid
	int coff = nDefinedTypes_*3;
	centroids_[coff] += x;
	centroids_[coff+1] += y;
	centroids_[coff+2] += z;
	// Finalise centroid?
	if (finalise)
	{
		centroids_[coff] /= verticesPerType_;
		centroids_[coff+1] /= verticesPerType_;
		centroids_[coff+2] /= verticesPerType_;	
	}
}

// Initialise structure
void VertexChunk::initialise(GLenum type, bool colourData)
{
	type_ = type;
	dataPerVertex_ = (colourData ? 10 : 6);
	if (type_ == GL_TRIANGLES) verticesPerType_ = 3;
	else if (type_ == GL_LINES) verticesPerType_ = 2;
	else if (type_ == GL_POINTS) verticesPerType_ = 1;
	else printf("Warning - Invalid GLenum type given to VertexChunk::initialise (%i)\n", type_);
	maxVertices_ = VERTEXCHUNKSIZE*verticesPerType_;
	nDefinedVertices_ = 0;
	nDefinedTypes_ = 0;
	vertexData_ = new GLfloat[maxVertices_*dataPerVertex_];
	centroids_ = new GLfloat[VERTEXCHUNKSIZE*3];
	for (int n=0; n<VERTEXCHUNKSIZE*3; ++n) centroids_[n] = 0.0f;
}

// Define next vertex and normal
void VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid)
{
	if (nDefinedVertices_ == maxVertices_) printf("Internal Error: Vertex limit for VertexChunk reached.\n");
	int index = nDefinedVertices_*dataPerVertex_;
	if (dataPerVertex_ == 10)
	{
		printf("Internal Error: No colour specified in vertex creation, but the primitive requires one.\n");
		index += 4;
	}
	// Store normal
	vertexData_[index++] = nx;
	vertexData_[index++] = ny;
	vertexData_[index++] = nz;
	// Store vertex
	vertexData_[index++] = x;
	vertexData_[index++] = y;
	vertexData_[index++] = z;
	// Increase vertex counter
	++nDefinedVertices_;
	// Update centroid
	bool finalise = (nDefinedVertices_%verticesPerType_) == 0;
	if (calcCentroid) updateCentroid(x, y, z, finalise);
	if (finalise) ++nDefinedTypes_;
}

// Define next vertex and normal with specific colour (as array)
void VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid)
{
	if (nDefinedVertices_ == maxVertices_) printf("Internal Error: Vertex limit for VertexChunk reached.\n");
	int index = nDefinedVertices_*dataPerVertex_;
	// Store colour
	if (dataPerVertex_ != 10) printf("Internal Error: Colour specified in vertex creation, but it is not required for primitive.\n");
	else
	{
		vertexData_[index++] = colour[0];
		vertexData_[index++] = colour[1];
		vertexData_[index++] = colour[2];
		vertexData_[index++] = colour[3];
	}
	// Store normal
	vertexData_[index++] = nx;
	vertexData_[index++] = ny;
	vertexData_[index++] = nz;
	// Store vertex
	vertexData_[index++] = x;
	vertexData_[index++] = y;
	vertexData_[index++] = z;
	// Increase vertex counter
	++nDefinedVertices_;
	// Update centroid
	bool finalise = (nDefinedVertices_%verticesPerType_) == 0;
	if (calcCentroid) updateCentroid(x, y, z, finalise);
	if (finalise) ++nDefinedTypes_;
}

// Define next vertex and normal with specific colour
void VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid)
{
	if (nDefinedVertices_ == maxVertices_) printf("Internal Error: Vertex limit for VertexChunk reached.\n");
	int index = nDefinedVertices_*dataPerVertex_;
	// Store colour
	if (dataPerVertex_ != 10) printf("Internal Error: Colour specified in vertex creation, but it is not required for primitive.\n");
	else
	{
		vertexData_[index++] = r;
		vertexData_[index++] = g;
		vertexData_[index++] = b;
		vertexData_[index++] = a;
	}
	// Store normal
	vertexData_[index++] = nx;
	vertexData_[index++] = ny;
	vertexData_[index++] = nz;
	// Store vertex
	vertexData_[index++] = x;
	vertexData_[index++] = y;
	vertexData_[index++] = z;
	// Increase vertex counter
	++nDefinedVertices_;
	// Update centroid
	bool finalise = (nDefinedVertices_%verticesPerType_) == 0;
	if (calcCentroid) updateCentroid(x, y, z, finalise);
	if (finalise) ++nDefinedTypes_;
}

// Return whether current array is full
bool VertexChunk::full()
{
	return (nDefinedVertices_ == maxVertices_);
}

// Forget all vertex data currently stored in array (but retain array)
void VertexChunk::forgetAll()
{
	nDefinedTypes_ = 0;
	nDefinedVertices_ = 0;
	for (int n=0; n<VERTEXCHUNKSIZE*3; ++n) centroids_[n] = 0.0f;
}

// Return number of defined primitive (GL) types
int VertexChunk::nDefinedTypes()
{
	return nDefinedTypes_;
}

// Return vertex array
GLfloat *VertexChunk::vertexData()
{
	return vertexData_;
}

// Return centroid array
GLfloat *VertexChunk::centroids()
{
	return centroids_;
}

// Return number of defined vertices in chunk
int VertexChunk::nDefinedVertices()
{
	return nDefinedVertices_;
}

// Send to OpenGL (i.e. render)
void VertexChunk::sendToGL()
{
	if (nDefinedVertices_ == 0) return;
	// Does the vertex data contain colour-per-vertex information?
	glInterleavedArrays(dataPerVertex_ == 10 ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, vertexData_);
	glDrawArrays(type_, 0, nDefinedVertices_);
}

