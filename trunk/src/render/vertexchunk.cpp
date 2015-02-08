/*
	*** Vertex Chunk
	*** src/render/vertexchunk.cpp
	Copyright T. Youngs 2013-2014

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

#include "render/vertexchunk.h"
#include <stdio.h>
#include <math.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#endif

// Constructor
VertexChunk::VertexChunk()
{
	// Public variables
	next = NULL;
	prev = NULL;
	
	// Private variables
	vertexData_ = NULL;
	indexData_ = NULL;
	centroids_ = NULL;
	verticesPerType_ = 0;
	dataPerVertex_ = 0;
	nDefinedVertices_ = 0;
	nDefinedIndices_ = 0;
	maxVertices_ = 0;
	maxIndices_ = 0;
	hasIndices_ = false;
	maxTypes_ = 0;
	nDefinedTypes_ = 0;
	calcCentroids_ = false;
	type_ = GL_TRIANGLES;
}

// Destructor
VertexChunk::~VertexChunk()
{
	if (vertexData_ != NULL) delete[] vertexData_;
	if (indexData_ != NULL) delete[] indexData_;
	if (centroids_ != NULL) delete[] centroids_;
}

// Update (or finalise) centroid for current primitive type
void VertexChunk::updateCentroid(GLfloat x, GLfloat y, GLfloat z)
{
	if (!calcCentroids_) return;

	// Accumulate centroid
	int coff = nDefinedTypes_*3;
	centroids_[coff] += x;
	centroids_[coff+1] += y;
	centroids_[coff+2] += z;

	// Finalise centroid?
	if ((nDefinedVertices_%verticesPerType_) == 0)
	{
		centroids_[coff] /= verticesPerType_;
		centroids_[coff+1] /= verticesPerType_;
		centroids_[coff+2] /= verticesPerType_;	
		++nDefinedTypes_;
	}
}

// Initialise structure
void VertexChunk::initialise(int newMaxVertices, int newMaxIndices, GLenum type, bool colourData)
{
	type_ = type;
	dataPerVertex_ = (colourData ? 10 : 6);
	if (type_ == GL_TRIANGLES) verticesPerType_ = 3;
	else if ((type_ == GL_LINES) || (type_ == GL_LINE_LOOP) || (type_ == GL_LINE_STRIP)) verticesPerType_ = 2;
	else if (type_ == GL_POINTS) verticesPerType_ = 1;
	else printf("Warning - Invalid GLenum type given to VertexChunk::initialise (%i)\n", type_);

	// (Re)create vertexData_ array if it doesn't currently exist or is too small
	if ((!vertexData_) || (newMaxVertices > maxVertices_))
	{
		if (vertexData_) delete[] vertexData_;
		maxVertices_ = newMaxVertices;
		vertexData_ = new GLfloat[maxVertices_*dataPerVertex_];
	}

	// (Re)create indexData_ array if it doesn't currently exist or is too small
	if ((!indexData_) || (newMaxIndices > maxIndices_))
	{
		if (indexData_) delete[] indexData_;
		maxIndices_ = newMaxIndices;
		indexData_ = new GLuint[maxIndices_];
	}
	hasIndices_ = (newMaxIndices > 0);

	// (Re)create centroids_ array if it doesn't currently exist or is too small
	if (newMaxIndices == 0)
	{
		calcCentroids_ = true;
		if ((!centroids_) || ((maxVertices_/verticesPerType_) > maxTypes_))
		{
			if (centroids_) delete[] centroids_;
			maxTypes_ = maxVertices_/verticesPerType_;
			centroids_ = new GLfloat[maxTypes_*3];
		}
		for (int n=0; n<maxTypes_*3; ++n) centroids_[n] = 0.0f;
	}
	else calcCentroids_ = false;

	nDefinedVertices_ = 0;
	nDefinedIndices_ = 0;
	nDefinedTypes_ = 0;
}

// Define next vertex and normal
GLuint VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz)
{
	if (nDefinedVertices_ >= maxVertices_)
	{
		printf("Internal Error: Vertex limit for VertexChunk reached.\n");
		return -1;
	}
	int index = nDefinedVertices_*dataPerVertex_;
	if (dataPerVertex_ == 10)
	{
		printf("Internal Error: No colour given to defineVertex(), but the primitive requires one.\n");
		return -1;
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
	if (calcCentroids_) updateCentroid(x, y, z);

	// Return index of vertex
	return (nDefinedVertices_-1);
}

// Define next vertex and normal with specific colour (as array)
GLuint VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat* colour)
{
	if (nDefinedVertices_ >= maxVertices_)
	{
		printf("Internal Error: Vertex limit for VertexChunk reached.\n");
		return -1;
	}
	int index = nDefinedVertices_*dataPerVertex_;

	// Store colour
	if (dataPerVertex_ != 10)
	{
		printf("Internal Error: Colour specified in vertex creation, but it is not required for primitive.\n");
		return -1;
	}
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
	updateCentroid(x, y, z);

	// Return index of vertex
	return (nDefinedVertices_-1);
}

// Define next vertex and normal with specific colour
GLuint VertexChunk::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	if (nDefinedVertices_ >= maxVertices_)
	{
		printf("Internal Error: Vertex limit for VertexChunk reached.\n");
		return -1;
	}
	int index = nDefinedVertices_*dataPerVertex_;

	// Store colour
	if (dataPerVertex_ != 10)
	{
		printf("Internal Error: Colour specified in vertex creation, but it is not required for primitive.\n");
		return -1;
	}
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
	updateCentroid(x, y, z);

	// Return index of vertex
	return (nDefinedVertices_-1);
}

// Define index doublet
bool VertexChunk::defineIndices(GLuint a, GLuint b)
{
	if ((maxIndices_-nDefinedIndices_) < 2)
	{
		printf("Internal Error: Index limit for IndexChunk reached.\n");
		return false;
	}
	// Store indices
	indexData_[nDefinedIndices_++] = a;
	indexData_[nDefinedIndices_++] = b;
	return true;
}

// Define next triplet of indices
bool VertexChunk::defineIndices(GLuint a, GLuint b, GLuint c)
{
	if ((maxIndices_-nDefinedIndices_) < 3)
	{
		printf("Internal Error: Index limit for IndexChunk reached.\n");
		return false;
	}
	// Store indices
	indexData_[nDefinedIndices_++] = a;
	indexData_[nDefinedIndices_++] = b;
	indexData_[nDefinedIndices_++] = c;
	return true;
}

// Return whether current array is full
bool VertexChunk::full() const
{
	return (nDefinedVertices_ == maxVertices_);
}

// Return whether indices are being used
bool VertexChunk::hasIndices() const
{
	return hasIndices_;
}

// Forget all vertex data currently stored in array (but retain array)
void VertexChunk::forgetAll()
{
	nDefinedTypes_ = 0;
	nDefinedVertices_ = 0;
	nDefinedIndices_ = 0;

	// Clear centroid array if we were using it
	if (calcCentroids_ && centroids_) for (int n=0; n<maxTypes_; ++n) centroids_[n] = 0.0f;
}

// Return number of vertices defined
int VertexChunk::nDefinedVertices() const
{
	return nDefinedVertices_;
}

// Return number of indices defined
int VertexChunk::nDefinedIndices() const
{
	return nDefinedIndices_;
}

// Return number of defined primitive (GL) types
int VertexChunk::nDefinedTypes() const
{
	return nDefinedTypes_;
}

// Return vertex array
const GLfloat *VertexChunk::vertexData()
{
	return vertexData_;
}

// Return centroid array
const GLfloat *VertexChunk::centroids()
{
	return centroids_;
}

// Return index array
const GLuint *VertexChunk::indexData()
{
	return indexData_;
}

// Send to GL
void VertexChunk::sendToGL() const
{
	// Check vertex count
	if (nDefinedVertices_ == 0) return;

	// Does the vertex data contain colour-per-vertex information?
	glInterleavedArrays(dataPerVertex_ == 10 ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, vertexData_);

	// Check if we are using indices
	if (hasIndices_) glDrawElements(type_, nDefinedIndices_, GL_UNSIGNED_INT, indexData_);
	else glDrawArrays(type_, 0, nDefinedVertices_);
}
