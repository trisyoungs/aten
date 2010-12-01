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
#include "gui/tcanvas.uih"
#include <stdio.h>
#include <math.h>

/*
// Vertex Chunk
*/

// Constructor / Destructor
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

VertexChunk::~VertexChunk()
{
	if (vertexData_ != NULL) delete[] vertexData_;
	if (centroids_ != NULL) delete[] centroids_;
}

// Initialise structure
void VertexChunk::initialise(GLenum type, bool colourData)
{
	type_ = type;
	dataPerVertex_ = (colourData ? 10 : 6);
	verticesPerType_ = (type_ == GL_TRIANGLES ? 3 : 2);
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
	// Accumulate centroid
	if (calcCentroid)
	{
		centroids_[nDefinedTypes_*3] += x;
		centroids_[nDefinedTypes_*3+1] += y;
		centroids_[nDefinedTypes_*3+2] += z;
	}
	// Increase vertex counter
	++nDefinedVertices_;
	// Increase type counter
	if ((nDefinedVertices_%verticesPerType_) == 0)
	{
		// Finalise centroid 
		if (calcCentroid)
		{
			centroids_[nDefinedTypes_*3] /= verticesPerType_;
			centroids_[nDefinedTypes_*3+1] /= verticesPerType_;
			centroids_[nDefinedTypes_*3+2] /= verticesPerType_;
		}
		++nDefinedTypes_;
	}
}

// Define next vertex and normal
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
	// Accumulate centroid
	if (calcCentroid)
	{
		centroids_[nDefinedTypes_*3] += x;
		centroids_[nDefinedTypes_*3+1] += y;
		centroids_[nDefinedTypes_*3+2] += z;
	}
	// Increase vertex counter
	++nDefinedVertices_;
	// Increase type counter
	if ((nDefinedVertices_%verticesPerType_) == 0)
	{
		// Finalise centroid 
		if (calcCentroid)
		{
			centroids_[nDefinedTypes_*3] /= verticesPerType_;
			centroids_[nDefinedTypes_*3+1] /= verticesPerType_;
			centroids_[nDefinedTypes_*3+2] /= verticesPerType_;
		}
		++nDefinedTypes_;
	}
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

// Send to OpenGL (i.e. render)
void VertexChunk::sendToGL()
{
	if (nDefinedVertices_ == 0) return;
	// Does the vertex data contain colour-per-vertex information?
	glInterleavedArrays(dataPerVertex_ == 10 ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, vertexData_);
	glDrawArrays(type_, 0, nDefinedVertices_);
}

/*
// Primitive
*/

// Constructor
Primitive::Primitive()
{
	currentVertexChunk_ = NULL;
	colouredVertexData_ = FALSE;
	type_ = GL_TRIANGLES;
	prev = NULL;
	next = NULL;
}

// Destructor
Primitive::~Primitive()
{
	clear();
}

// Flag that primitive should contain colour data information for each vertex
void Primitive::setColourData()
{
	colouredVertexData_ = TRUE;
}

// Clear existing data
void Primitive::clear()
{
	vertexChunks_.clear();
	currentVertexChunk_ = NULL;
}

// Forget all data, leaving arrays intact
void Primitive::forgetAll()
{
	for (VertexChunk *v = vertexChunks_.first(); v != NULL; v = v->next) v->forgetAll();
	currentVertexChunk_ = vertexChunks_.first();
}

/*
// Primitive Generation
*/

// Create vertices of cross with specified width
void Primitive::createCross(double width, int naxes)
{
	int i,j,count, limit = max(1,naxes);
	GLfloat vert[3], norm[3];

	// Clear existing data first (if it exists)
	type_ = GL_LINES;
	forgetAll();
	

	count = 0;
	for (i=0; i<limit; ++i)
	{
		for (j=0; j<3; ++j)
		{
			vert[j] = (i == j ? width : 0.0);
			norm[j] = (j == 0 ? 1.0 : 0.0);
		}
		defineVertex(vert[0], vert[1], vert[2], norm[0], norm[1], norm[2]);
		vert[i] = -vert[i];
		defineVertex(vert[0], vert[1], vert[2], norm[0], norm[1], norm[2]);
	}
}

// Create wireframe cube centred at zero
void Primitive::createWireCube(double size)
{
	// Clear existing data first (if it exists)
	type_ = GL_LINES;
	forgetAll();
	
	size = 0.5*size;
	int i, j;
	GLfloat r[3];
	// Set initial corner
	r[0] = -size;
	r[1] = -size;
	r[2] = -size;
	for (i=0; i<4; ++i)
	{
		// Swap signs to generate new corner if necessary
		if (i>0)
		{
			r[1] = -r[1];
			if (i == 2) r[2] = -r[2];
			else r[0] = -r[0];
		}
		// Generate lines
		for (j=0; j<3; ++j)
		{
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0);
			defineVertex(j == 0 ? -r[0] : r[0], j == 1 ? -r[1] : r[1], j == 2 ? -r[2] : r[2], 1.0, 0.0, 0.0);	
		}
	}
}

// Create solid cube of specified size, centred at zero, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
void Primitive::createCube(double size, int nsubs)
{
	// Clear existing data first (if it exists)
	forgetAll();
	
	// Create each face individually
	GLfloat origin, delta = (GLfloat) size/nsubs, veca[3], vecb[3], vertex[3];
	int i, j, plane;
	// Set general origin coordinate
	origin = -0.5*size;
	// Loop over planes
	for (plane=0; plane<3; ++plane)
	{
		// Define deltas for this plane
		for (j=0; j<3; ++j)
		{
			veca[j] = 0.0;
			vecb[j] = 0.0;
		}
		veca[(plane+1)%3] = delta;
		vecb[(plane+2)%3] = delta;
		// Loop over subdivisions in plane
		for (i=0; i<nsubs; ++i)
		{
			for (j=0; j<nsubs; ++j)
			{
				vertex[0] = origin + i*veca[0] + j*vecb[0];
				vertex[1] = origin + i*veca[1] + j*vecb[1];
				vertex[2] = origin + i*veca[2] + j*vecb[2];
				// Define trangle vertices for 'lower' plane
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0], vertex[1]+veca[1], vertex[2]+veca[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+vecb[0], vertex[1]+vecb[1], vertex[2]+vecb[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2);

				// Define trangle vertices for 'upper' plane
				vertex[plane] += size;
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0], vertex[1]+veca[1], vertex[2]+veca[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+vecb[0], vertex[1]+vecb[1], vertex[2]+vecb[2], plane == 0, plane == 1, plane == 2);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2);
			}
		}
	}
	
}

// Create cell axes
void Primitive::createCellAxes()
{
	int nstacks = max(3,(int) (prefs.primitiveQuality()*0.75));
	int nslices = max(3,(int) (prefs.primitiveQuality()*1.5));

	// Clear existing data first (if it exists) - need enough space for 6 cylinders
	forgetAll();
	
	// X axis
	plotCylinder(0.0, 0.0, 0.0, 0.65, 0.0, 0.0, 0.1, 0.1, nstacks, nslices);
	plotCylinder(0.65, 0.0, 0.0, 0.35, 0.0, 0.0, 0.2, 0.0, nstacks, nslices);

	// Y axis
	plotCylinder(0.0, 0.0, 0.0, 0.0, 0.65, 0.0, 0.1, 0.1, nstacks, nslices);
	plotCylinder(0.0, 0.65, 0.0, 0.0, 0.35, 0.0, 0.2, 0.0, nstacks, nslices);
	
	// Z axis
	plotCylinder(0.0, 0.0, 0.0, 0.0, 0.0, 0.65, 0.1, 0.1, nstacks, nslices);
	plotCylinder(0.0, 0.0, 0.65, 0.0, 0.0, 0.35, 0.2, 0.0, nstacks, nslices);
	
}

// Create rotation globe axes
void Primitive::createRotationGlobeAxes(int nstacks, int nslices)
{
	// Create space for one sphere and three cylinders
	forgetAll();
	
	// Axis pointers
	plotCylinder(0.7, 0.0, 0.0, 0.3, 0.0, 0.0, 0.2, 0.0, nstacks, nslices);
	plotCylinder(0.0, 0.7, 0.0, 0.0, 0.3, 0.0, 0.2, 0.0, nstacks, nslices);
	plotCylinder(0.0, 0.0, 0.7, 0.0, 0.0, 0.3, 0.2, 0.0, nstacks, nslices);
}

// Return first chunk vertex array
VertexChunk *Primitive::vertexChunks()
{
	return vertexChunks_.first();;
}

// Return whether vertex data contains colour information
bool Primitive::colouredVertexData()
{
	return colouredVertexData_;
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL()
{
	for (VertexChunk *chunk = vertexChunks_.first(); chunk != NULL; chunk = chunk->next) chunk->sendToGL();
}

/*
// Primitive Info
*/

// Constructor
PrimitiveInfo::PrimitiveInfo()
{
	// Private variables
	primitive_ = NULL;
	fillMode_ = GL_FILL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set primitive info data
void PrimitiveInfo::set(Primitive* prim, GLfloat* colour, Matrix& transform, GLenum fillMode)
{
	primitive_ = prim;
	localTransform_ = transform;
	fillMode_ = fillMode;
	for (int n=0; n<4; ++n) colour_[n] = colour[n];
}

// Return pointer to primitive
Primitive *PrimitiveInfo::primitive()
{
	return primitive_;
}

// Return local transformation of primitive
Matrix &PrimitiveInfo::localTransform()
{
	return localTransform_;
}

// Return colour array
GLfloat *PrimitiveInfo::colour()
{
	return colour_;
}

// Return polygon fill mode
GLenum PrimitiveInfo::fillMode()
{
	return fillMode_;
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
	// Clamp LOD to allowable range
	if (lod < 0) return primitives_[0];
	else if (lod >= nPrimitives_) return primitives_[nPrimitives_-1];
	else return primitives_[lod];
}

// Send to OpenGL (i.e. render) at specified level of detail
void PrimitiveGroup::sendToGL(int lod)
{
	// Clamp lod to allowable range for this PrimitiveGroup
	if (lod < 0) lod = 0;
	else if (lod >= nPrimitives_) lod = nPrimitives_-1;
	primitives_[lod].sendToGL();
}

/*
// Grid Primitive
*/

// Constructor
GridPrimitive::GridPrimitive()
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	source_ = NULL;
	renderPoint_ = -1;
}

// Return primitive
Primitive &GridPrimitive::primitive()
{
	return primitive_;
}

// Return source grid pointer
Grid *GridPrimitive::source()
{
	return source_;
}

// Generate (or update) from supplied grid pointer
void GridPrimitive::createSurface(Grid *g)
{
}
