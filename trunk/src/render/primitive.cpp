/*
	*** Rendering Primitive
	*** src/render/primitive.cpp
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

#define GL_GLEXT_PROTOTYPES
#include "render/primitive.h"
#include "base/messenger.h"
#include "base/constants.h"
#include "classes/prefs.h"
#include "gui/tcanvas.uih"
#include <QtOpenGL/QGLWidget>
#include <stdio.h>
#include <math.h>

#define BUFFER_OFFSET(i) ((char *)NULL + (i*sizeof(GLfloat)))

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
	idVBO_ = 0;
	hasVBO_ = FALSE;
	nDefinedVertices_ = 0;
}

// Destructor
Primitive::~Primitive()
{
	clear();
}

// Flag that primitive should contain colour data information for each vertex
void Primitive::setColourData(bool b)
{
	colouredVertexData_ = b;
}

// Clear existing data
void Primitive::clear()
{
	vertexChunks_.clear();
	currentVertexChunk_ = NULL;
	nDefinedVertices_ = 0;
}

// Forget all data, leaving arrays intact
void Primitive::forgetAll()
{
	for (VertexChunk *v = vertexChunks_.first(); v != NULL; v = v->next) v->forgetAll();
	currentVertexChunk_ = vertexChunks_.first();
	nDefinedVertices_ = 0;
}

// Set GL drawing primitive type
void Primitive::setType(GLenum type)
{
	type_ = type;
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

// Create wireframe, crossed cube centred at zero
void Primitive::createCrossedCube(double size)
{
	// Create wire cube to start with
	createWireCube(size);
	// Add crosses to faces
	int i, j, sign;
	GLfloat r[3];
	for (i=0; i<3; ++i)
	{
		for (sign = 1; sign > -2; sign -=2)
		{
			// Determine single coordinate on positive face from which to determine all others
			for (j = 0; j<3; ++j) r[j] = (j == i ? 0.55*size*sign : 0.4*size);
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0);
			r[(i+1)%3] = -r[(i+1)%3];
			r[(i+2)%3] = -r[(i+2)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0);
			r[(i+1)%3] = -r[(i+1)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0);
			r[(i+1)%3] = -r[(i+1)%3];
			r[(i+2)%3] = -r[(i+2)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0);
		}
	}
}

// Create solid cube of specified size, centred at zero, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
void Primitive::createCube(double size, int nsubs, double ox, double oy, double oz)
{
	// Clear existing data first (if it exists)
	forgetAll();
	
	// Create each face individually
	GLfloat delta = (GLfloat) size/nsubs, veca[3], vecb[3], vertex[3];
	int i, j, plane;
	// Set general origin coordinate
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
				vertex[0] = ox + i*veca[0] + j*vecb[0];
				vertex[1] = oy + i*veca[1] + j*vecb[1];
				vertex[2] = oz + i*veca[2] + j*vecb[2];
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
	plotCylinder(0.0f, 0.0f, 0.0f, 0.65f, 0.0f, 0.0f, 0.1f, 0.1f, nstacks, nslices);
	plotCylinder(0.65f, 0.0f, 0.0f, 0.35f, 0.0f, 0.0f, 0.2f, 0.0f, nstacks, nslices);

	// Y axis
	plotCylinder(0.0f, 0.0f, 0.0f, 0.0f, 0.65f, 0.0f, 0.1f, 0.1f, nstacks, nslices);
	plotCylinder(0.0f, 0.65f, 0.0f, 0.0f, 0.35f, 0.0f, 0.2f, 0.0f, nstacks, nslices);
	
	// Z axis
	plotCylinder(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.65f, 0.1f, 0.1f, nstacks, nslices);
	plotCylinder(0.0f, 0.0f, 0.65f, 0.0f, 0.0f, 0.35f, 0.2f, 0.0f, nstacks, nslices);
	
}

// Create rotation globe axes
void Primitive::createRotationGlobeAxes(int nstacks, int nslices)
{
	// Create space for one sphere and three cylinders
	forgetAll();
	
	// Axis pointers
	plotCylinder(0.7f, 0.0f, 0.0f, 0.3f, 0.0f, 0.0f, 0.2f, 0.0f, nstacks, nslices);
	plotCylinder(0.0f, 0.7f, 0.0f, 0.0f, 0.3f, 0.0f, 0.2f, 0.0f, nstacks, nslices);
	plotCylinder(0.0f, 0.0f, 0.7f, 0.0f, 0.0f, 0.3f, 0.2f, 0.0f, nstacks, nslices);
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

// Create VBO from current vertex chunk list
void Primitive::createVBO()
{
	msg.enter("Primitive::createVBO");
	// Prepare local array of data to pass to VBO
	int offset, n;
	if (nDefinedVertices_ == -1)
	{
		printf("Error: No data in Primitive with which to create VBO.\n");
		hasVBO_ = FALSE;
		msg.exit("Primitive::createVBO");
		return;
	}
	
	// Determine total size of array (in bytes) for VBO
	int vboSize = nDefinedVertices_ * (colouredVertexData_ ? 10 : 6) * sizeof(GLfloat);
	
	// Remove old VBO first (if one exists)
	if (idVBO_ != 0) glDeleteBuffers(1, &idVBO_);
	
	// Generate VBO
	glGenBuffers(1, &idVBO_);

	// Bind VBO
	glBindBuffer(GL_ARRAY_BUFFER, idVBO_);
	
	// Initialise VBO data, but don't copy anything here
	glBufferData(GL_ARRAY_BUFFER, vboSize, NULL, GL_STATIC_DRAW);

// 	GLfloat *bufdat = (GLfloat*) glMapBuffer(GL_ARRAY_BUFFER, GL_READ_ONLY);
// // 	for (int n=0; n<30; ++n) printf("Buffer data %i is %f\n", n, bufdat[n]);
// 	glUnmapBuffer(GL_ARRAY_BUFFER);
	
	// Loop over stored VertexChunks and copy data to VBO
	offset = 0;
	int chunksize;
	for (VertexChunk *chunk = vertexChunks_.first(); chunk != NULL; chunk = chunk->next)
	{
		chunksize = chunk->nDefinedVertices()*(colouredVertexData_ ? 10 : 6)*sizeof(GLfloat);
		glBufferSubData(GL_ARRAY_BUFFER_ARB, offset, chunksize, chunk->vertexData());
		offset += chunksize;
	}
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	// Set flag
	hasVBO_ = TRUE;
	msg.exit("Primitive::createVBO");
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL()
{
	glEnableClientState(GL_VERTEX_ARRAY);
	// Do we have a VBO (and we're allowed to use it?)
	if (hasVBO_ && prefs.useVBOs())
	{
		// Bind VBO
		glDisableClientState(GL_INDEX_ARRAY);
		glBindBuffer(GL_ARRAY_BUFFER, idVBO_);

		glInterleavedArrays(colouredVertexData_ ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, NULL);
		glDrawArrays(type_, 0, nDefinedVertices_);

		// Revert to normal operation - pass 0 as VBO index
		glBindBuffer(GL_ARRAY_BUFFER, 0);
		glDisableClientState(GL_VERTEX_ARRAY);
		glDisableClientState(GL_NORMAL_ARRAY);
		glDisableClientState(GL_COLOR_ARRAY);
	}
	else for (VertexChunk *chunk = vertexChunks_.first(); chunk != NULL; chunk = chunk->next) chunk->sendToGL();
	glDisableClientState(GL_VERTEX_ARRAY);
}

/*
// Primitive Info
*/

// Constructor
PrimitiveInfo::PrimitiveInfo()
{
	// Private variables
	primitive_ = NULL;
	primitiveGroup_ = NULL;
	fillMode_ = GL_FILL;
	lineWidth_ = 1.0f;
	colour_[0] = 0.0;
	colour_[1] = 0.0;
	colour_[2] = 0.0;
	colour_[3] = 1.0;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set primitive info data
void PrimitiveInfo::set(Primitive *prim, GLfloat *colour, Matrix &transform, GLenum fillMode, GLfloat lineWidth)
{
	primitive_ = prim;
	localTransform_ = transform;
	fillMode_ = fillMode;
	lineWidth_ = lineWidth;
	if (colour != NULL) for (int n=0; n<4; ++n) colour_[n] = colour[n];
}

// Set primitive info data
void PrimitiveInfo::set(PrimitiveGroup *pg, GLfloat *colour, Matrix &transform, GLenum fillMode, GLfloat lineWidth)
{
	primitiveGroup_ = pg;
	localTransform_ = transform;
	fillMode_ = fillMode;
	lineWidth_ = lineWidth;
	if (colour != NULL) for (int n=0; n<4; ++n) colour_[n] = colour[n];
}

// Return pointer to primitive
Primitive *PrimitiveInfo::primitive()
{
	return primitive_;
}

// Return pointer to primitive, selected from group (based on level of detail)
Primitive *PrimitiveInfo::primitive(Matrix &modeltransform)
{
	// Determine LOD for primitive based on supplied transform and stored matrix
	Matrix A = modeltransform * localTransform_;
	// If z is greater than 0 (i.e. it's behind the viewer), we are rendering to an offscreen bitmap, or 
	if ((A[14] > 0) || (-A[14] < prefs.levelOfDetailStartZ())) return &primitiveGroup_->primitive(0);
	return &primitiveGroup_->primitive(int((-A[14]-prefs.levelOfDetailStartZ()) / prefs.levelOfDetailWidth()));
}

// Return pointer to best primitive in the group
Primitive *PrimitiveInfo::bestPrimitive()
{
	return &primitiveGroup_->primitive(0);
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

// Return line width
GLfloat PrimitiveInfo::lineWidth()
{
	return lineWidth_;
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

// Create VBOs for all stored primitives in the group
void PrimitiveGroup::createVBOs()
{
	for (int n=0; n<nPrimitives_; ++n) primitives_[n].createVBO();
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
GridPrimitive::GridPrimitive(Grid *source)
{
	// Public variables
	prev = NULL;
	next = NULL;

	// Private variables
	source_ = source;
	primaryIsTransparent_ = FALSE;
	secondaryIsTransparent_ = FALSE;
}

// Return primary primitive
Primitive &GridPrimitive::primaryPrimitive()
{
	return primaryPrimitive_;
}

// Return secondary primitive
Primitive &GridPrimitive::secondaryPrimitive()
{
	return secondaryPrimitive_;
}

// Set source grid pointer
void GridPrimitive::setSource(Grid *g)
{
	source_ = g;
}

// Return source grid pointer
Grid *GridPrimitive::source()
{
	return source_;
}

// Return whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
bool GridPrimitive::primaryIsTransparent()
{
	return primaryIsTransparent_;
}

// Return whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
bool GridPrimitive::secondaryIsTransparent()
{
	return secondaryIsTransparent_;
}
