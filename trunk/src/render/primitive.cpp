/*
	*** Primitive
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
#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#endif
#include "render/primitive.h"
#include "classes/prefs.h"

// Declare static VBO functions (Windows only)
#ifdef _WIN32
PFNGLGENBUFFERSPROC Primitive::glGenBuffers = NULL;
PFNGLBINDBUFFERPROC Primitive::glBindBuffer = NULL;
PFNGLBUFFERDATAPROC Primitive::glBufferData = NULL;
PFNGLBUFFERSUBDATAPROC Primitive::glBufferSubData = NULL;
PFNGLDELETEBUFFERSPROC Primitive::glDeleteBuffers = NULL;
#endif

/*
// Primitive Instance
*/

// Constructor
PrimitiveInstance::PrimitiveInstance()
{
	// Public variables
	prev = NULL;
	next = NULL;
	
	// Private variables
	context_ = NULL;
	type_ = PrimitiveInstance::ListInstance;
	id_ = 0;
}

// Set data
void PrimitiveInstance::set(const QGLContext *context, PrimitiveInstance::InstanceType type, GLuint id)
{
	context_ = context;
	type_ = type;
	id_ = id;
}

// Return context to which primitive instance is associated
const QGLContext *PrimitiveInstance::context()
{
	return context_;
}

// Return type of instance
PrimitiveInstance::InstanceType PrimitiveInstance::type()
{
	return type_;
}

// Return OpenGL ID of instance
int PrimitiveInstance::id()
{
	return id_;
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
	nDefinedVertices_ = 0;
	useInstances_ = prefs.instanceType() != PrimitiveInstance::NoInstance;
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
		defineVertex(vert[0], vert[1], vert[2], norm[0], norm[1], norm[2], TRUE);
		vert[i] = -vert[i];
		defineVertex(vert[0], vert[1], vert[2], norm[0], norm[1], norm[2], TRUE);
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
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0, TRUE);
			defineVertex(j == 0 ? -r[0] : r[0], j == 1 ? -r[1] : r[1], j == 2 ? -r[2] : r[2], 1.0, 0.0, 0.0, TRUE);
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
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0, TRUE);
			r[(i+1)%3] = -r[(i+1)%3];
			r[(i+2)%3] = -r[(i+2)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0, TRUE);
			r[(i+1)%3] = -r[(i+1)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0, TRUE);
			r[(i+1)%3] = -r[(i+1)%3];
			r[(i+2)%3] = -r[(i+2)%3];
			defineVertex(r[0], r[1], r[2], 1.0, 0.0, 0.0, TRUE);
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
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0], vertex[1]+veca[1], vertex[2]+veca[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+vecb[0], vertex[1]+vecb[1], vertex[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);

				// Define trangle vertices for 'upper' plane
				vertex[plane] += size;
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0], vertex[1]+veca[1], vertex[2]+veca[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+vecb[0], vertex[1]+vecb[1], vertex[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, plane == 1, plane == 2, TRUE);
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

// Flag that this primitive should not use instances (rendering will use vertex arrays)
void Primitive::setNoInstances()
{
	useInstances_ = FALSE;
}

// Push instance of primitive
void Primitive::pushInstance(const QGLContext *context)
{
	// Does this primitive use instances?
	if (!useInstances_) return;
	// Vertex buffer object or plain old display list?
	if (prefs.instanceType() == PrimitiveInstance::VBOInstance)
	{
		// Prepare local array of data to pass to VBO
		int offset;
		GLuint idVBO;
		if (nDefinedVertices_ == -1)
		{
			printf("Error: No data in Primitive with which to create VBO.\n");
			msg.exit("Primitive::pushInstance");
			return;
		}
		
		// Determine total size of array (in bytes) for VBO
		int vboSize = nDefinedVertices_ * (colouredVertexData_ ? 10 : 6) * sizeof(GLfloat);
		
		// Generate VBO
		glGenBuffers(1, &idVBO);

		// Bind VBO
		glBindBuffer(GL_ARRAY_BUFFER, idVBO);
		
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
		
		// Store data
		PrimitiveInstance *pi = instances_.add();
		pi->set(context, PrimitiveInstance::VBOInstance, idVBO);
	}
	else
	{
		// Generate display list
		int listId = glGenLists(1);
		glNewList(listId, GL_COMPILE);
		for (VertexChunk *chunk = vertexChunks_.first(); chunk != NULL; chunk = chunk->next) chunk->sendToGL();
		glEndList();

		// Store data
		PrimitiveInstance *pi = instances_.add();
		pi->set(context, PrimitiveInstance::ListInstance, listId);
	}
}

// Pop topmost instance on primitive's stack
void Primitive::popInstance()
{
	// Does this primitive use instances?
	if (!useInstances_) return;
	instances_.removeLast();
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL()
{
	if (useInstances_)
	{
// 		printf("THere are currently %i instances in this primitive's stack\n", instances_.nItems());
		// Grab topmost instance
		PrimitiveInstance *pi = instances_.last();
		if (pi == NULL) printf("Internal Error: No instance on stack in primitive.\n");
		else if (pi->type() == PrimitiveInstance::VBOInstance)
		{
			glEnableClientState(GL_VERTEX_ARRAY);
			glDisableClientState(GL_INDEX_ARRAY);
			// Bind VBO
			glBindBuffer(GL_ARRAY_BUFFER, pi->id());

			glInterleavedArrays(colouredVertexData_ ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, NULL);
			glDrawArrays(type_, 0, nDefinedVertices_);

			// Revert to normal operation - pass 0 as VBO index
			glBindBuffer(GL_ARRAY_BUFFER, 0);
			glDisableClientState(GL_VERTEX_ARRAY);
			glDisableClientState(GL_NORMAL_ARRAY);
			glDisableClientState(GL_COLOR_ARRAY);
		}
		else glCallList(pi->id());
	}
	else for (VertexChunk *chunk = vertexChunks_.first(); chunk != NULL; chunk = chunk->next) chunk->sendToGL();
}

