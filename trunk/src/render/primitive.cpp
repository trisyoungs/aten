/*
	*** Rendering Primitive
	*** src/render/primitive.cpp
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

#include "base/messenger.h"
#include "render/glextensions.h"
#include "render/primitive.h"
#include <string.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#endif

// Constructor
Primitive::Primitive() : ListItem<Primitive>()
{
	colouredVertexData_ = false;
	type_ = GL_TRIANGLES;
	useInstances_ = true;
}

// Destructor
Primitive::~Primitive()
{
}

/*
 * Data
 */

// Initialise primitive
void Primitive::initialise(int maxVertices, int maxIndices, GLenum type, bool colourData)
{
	type_ = type;
	colouredVertexData_ = colourData;
	vertexChunk_.initialise(maxVertices, maxIndices, type, colouredVertexData_);
}

// Forget all data, leaving arrays intact
void Primitive::forgetAll()
{
	vertexChunk_.forgetAll();
}

// Return number of vertices currently defined in primitive
int Primitive::nDefinedVertices() const
{
	return vertexChunk_.nDefinedVertices();
}

// Return number of indices currently defined in primitive
int Primitive::nDefinedIndices() const
{
	return vertexChunk_.nDefinedIndices();
}

// Return first chunk vertex array
const VertexChunk& Primitive::vertexChunk()
{
	return vertexChunk_;
}

// Return whether vertex data contains colour information
bool Primitive::colouredVertexData() const
{
	return colouredVertexData_;
}

// Flag that this primitive should not use instances (rendering will use vertex arrays)
void Primitive::setNoInstances()
{
	useInstances_ = false;
}

// Push instance of primitive
void Primitive::pushInstance(const QGLContext* context, GLExtensions* extensions)
{
	// Does this primitive use instances?
	if (!useInstances_) return;

	// Clear the error flag
	glGetError();

	// Create new instance
	PrimitiveInstance *pi = instances_.add();
	pi->setExtensions(extensions);

	// Vertex buffer object or plain old display list?
	if (PrimitiveInstance::globalInstanceType() == PrimitiveInstance::VBOInstance)
	{
		// Prepare local array of data to pass to VBO
		GLuint vertexVBO = 0, indexVBO = 0;
		if (vertexChunk_.nDefinedVertices() <= 0)
		{
			// Store instance data
			pi->setVBO(context, 0, 0);
			msg.exit("Primitive::pushInstance");
			return;
		}

		// Determine total size of array (in bytes) for VBO
		int vboSize = vertexChunk_.nDefinedVertices() * (colouredVertexData_ ? 10 : 6) * sizeof(GLfloat);
		
		// Generate vertex array object
		extensions->glGenBuffers(1, &vertexVBO);

		// Bind VBO
		extensions->glBindBuffer(GL_ARRAY_BUFFER, vertexVBO);
		
		// Initialise vertex array data
		extensions->glBufferData(GL_ARRAY_BUFFER, vboSize, vertexChunk_.vertexData(), GL_STATIC_DRAW);
		if (glGetError() != GL_NO_ERROR)
		{
			extensions->glBindBuffer(GL_ARRAY_BUFFER, 0);
			printf("Error occurred while generating vertex buffer object for Primitive.\n");
			extensions->glDeleteBuffers(1, &vertexVBO);
			vertexVBO = 0;
			return;
		}
		extensions->glBindBuffer(GL_ARRAY_BUFFER, 0);

		// Generate index array object (if using indices)
		if (vertexChunk_.hasIndices())
		{
			// Generate index array object
			extensions->glGenBuffers(1, &indexVBO);

			// Bind VBO
			extensions->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexVBO);
			
			// Initialise index array data
			extensions->glBufferData(GL_ELEMENT_ARRAY_BUFFER, vertexChunk_.nDefinedIndices()*sizeof(GLuint), vertexChunk_.indexData(), GL_STATIC_DRAW);
			if (glGetError() != GL_NO_ERROR)
			{
				extensions->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
				printf("Error occurred while generating index buffer object for Primitive.\n");
				extensions->glDeleteBuffers(1, &indexVBO);
				indexVBO = 0;
				return;
			}
			extensions->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
		}

		// Store instance data
		pi->setVBO(context, vertexVBO, vertexChunk_.hasIndices() ? indexVBO : 0);
	}
	else
	{
		// Generate display list
		int listId = glGenLists(1);
		if (listId == 0) msg.print("Primitive::pushInstance - glGenLists(1) returned 0!\n!");
		else
		{
			glNewList(listId, GL_COMPILE);
			vertexChunk_.sendToGL();
			glEndList();
		}

		// Store data
		pi->setDisplayList(context, listId);
	}
}

// Pop topmost instance on primitive's stack
void Primitive::popInstance(const QGLContext *context)
{
	// Does this primitive use instances?
	if (!useInstances_) return;
	PrimitiveInstance *pi = instances_.last();
	if (pi != NULL)
	{
		if (pi->context() == context)
		{
			// Vertex buffer object or plain old display list?
			if (pi->type() == PrimitiveInstance::VBOInstance)
			{
				const GLExtensions* extensions = pi->extensions();
				GLuint bufid  = pi->vboVertexObject();
				if (bufid != 0) extensions->glDeleteBuffers(1, &bufid);
				if (vertexChunk_.hasIndices())
				{
					bufid = pi->vboIndexObject();
					if (bufid != 0) extensions->glDeleteBuffers(1, &bufid);
				}
			}
			else if (pi->listObject() != 0) glDeleteLists(pi->listObject(),1);
		}
		instances_.removeLast();
	}
}

// Return number of instances available
int Primitive::nInstances()
{
	return instances_.nItems();
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL() const
{
	// If no vertices are defined, nothing to do...
	if (vertexChunk_.nDefinedVertices() == 0) return;

	// Check if using instances...
	if (useInstances_)
	{
		// Grab topmost instance
		PrimitiveInstance *pi = instances_.last();
		if (pi == NULL) printf("Internal Error: No instance on stack in primitive %p.\n", this);
		else if (pi->type() == PrimitiveInstance::VBOInstance)
		{
			const GLExtensions* extensions = pi->extensions();
			glEnableClientState(GL_VERTEX_ARRAY);
			glEnableClientState(GL_NORMAL_ARRAY);
			glEnableClientState(GL_COLOR_ARRAY);
			if (vertexChunk_.hasIndices()) glEnableClientState(GL_INDEX_ARRAY);
			else glDisableClientState(GL_INDEX_ARRAY);

			// Bind VBO and index buffer (if using it)
			extensions->glBindBuffer(GL_ARRAY_BUFFER, pi->vboVertexObject());
			if (vertexChunk_.hasIndices()) extensions->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, pi->vboIndexObject());

			glInterleavedArrays(colouredVertexData_ ? GL_C4F_N3F_V3F : GL_N3F_V3F, 0, NULL);
			if (vertexChunk_.hasIndices()) glDrawElements(type_, vertexChunk_.nDefinedIndices(), GL_UNSIGNED_INT, 0);
			else glDrawArrays(type_, 0, vertexChunk_.nDefinedVertices());

			// Revert to normal operation - pass 0 as VBO index
			extensions->glBindBuffer(GL_ARRAY_BUFFER, 0);
			if (vertexChunk_.hasIndices()) extensions->glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
			glDisableClientState(GL_VERTEX_ARRAY);
			glDisableClientState(GL_NORMAL_ARRAY);
			glDisableClientState(GL_COLOR_ARRAY);
			if (vertexChunk_.hasIndices()) glDisableClientState(GL_INDEX_ARRAY);
		}
		else if (pi->listObject() != 0) glCallList(pi->listObject());
	}
	else vertexChunk_.sendToGL();
}

/*
 * Vertex / Index Generation
 */

// Define next vertex and normal
GLuint Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz)
{
	return vertexChunk_.defineVertex(x,y,z,nx,ny,nz);
}

// Define next vertex and normal (as Vec3<double>)
GLuint Primitive::defineVertex(Vec3<double> vertex, Vec3<double> normal)
{
	return defineVertex(vertex.x, vertex.y, vertex.z, normal.x, normal.y, normal.z);
}

// Define next vertex and normal with colour
GLuint Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a)
{
	return vertexChunk_.defineVertex(x,y,z,nx,ny,nz,r,g,b,a);
}

// Define next vertex and normal with colour (as array)
GLuint Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, Vec3<double>& normal, Vec4<GLfloat>& colour)
{
	return vertexChunk_.defineVertex(x,y,z,normal.x,normal.y,normal.z,colour.x,colour.y,colour.z,colour.w);
}

// Define next vertex, normal, and colour (as Vec3<double>s and array)
GLuint Primitive::defineVertex(Vec3<double>& v, Vec3<double>& u, Vec4<GLfloat>& colour)
{
	return vertexChunk_.defineVertex(v.x,v.y,v.z,u.x,u.y,u.z,colour.x,colour.y,colour.z,colour.w);
}

// Define next index double
bool Primitive::defineIndices(GLuint a, GLuint b)
{
	return vertexChunk_.defineIndices(a, b);
}

// Define next index triple
bool Primitive::defineIndices(GLuint a, GLuint b, GLuint c)
{
	return vertexChunk_.defineIndices(a, b, c);
}

/*
 * Geometric Primitive Generation
 */

// Draw line
void Primitive::line(double x1, double y1, double z1, double x2, double y2, double z2)
{
	defineVertex(x1, y1, z1, 1.0, 0.0, 0.0);
	defineVertex(x2, y2, z2, 1.0, 0.0, 0.0);
}

// Add line to axis primitive
void Primitive::line(Vec3<double> v1, Vec3<double> v2)
{
	defineVertex(v1.x, v1.y, v1.z, 1.0, 0.0, 0.0);
	defineVertex(v2.x, v2.y, v2.z, 1.0, 0.0, 0.0);
}
