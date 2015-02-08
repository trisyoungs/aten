/*
	*** Vertex Chunk Storage
	*** src/render/vertexchunk.h
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

#ifndef ATEN_VERTEXCHUNK_H
#define ATEN_VERTEXCHUNK_H

#ifdef _WIN32
#include <windows.h>
#endif
#ifdef _MAC
#include <OpenGL/gl3.h>
#else
#include <GL/gl.h>
#endif
#include "math/constants.h"

// Chunk of triangles
class VertexChunk
{
	public:
	// Constructor / Destructor
	VertexChunk();
	~VertexChunk();
	// List pointers
	VertexChunk *prev, *next;
	
	private:
	// Vertex data array (containing normal and possibly colour data)
	GLfloat* vertexData_;
	// Index data array
	GLuint* indexData_;
	// Centroid array
	GLfloat* centroids_;
	// Number of data points per vertex (NR=6, CNR=10)
	int dataPerVertex_;
	// Number of defined vertices in chunk
	int nDefinedVertices_;
	// Number of defined indices in chunk
	int nDefinedIndices_;
	// Nomber of primitive types (nDefinedVertices/verticesPerType) currently defined (only for non-indexed data)
	int nDefinedTypes_;
	// Maximum number of vertices
	int maxVertices_;
	// Maximum number of indices
	int maxIndices_;
	// Whether indices are being used when rendering
	bool hasIndices_;
	// Maximum number of types (for non-indexed data)
	int maxTypes_;
	// Primitive type (GL)
	GLenum type_;
	// Number of vertices per primitive type
	int verticesPerType_;
	// Whether centroid calculation (per primitive type) is on
	bool calcCentroids_;
	
	private:
	// Update (and finalise) centroid for current primitive type
	void updateCentroid(GLfloat x, GLfloat y, GLfloat z);
	
	public:
	// Initialise structure
	void initialise(int newMaxVertices, int newMaxIndices, GLenum type, bool colourData);
	// Forget all vertex data currently stored in array (but retain array)
	void forgetAll();
	// Define next vertex and normal
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz);
	// Define next vertex, normal, and colour (as array)
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour);
	// Define next vertex, normal, and colour
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a);
	// Define index doublet
	bool defineIndices(GLuint a, GLuint b);
	// Define index triplet
	bool defineIndices(GLuint a, GLuint b, GLuint c);
	// Return whether current array is full
	bool full() const;
	// Return whether indices are being used
	bool hasIndices() const;
	// Return number of vertices defined
	int nDefinedVertices() const;
	// Return number of indices defined
	int nDefinedIndices() const;
	// Return number of defined primitive (GL) types
	int nDefinedTypes() const;
	// Return vertex array
	const GLfloat* vertexData();
	// Return index array
	const GLuint* indexData();
	// Return centroid array
	const GLfloat* centroids();
	// Send to GL
	void sendToGL() const;
};

#endif
