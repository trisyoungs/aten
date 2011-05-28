/*
	*** Primitive
	*** src/render/primitive.h
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

#ifndef ATEN_PRIMITIVE_H
#define ATEN_PRIMITIVE_H

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#include "glext.h"
#endif
#include "templates/list.h"
#include "render/vertexchunk.h"
#include "base/matrix.h"

// Forward Declarations
class QGLContext;

// Primitive Instance
class PrimitiveInstance
{
	public:
	// Constructor
	PrimitiveInstance();
	// List pointers
	PrimitiveInstance *prev, *next;
	// Instance Type
	enum InstanceType { NoInstance, ListInstance, VBOInstance };
	
	private:
	// Context to which primitive instance is associated
	const QGLContext *context_;
	// Type of instance
	InstanceType type_;
	// OpenGL ID of instance
	GLuint id_;
	
	public:
	// Set data
	void set(const QGLContext *context, PrimitiveInstance::InstanceType type, GLuint id);
	// Return context to which primitive instance is associated
	const QGLContext *context();
	// Return type of instance
	InstanceType type();
	// Return OpenGL ID of instance
	int id();
};

// Rendering Primitive
class Primitive
{
	public:
	// Constructor / Destructor
	Primitive();
	~Primitive();
	// List pointer
	Primitive *prev, *next;
	// VBO function pointers (Windows only)
	#ifdef _WIN32
	static PFNGLGENBUFFERSPROC glGenBuffers;
	static PFNGLBINDBUFFERPROC glBindBuffer;
	static PFNGLBUFFERDATAPROC glBufferData;
	static PFNGLBUFFERSUBDATAPROC glBufferSubData;
	static PFNGLDELETEBUFFERSPROC glDeleteBuffers;
	#endif

	/*
	// Data
	*/
	private:
	// List of vertices in primitive
	List<VertexChunk> vertexChunks_;
	// Current vertex chunk
	VertexChunk *currentVertexChunk_;
	// Whether vertexData_ array also contains colour information
	bool colouredVertexData_;
	// Number of vertices that have been defined
	int nDefinedVertices_;
	// GL object drawing method
	GLenum type_;
	// Stack of OpenGL VBO or display list IDs and the contexts in which they were created
	List<PrimitiveInstance> instances_;
	// Flag stating whether or not instances should be used for this primitive
	bool useInstances_;

	public:
	// Clear existing data (including deleting arrays)
	void clear();
	// Forget all data, leaving arrays intact
	void forgetAll();
	// Return number of vertices currently defined in primitive
	int nDefinedVertices();
	// Set GL drawing primitive type
	void setType(GLenum type);
	// Return vertex array
	VertexChunk *vertexChunks();
	// Flag whether primitive should contain colour data information for each vertex
	void setColourData(bool b);
	// Return whether vertex data contains colour information
	bool colouredVertexData();
	// Flag that this primitive should not use instances (rendering will use vertex arrays)
	void setNoInstances();
	// Push instance layer from current vertex chunk list
	void pushInstance(const QGLContext *context);
	// Pop topmost instance layer
	void popInstance(const QGLContext *context);
	// Send to OpenGL (i.e. render)
	void sendToGL();
	
	
	/*
	// Vertex Generation
	*/
	public:
	// Define next vertex and normal
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid);
	// Define next vertex, normal, and colour (as array)
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid);
	// Define next vertex, normal, and colour
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid);
	// Define triangle fromn supplied array data, unique colour per vertex
	void defineTriangle(GLfloat *vertices, GLfloat *normals, GLfloat *colour);
	// Define triangle with single colour per vertex
	void defineTriangleSingleColour(GLfloat *vertices, GLfloat *normals, GLfloat *colour);
	// Plot simple line between specified coordinates
	void plotLine(GLfloat x1, GLfloat y1, GLfloat z1, GLfloat x2, GLfloat y2, GLfloat z2);
	// Plot vertices of sphere with specified radius and quality
	void plotSphere(double radius, int nstacks, int nslices);
	// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with radii and quality specified
	void plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double startradius, double endradius, int nstacks, int nslices);
	// Plot tube ring of specified radius and tube width
	void plotRing(double radius, double width, int nstacks, int nslices, int nsegments, bool segmented = FALSE);
	// Plot circle of specified radius
	void plotCircle(double radius, int nstacks, int nsegments, bool segmented = FALSE);
	// Create vertices of cross with specified width
	void plotCross(double halfWidth, Matrix &transform, GLfloat colour[4]);


	/*
	// Primitive Generation
	*/
	public:
	// Create wireframe cube centred at zero
	void createWireCube(double size);
	// Create wireframe, crossed, cube centred at zero
	void createCrossedCube(double size);
	// Create solid cube of specified size at specified origin, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
	void createCube(double size, int nsubs, double ox, double oy, double oz);
	// Create cell axes
	void createCellAxes();
	// Create rotation globe axes
	void createRotationGlobeAxes(int nstacks, int nslices);
};

#endif
