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
#include "render/glmatrix.h"
#include "templates/vector3.h"

// Rendering Primitive
class Primitive
{
	public:
	// Constructor / Destructor
	Primitive();
	~Primitive();

	private:
	// Primitive's vertex data array (containing normal and possibly colour data)
	GLfloat *vertexData_;
	// Whether vertexData_ array also contains colour information
	bool colouredVertexData_;
	// Centroid array
	GLfloat *centroids_;
	// Maximum number of vertices in primitive (i.e. array size)
	int maxVertices_;
	// Number of defined vertices (and normals) so far (when using createEmpty())
	int nDefinedVertices_;
	// GL object drawing method
	GLenum type_;
	// Number of primitive types stored
	int nType_;
	// Number of vertices per primitive type
	int verticesPerType_;
	// Number of defined primitive types
	int nDefinedTypes_;

	/*
	// Vertex Generation
	*/
	private:
	// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with radii and quality specified
	void plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double length, double startradius, double endradius, int nstacks, int nslices);

	public:
	// Define next vertex and normal
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid = TRUE);
	// Define next vertex, normal, and colour
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid);


	/*
	// Primitive Generation
	*/
	public:
	// Clear existing data (including deleting arrays)
	void clear();
	// Forget all data, leaving arrays intact
	void forgetAll();
	// Create empty data arrays
	void createEmpty(GLenum type, int ntype, bool colours);
	// Create vertices of sphere with specified radius and quality
	void createSphere(double radius, int nstacks, int nslices);
	// Create vertices of cylinder along z with specified radius, length, and quality
	void createCylinder(double startradius, double endradius, double length, int nstacks, int nslices);
	// Create vertices of cross with specified width
	void createCross(double width, int naxes);
	// Create wireframe cube centred at zero
	void createWireCube(double size);
	// Create solid cube of specified size, centred at zero, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
	void createCube(double size, int nsubs);
	// Create cell axes
	void createCellAxes();
	// Return vertex array
	GLfloat *vertexData();
	// Return whether vertex data contains colour information
	bool colouredVertexData();
	// Return centroids array
	GLfloat *centroids();
	// Return number of vertices defined
	int nDefinedVertices();
	// Return number of primitive types defined
	int nDefinedTypes();
	// Return whether all arrays are full
	bool full();
	// Send to OpenGL (i.e. render)
	void sendToGL();
};

// Primitive Info
class PrimitiveInfo
{
	public:
	// Constructor
	PrimitiveInfo();
	// List pointer
	PrimitiveInfo *prev, *next;

	private:
	// Target primitive
	Primitive *primitive_;
	// Local transformation of primitive
	GLMatrix localTransform_;
	// Colour of primitive (if vertexData_ doesn't contain colour information)
	GLfloat colour_[4];
	
	public:
	// Set primitive info data
	void set(Primitive *prim, GLfloat *colour, GLMatrix &transform);
	// Return pointer to primitive
	Primitive *primitive();
	// Return local transformation of primitive
	GLMatrix &localTransform();
	// Return colour array
	GLfloat *colour();
};

// Primitive Group
class PrimitiveGroup
{
	public:
	// Constructor
	PrimitiveGroup();
	~PrimitiveGroup();

	private:
	// Array of Primitives, corresponding to different levels of detail
	Primitive *primitives_;
	// Number of primitives in array (copied from Prefs)
	int nPrimitives_;

	public:
	// Clear old primitives array and allocate new one
	void clear();
	// Return primitive corresponding to level of detail specified
	Primitive &primitive(int lod);
	// Send to OpenGL (i.e. render) at specified level of detail
	void sendToGL(int lod);
};

#endif
