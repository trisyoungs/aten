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
#include <Qt/QtGui>
#include "templates/list.h"
#include "base/matrix.h"

#define VERTEXCHUNKSIZE 102

// Forward Declarations
class TCanvas;
class Grid;

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
	GLfloat *vertexData_;
	// Centroid array
	GLfloat *centroids_;
	// Number of data points per vertex (NR=6, CNR=10)
	int dataPerVertex_;
	// Number of defined vertices in current chunk
	int nDefinedVertices_;
	// NUmber of primitive types (nDefinedVertices/verticesPerType) currently defined
	int nDefinedTypes_;
	// Maximum number of allowable vertices
	int maxVertices_;
	// Primitive type (GL)
	GLenum type_;
	// Number of vertices per primitive type
	int verticesPerType_;
	
	private:
	// Update (and finalise) centroid for current primitive type
	void updateCentroid(GLfloat x, GLfloat y, GLfloat z, bool finalise);
	
	public:
	// Initialise structure
	void initialise(GLenum type, bool colourData);
	// Forget all vertex data currently stored in array (but retain array)
	void forgetAll();
	// Define next vertex and normal
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid = TRUE);
	// Define next vertex, normal, and colour (as array)
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid = TRUE);
	// Define next vertex, normal, and colour
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid = TRUE);
	// Return whether current array is full
	bool full();
	// Return number of defined primitive (GL) types
	int nDefinedTypes();
	// Return vertex array
	GLfloat *vertexData();
	// Return centroid array
	GLfloat *centroids();
	// Send to OpenGL (i.e. render)
	void sendToGL();
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
	// GL object drawing method
	GLenum type_;

	public:
	// Clear existing data (including deleting arrays)
	void clear();
	// Forget all data, leaving arrays intact
	void forgetAll();
	// Return vertex array
	VertexChunk *vertexChunks();
	// Flag whether primitive should contain colour data information for each vertex
	void setColourData(bool b);
	// Return whether vertex data contains colour information
	bool colouredVertexData();
	// Send to OpenGL (i.e. render)
	void sendToGL();
	
	
	/*
	// Vertex Generation
	*/
	public:
	// Define next vertex and normal
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid = TRUE);
	// Define next vertex, normal, and colour (as array)
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid = TRUE);
	// Define next vertex, normal, and colour
	void defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid = TRUE);
	// Define triangle fromn supplied array data, unique colour per vertex
	void defineTriangle(GLfloat *vertices, GLfloat *normals, GLfloat *colour);
	// Define triangle with single colour per vertex
	void defineTriangleSingleColour(GLfloat *vertices, GLfloat *normals, GLfloat *colour);
	// Plot vertices of sphere with specified radius and quality
	void plotSphere(double radius, int nstacks, int nslices);
	// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with radii and quality specified
	void plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double startradius, double endradius, int nstacks, int nslices);
	// Plot tube ring of specified radius and tube width
	void plotRing(double radius, double width, int nstacks, int nslices, int nsegments, bool segmented = FALSE);


	/*
	// Primitive Generation
	*/
	public:
	// Create vertices of cross with specified width
	void createCross(double width, int naxes);
	// Create wireframe cube centred at zero
	void createWireCube(double size);
	// Create wireframe, crossed, cube centred at zero
	void createCrossedCube(double size);
	// Create solid cube of specified size, centred at zero, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
	void createCube(double size, int nsubs);
	// Create cell axes
	void createCellAxes();
	// Create rotation globe axes
	void createRotationGlobeAxes(int nstacks, int nslices);
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
	Matrix localTransform_;
	// Colour of primitive (if vertexData_ doesn't contain colour information)
	GLfloat colour_[4];
	// Whether to draw the primitive as filled or wireframe polygons
	GLenum fillMode_;
	// GL object line width (if type_ == GL_LINE or chunk primitive type == GL_LINES)
	GLfloat lineWidth_;
	
	public:
	// Set primitive info data
	void set(Primitive *prim, GLfloat *colour, Matrix &transform, GLenum fillMode = GL_FILL, GLfloat lineWidth = 1.0f);
	// Return pointer to primitive
	Primitive *primitive();
	// Return local transformation of primitive
	Matrix &localTransform();
	// Return colour array
	GLfloat *colour();
	// Return polygon fill mode
	GLenum fillMode();
	// Return line width
	GLfloat lineWidth();
};

// Primitive Group
class PrimitiveGroup
{
	public:
	// Constructor / Destructor
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

// Grid Primitive
class GridPrimitive
{
	public:
	// Constructor
	GridPrimitive();
	// List pointers
	GridPrimitive *prev, *next;

	private:
	// Primitive containing generated porimary surface
	Primitive primaryPrimitive_;
	// Primitive containing generated secondary surface
	Primitive secondaryPrimitive_;
	// Grid from which primitive was created
	Grid *source_;
	// Whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool primaryIsTransparent_;
	// Whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool secondaryIsTransparent_;
	
	public:
	// Return primary primitive
	Primitive &primaryPrimitive();
	// Return secondary primitive
	Primitive &secondaryPrimitive();
	// Set source grid pointer
	void setSource(Grid *g);
	// Return source grid pointer
	Grid *source();
	// Return whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool primaryIsTransparent();
	// Return whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool secondaryIsTransparent();
	// Create 2D (heightmap-style) surface
	void createSurface2D();
	// Create 3D isosurface using Marching Cubes algorithm
	void createSurfaceMarchingCubes();
};

#endif
