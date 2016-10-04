/*
	*** Rendering Primitive
	*** src/render/primitive.h
	Copyright T. Youngs 2013-2016

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
#include "render/primitiveinstance.h"
#include "math/matrix.h"
#include "templates/array.h"
#include "base/namespace.h"

// Forward Declarations (Qt)
class QOpenGLContext;
class QOpenGLFunctions;

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Grid;

// Rendering Primitive
class Primitive : public ListItem<Primitive>
{
	public:
	// Constructor / Destructor
	Primitive();
	~Primitive();


	/*
	 * Data
	 */
	private:
	// Vertex data array
	Array<GLfloat> vertexData_;
	// Number of vertices defined in vertexData_
	int nDefinedVertices_;
	// Index data array
	Array<GLuint> indexData_;
	// GL primitive type (GL_TRIANGLES, GL_LINES etc.)
	GLenum type_;
	// Number of data points per vertex
	int dataPerVertex_;
	// Whether vertex data array also contains colour information
	bool colouredVertexData_;

	public:
	// Initialise primitive storage
	void initialise(GLenum type, bool colourData);
	// Forget all data, leaving arrays intact
	void forgetAll();
	// Return number of vertices currently defined in primitive
	int nDefinedVertices() const;
	// Return number of indices currently defined in primitive
	int nDefinedIndices() const;
	// GL primitive type (GL_TRIANGLES, GL_LINES etc.)
	GLenum type();
	// Return whether vertex data contains colour information
	bool colouredVertexData() const;
	// Update mesh (recreate instance / VBO / display list) of primitive
	void updateMesh();


	/*
	 * Instances
	 */
	private:
	// Stack of OpenGL VBO or display list IDs and the contexts in which they were created
	List<PrimitiveInstance> instances_;
	// Flag stating whether or not instances should be used for this primitive
	bool useInstances_;
	// Whether primitive is registered as a dynamic primitive (in PrimitiveSet)
	bool registeredAsDynamic_;

	public:
	// Flag that this primitive should not use instances (rendering will use vertex arrays)
	void setNoInstances();
	// Push instance layer from current vertex chunk list
	void pushInstance(const QOpenGLContext* context);
	// Pop topmost instance layer
	void popInstance(const QOpenGLContext* context);
	// Return number of instances available
	int nInstances();
	// Return topmost instance in list
	PrimitiveInstance* lastInstance();
	// Set whether primitive is registered as a dynamic primitive (in PrimitiveSet)
	void setRegisteredAsDynamic(bool b);
	// Return whether primitive is registered as a dynamic primitive (in PrimitiveSet)
	bool registeredAsDynamic();


	/*
	 * GL Error Reporting
	 */
	public:
	// Clear GL errors
	static void clearGLErrors();
	// Check for GL errors, returning if any were encountered
	static bool glFlaggedError();


	/*
	 * Vertex / Index Generation
	 */
	public:
	// Define next vertex and normal
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz);
	// Define next vertex and normal (as Vec3<double>)
	GLuint defineVertex(Vec3<double>& vertex, Vec3<double>& normal);
	// Define next vertex, normal, and colour
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a);
	// Define next vertex and normal
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, Vec4<GLfloat>& colour);
	// Define next vertex, normal, and colour
	GLuint defineVertex(GLfloat x, GLfloat y, GLfloat z, Vec3<double>& normal, Vec4<GLfloat>& colour);
	// Define next vertex, normal, and colour (as Vec3<double>s and array)
	GLuint defineVertex(Vec3<double>& v, Vec3<double>& u, Vec4<GLfloat>& colour);
	// Define next index double
	void defineIndices(GLuint a, GLuint b);
	// Define next index triple
	void defineIndices(GLuint a, GLuint b, GLuint c);
	// Add degenerate triangle
	void addDegenerateTriangle();


	/*
	 * Geometric Primitive Generation
	 */
	private:
	inline void plotEdge(const int cubeType, const double lowerCutoff, const double valueA, const double valueB, const Vec3<double> gradientA, const Vec3<double> gradientB, const Vec3<double> cubeLLC, const int edgeIndexA, const int edgeIndexB, const int colourscale = -1);

	public:
	// Draw line
	void line(double x1, double y1, double z1, double x2, double y2, double z2);
	// Draw line
	void line(Vec3<double> v1, Vec3<double> v2);
	// Draw line with colour
	void line(Vec3<double> v1, Vec3<double> v2, Vec4<GLfloat> colour);
	// Plot vertices of sphere with specified radius and quality
	void plotSphere(double radius, int nStacks, int nSlices, bool colourData = false, Vec4<GLfloat> colour = Vec4<GLfloat>());
	// Plot line sphere with specified radius and quality
	void plotLineSphere(double radius, int nStacks, int nSlices);
	// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with radius and quality specified
	void plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double radius, int nSlices, bool capStart = false, bool capEnd = false, bool colourData = false, Vec4<GLfloat> colour = Vec4<GLfloat>());
	// Plot cone vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with start radius and quality specified
	void plotCone(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double startRadius, int nSlices, bool capStart = false, bool colourData = false, Vec4<GLfloat> colour = Vec4<GLfloat>());
	// Plot tube ring of specified radius and tube width
	void plotRing(double radius, double width, int nStacks, int nSlices, int nSegments, bool segmented = false);
	// Plot circle of specified radius
	void plotCircle(double radius, int nStacks, int nSegments, bool segmented = false);
	// Create vertices of cross with specified width
	void plotCross(double halfWidth, Matrix& transform, Vec4<GLfloat>& colour);
	// Plot solid cube of specified size at specified origin, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
	void plotCube(double size, int nSubs, double ox, double oy, double oz);
	// Plot wire cube of specified size at specified origin
	void plotWireCube(double size, double ox, double oy, double oz);
	// Create wireframe, crossed cube centred at zero
	void plotCrossedCube(double size, int nSubs, double ox, double oy, double oz);
	// Create halo
	void plotHalo(double radius1, double radius2, int nSegments);
	// Generate surface via marching cubes algorithm
	void marchingCubes(AtenSpace::Grid* source, double lowerCutoff, double upperCutoff, int colourScale);
	// Generate surface via marching cubes algorithm
	void createSurface(Grid* source, Vec4<GLfloat> colour, int colourScale);


	/*
	 * OpenGL
	 */
	public:
	// Prepare for GL rendering
	bool beginGL(QOpenGLFunctions* glFunctions);
	// Draw primitive (as VBO, with indices)
	void sendVBOWithIndices();
	// Draw primitive (as VBO, with indices)
	void sendVBO();
	// Finalise after GL rendering
	void finishGL(QOpenGLFunctions* glFunctions);
	// Send to GL (i.e. prep, send, and release)
	void sendToGL(const QOpenGLContext* context);
	// Send to GL (i.e. prep, send, and release) in specified style
	void sendToGL(const QOpenGLContext* context, GLenum style, bool lighting, bool hasColour, double* colour);
};

ATEN_END_NAMESPACE

#endif
