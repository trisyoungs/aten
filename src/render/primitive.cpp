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
#include <stdio.h>
#include <math.h>

// Constructor
Primitive::Primitive()
{
	vertexData_ = NULL;
	colouredVertexData_ = FALSE;
	centroids_ = NULL;
	maxVertices_ = 0;
	nDefinedVertices_ = 0;
	type_ = GL_TRIANGLES;
	verticesPerType_ = 3;
	nDefinedTypes_ = 0;
}

// Destructor
Primitive::~Primitive()
{
	clear();
}

// Clear existing data
void Primitive::clear()
{
	if (vertexData_ != NULL) delete[] vertexData_;
	vertexData_ = NULL;
	if (centroids_ != NULL) delete[] centroids_;
	centroids_ = NULL;
}

// Forget all data, leaving arrays intact
void Primitive::forgetAll()
{
	nDefinedVertices_ = 0;
}

/*
// Vertex Generation
*/

// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, for 'length', with radii and quality specified
void Primitive::plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double length, double startradius, double endradius, int nstacks, int nslices)
{
	int i, j, count;
	Vec3<GLfloat> vec, u, v, vert[4];

	double d, dtheta = TWOPI / nslices, dradius = (startradius-endradius)/nstacks;
	deltarj = rj / nstacks;

	// Calculate orthogonal vectors
	vec.set(vx,vy,vz);
	u = vec.orthogonal();
	u.normalise();
	v = vec * u;
	v.normalise();

	for (n=0; n<nstacks; ++n)
	{
//                 if (segmented && (n+1)%2) continue;
		for (m=0; m<=nslices; ++m)
		{
			d = m * dtheta;
			// glBegin(GL_QUAD_STRIP);
			normal = u*cos(d) + v*sin(d);
			glNormal3d(normal.x, normal.y, normal.z);
			coord = normal*(startradius-n*dradius) + deltarj*n;
			glVertex3d(coord.x, coord.y, coord.z);
			coord = normal*(startradius-(n+1)*dradius) + deltarj*(n+1);
			glVertex3d(coord.x, coord.y, coord.z);


		}
	}
}


// 			// First triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
// 			defineVertex(x0 * zr0, y0 * zr0, z0, x0 * zr0, y0 * zr0, 0.0);
// 			defineVertex(x0 * zr1, y0 * zr1, z1, x0 * zr1, y0 * zr1, 0.0);
// 			defineVertex(x1 * zr0, y1 * zr0, z0, x1 * zr0, y1 * zr0, 0.0);
// 
// 			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
// 			defineVertex(x0 * zr1, y0 * zr1, z1, x0 * zr1, y0 * zr1, 0.0);
// 			defineVertex(x1 * zr0, y1 * zr0, z0, x1 * zr0, y1 * zr0, 0.0);
// 			defineVertex(x1 * zr1, y1 * zr1, z1, x1 * zr1, y1 * zr1, 0.0);
// 		}
// 	}
// }

// Define next vertex and normal
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid)
{
	if (colouredVertexData_) printf("Internal Error: No colour specified in vertex creation, but the primitive requires one.\n");
	if (nDefinedVertices_ == maxVertices_) printf("Internal Error: Vertex limit for primitive reached.\n");
	int index = nDefinedVertices_*6;
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
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid)
{
	if (!colouredVertexData_) printf("Internal Error: Colour specified in vertex creation, but it is not required for primitive.\n");
	if (nDefinedVertices_ == maxVertices_) printf("Internal Error: Vertex limit for primitive reached.\n");
	int index = nDefinedVertices_*10;
	// Store colour
	vertexData_[index++] = r;
	vertexData_[index++] = g;
	vertexData_[index++] = b;
	vertexData_[index++] = a;
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

/*
// Primitive Generation
*/

// Create empty data arrays, setting type specified
void Primitive::createEmpty(GLenum type, int ntype, bool colours)
{
	// Clear old data, if any
	clear();
	type_ = type;
	colouredVertexData_ = colours;
	nType_ = ntype;
	if (type_ == GL_LINES) verticesPerType_ = 2;
	else verticesPerType_ = 3;
	maxVertices_ = nType_*verticesPerType_;
	nDefinedVertices_ = 0;
	nDefinedTypes_ = 0;
	if (colouredVertexData_) vertexData_ = new GLfloat[maxVertices_*10];
	else vertexData_ = new GLfloat[maxVertices_*6];
	centroids_ = new GLfloat[nType_*3];
	for (int n=0; n<nType_*3; ++n) centroids_[n] = 0.0f;
}

// Create vertices of sphere with specified radius and quality
void Primitive::createSphere(double radius, int nstacks, int nslices)
{
	msg.enter("Primitive::createSphere");
	int i, j, count;
	double stack0, stack1, z0, zr0, z1, zr1, slice0, slice1, x0, y0, x1, y1;

	// Clear existing data first (if it exists)
	createEmpty(GL_TRIANGLES, nstacks*nslices*2, FALSE);

	count = 0;
	for (i = 0; i < nstacks; i++)
	{
		stack0 = PI * (-0.5 + (double) i / nstacks);
		z0  = sin(stack0);
		zr0 = cos(stack0);

		stack1 = PI * (-0.5 + (double) (i+1) / nstacks);
		z1 = sin(stack1);
		zr1 = cos(stack1);

		for (j = 0; j < nslices; j++)
		{
			slice0 = 2 * PI * (double) j / nslices;
			x0 = cos(slice0);
			y0 = sin(slice0);

			slice1 = 2 * PI * (double) (j+1) / nslices;
			x1 = cos(slice1);
			y1 = sin(slice1);

			// First triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			defineVertex(x0 * zr0 * radius, y0 * zr0 * radius, z0 * radius, x0 * zr0, y0 * zr0, z0);
			defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1);
			defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0);

			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1);
			defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0);
			defineVertex(x1 * zr1 * radius, y1 * zr1 * radius, z1 * radius, x1 * zr1, y1 * zr1, z1);
		}
	}
	msg.exit("Primitive::createSphere");
}

// Create vertices of cylinder with specified radii, length, and quality
void Primitive::createCylinder(double startradius, double endradius, double length, int nstacks, int nslices)
{
	msg.enter("Primitive::createCylinder");
	int i, j, count;
	double stack0, stack1, z0, zr0, z1, zr1, slice0, slice1, x0, y0, x1, y1, deltaz, deltar;

	// Clear existing data first (if it exists)
	createEmpty(GL_TRIANGLES, nstacks*nslices*2, FALSE);

	count = 0;
	deltaz = length / nstacks;
	deltar = (endradius-startradius) / nstacks;
	for (i = 0; i < nstacks; i++)
	{
		stack0 = PI * (-0.5 + (double) i / nstacks);
		z0  = i*deltaz;
		zr0 = startradius + i*deltar;

		stack1 = PI * (-0.5 + (double) (i+1) / nstacks);
		z1 = (i+1)*deltaz;
		zr1 = startradius + (i+1)*deltar;

		for (j = 0; j < nslices; j++)
		{
			slice0 = 2 * PI * (double) j / nslices;
			x0 = cos(slice0);
			y0 = sin(slice0);

			slice1 = 2 * PI * (double) (j+1) / nslices;
			x1 = cos(slice1);
			y1 = sin(slice1);

			// First triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			defineVertex(x0 * zr0, y0 * zr0, z0, x0 * zr0, y0 * zr0, 0.0);
			defineVertex(x0 * zr1, y0 * zr1, z1, x0 * zr1, y0 * zr1, 0.0);
			defineVertex(x1 * zr0, y1 * zr0, z0, x1 * zr0, y1 * zr0, 0.0);

			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			defineVertex(x0 * zr1, y0 * zr1, z1, x0 * zr1, y0 * zr1, 0.0);
			defineVertex(x1 * zr0, y1 * zr0, z0, x1 * zr0, y1 * zr0, 0.0);
			defineVertex(x1 * zr1, y1 * zr1, z1, x1 * zr1, y1 * zr1, 0.0);
		}
	}
	msg.exit("Primitive::createCylinder");
}

// Create vertices of cross with specified width
void Primitive::createCross(double width, int naxes)
{
	int i,j,count, limit = max(1,naxes);
	GLfloat vert[3], norm[3];

	// Clear existing data first (if it exists)
	createEmpty(GL_LINES, limit, FALSE);

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
	createEmpty(GL_LINES, 12, FALSE);
	size = 0.5*size;
	int i, j;
	GLfloat r[3], v[3];
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
	createEmpty(GL_TRIANGLES, nsubs*nsubs*2*6, FALSE);
	
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
				// Define trangle vertices
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
	// Clear existing data first (if it exists)
	createEmpty(GL_TRIANGLES, nsubs*nsubs*2*6, FALSE);

}

// Return vertex array
GLfloat *Primitive::vertexData()
{
	return vertexData_;
}

// Return whether vertex data contains colour information
bool Primitive::colouredVertexData()
{
	return colouredVertexData_;
}

// Return centroids array
GLfloat *Primitive::centroids()
{
	return centroids_;
}

// Return number of vertices defined
int Primitive::nDefinedVertices()
{
	return nDefinedVertices_;
}

// Return number of primitive types defined
int Primitive::nDefinedTypes()
{
	return nDefinedTypes_;
}

// Return whether all arrays are full
bool Primitive::full()
{
	return (nDefinedVertices_ == maxVertices_);
}

// Send to OpenGL (i.e. render)
void Primitive::sendToGL()
{
	if (nDefinedVertices_ == 0) return;
	// Does the vertex data contain colour-per-vertex information?
	if (colouredVertexData_)
	{
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		glEnable(GL_COLOR_MATERIAL);
		glInterleavedArrays(GL_C4F_N3F_V3F, 0, vertexData_);
		glDrawArrays(type_, 0, nDefinedVertices_);
		glDisable(GL_COLOR_MATERIAL);
	}
	else
	{
		glInterleavedArrays(GL_N3F_V3F, 0, vertexData_);
		glDrawArrays(type_, 0, nDefinedVertices_);
	}
}

/*
// Primitive Info
*/

// Constructor
PrimitiveInfo::PrimitiveInfo()
{
	// Private variables
	primitive_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set primitive info data
void PrimitiveInfo::set(Primitive* prim, GLfloat* colour, GLMatrix& transform)
{
	primitive_ = prim;
	localTransform_ = transform;
	for (int n=0; n<4; ++n) colour_[n] = colour[n];
}

// Return pointer to primitive
Primitive *PrimitiveInfo::primitive()
{
	return primitive_;
}

// Return local transformation of primitive
GLMatrix &PrimitiveInfo::localTransform()
{
	return localTransform_;
}

// Return colour array
GLfloat *PrimitiveInfo::colour()
{
	return colour_;
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
