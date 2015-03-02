/*
	*** Rendering Primitive
	*** src/render/primitive.cpp
	Copyright T. Youngs 2013-2015

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
#include "render/glextensions.h"
#include "base/messenger.h"
// #include <string.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#endif

ATEN_USING_NAMESPACE

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
			Messenger::exit("Primitive::pushInstance");
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
		if (listId == 0) Messenger::print("Primitive::pushInstance - glGenLists(1) returned 0!\n!");
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
void Primitive::popInstance(const QGLContext *context, GLExtensions* extensions)
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
	return vertexChunk_.defineVertex(vertex.x, vertex.y, vertex.z, normal.x, normal.y, normal.z);
}

// Define next vertex and normal
GLuint Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, Vec4<GLfloat>& colour)
{
	return vertexChunk_.defineVertex(x,y,z,nx,ny,nz, colour.x, colour.y, colour.z, colour.w);
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

// Create vertices of sphere with specified radius and quality
void Primitive::plotSphere(double radius, int nStacks, int nSlices)
{
	int i, j, count;
	double stack0, stack1, z0, zr0, z1, zr1, slice0, slice1, x0, y0, x1, y1;
	
	count = 0;
	for (i = 1; i <= nStacks; ++i)
	{
		stack0 = PI * (-0.5 + (double) (i-1) / nStacks );
		z0  = sin(stack0);
		zr0 = cos(stack0);
		
		stack1 = PI * (-0.5 + (double) i / nStacks );
		z1 = sin(stack1);
		zr1 = cos(stack1);
		
		for (j = 1; j <= nSlices; ++j)
		{
			slice0 = 2 * PI * (double) (j-1) / nSlices;
			x0 = cos(slice0);
			y0 = sin(slice0);
			
			slice1 = 2 * PI * (double) j / nSlices;
			x1 = cos(slice1);
			y1 = sin(slice1);
			
			// First triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			// N.B Don't plot if i == 1, to avoid overlapping with subsequent vertices in this pass
			if (i > 1)
			{
				defineVertex(x0 * zr0 * radius, y0 * zr0 * radius, z0 * radius, x0 * zr0, y0 * zr0, z0);
				defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1);
				defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0);
			}
			
			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			// N.B. Don't plot if i == nstacks, to avoid overlapping with previous vertices in this pass
			if (i < nStacks )
			{
				defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1);
				defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0);
				defineVertex(x1 * zr1 * radius, y1 * zr1 * radius, z1 * radius, x1 * zr1, y1 * zr1, z1);
			}
		}
	}
}

// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, with radii and quality specified
void Primitive::plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double startRadius, double endRadius, int nStacks, int nSlices, bool capStart, bool capEnd)
{
	int i, j;
	Vec3<GLfloat> u, v, w, vert[4], normal[2], deltarj, rj;
	double d, dTheta, dRadius;
	
	// Setup some variables
	rj.set(vx,vy,vz);
	dTheta = TWOPI / nSlices;
	dRadius = ( startRadius - endRadius ) / nStacks;
	deltarj = rj / nStacks;

	// Calculate orthogonal vectors
	u = rj.orthogonal();
// 	u.normalise();
	v = rj * u;
	v.normalise();
	w = rj;
	w.normalise();

	// TODO Normal calculation for cones will be incorrect
	for (i=1; i <= nStacks; ++i)
	{
		for (j = 1; j <= nSlices; ++j)
		{
			d = (j-1) * dTheta;
			normal[0] = u*cos(d) + v*sin(d);
			vert[0] = normal[0]*( startRadius -(i-1)* dRadius ) + deltarj*(i-1);
			vert[1] = normal[0]*( startRadius -i* dRadius ) + deltarj*i;
			d = j * dTheta;
			normal[1] = u*cos(d) + v*sin(d);
			vert[2] = normal[1]*( startRadius -(i-1)* dRadius ) + deltarj*(i-1);
			vert[3] = normal[1]*( startRadius -i* dRadius ) + deltarj*i;
			
			// Triangle 1
			if ((i > 1) || ( startRadius > 1.0e-5))
			{
				defineVertex(ox+vert[0].x, oy+vert[0].y, oz+vert[0].z, normal[0].x, normal[0].y, normal[0].z);
				defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z);
				defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z);
			}
 
			// Triangle 2
			if ((i < nStacks ) || ( endRadius > 1.0e-5))
			{
				defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z);
				defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z);
				defineVertex(ox+vert[3].x, oy+vert[3].y, oz+vert[3].z, normal[1].x, normal[1].y, normal[1].z);
			}
			
			// Start cap
			if ((i == 1) && ( startRadius > 1.0e-5) && capStart)
			{
				defineVertex(ox, oy, oz, -w.x, -w.y, -w.z);
				defineVertex(ox+vert[0].x, oy+vert[0].y, oz+vert[0].z, -w.x, -w.y, -w.z);
				defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, -w.x, -w.y, -w.z);
			}

			// End cap
			if ((i == nStacks ) && ( endRadius > 1.0e-5) && capEnd)
			{
				defineVertex(ox+rj.x, oy+rj.y, oz+rj.z, w.x, w.y, w.z);
				defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, w.x, w.y, w.z);
				defineVertex(ox+vert[3].x, oy+vert[3].y, oz+vert[3].z, w.x, w.y, w.z);
			}
		}
	}
}

// Plot tube ring of specified radius and tube width
void Primitive::plotRing(double radius, double width, int nstacks, int nslices, int nsegments, bool segmented)
{
	int n, m, o;
	Vec3<GLfloat> x1, x2, y(0.0,0.0,1.0), normal[4], vert[4], r1, r2;
	double d1, d2, dtheta, dphi, dpsi, cosphi1, sinphi1, cosphi2, sinphi2;

	// Setup some variables
	dphi = TWOPI / nstacks;
	dpsi = dphi / nsegments;
	dtheta = TWOPI / nslices;
	
	for (n=0; n<nstacks; ++n)
	{
		// Calculate position around circle and orthogonal vectors (for cylinder plotting)
		if (segmented && (n+1)%2) continue;

		for (o=0; o<nsegments; ++o)
		{
			cosphi1 = cos(n*dphi+o*dpsi);
			sinphi1 = sin(n*dphi+o*dpsi);
			cosphi2 = cos(n*dphi+(o+1)*dpsi);
			sinphi2 = sin(n*dphi+(o+1)*dpsi);
			r1.set(cosphi1*radius, sinphi1*radius, 0.0);
			r2.set(cosphi2*radius, sinphi2*radius, 0.0);
			x1.set(cosphi1, sinphi1, 0.0);
			x2.set(cosphi2, sinphi2, 0.0);
			
			for (m=0; m<nslices; ++m)
			{
				// Plot along specified direction, and then map vertices from straight cylinder onto circle in XY plane
				d1 = m * dtheta;
				d2 = d1 + dtheta;
	
				normal[0] = x1*cos(d1) + y*sin(d1);
				normal[1] = x1*cos(d2) + y*sin(d2);
				normal[2] = x2*cos(d1) + y*sin(d1);
				normal[3] = x2*cos(d2) + y*sin(d2);
	
				vert[0] = normal[0]*width + r1;
				vert[1] = normal[1]*width + r1;
				vert[2] = normal[2]*width + r2;
				vert[3] = normal[3]*width + r2;
	
				// Triangle 1
				defineVertex(vert[0].x, vert[0].y, vert[0].z, normal[0].x, normal[0].y, normal[0].z);
				defineVertex(vert[1].x, vert[1].y, vert[1].z, normal[1].x, normal[1].y, normal[1].z);
				defineVertex(vert[2].x, vert[2].y, vert[2].z, normal[2].x, normal[2].y, normal[2].z);
				
				// Triangle 2
				defineVertex(vert[1].x, vert[1].y, vert[1].z, normal[1].x, normal[1].y, normal[1].z);
				defineVertex(vert[2].x, vert[2].y, vert[2].z, normal[2].x, normal[2].y, normal[2].z);
				defineVertex(vert[3].x, vert[3].y, vert[3].z, normal[3].x, normal[3].y, normal[3].z);
			}
		}
	}
}

// Plot circle of specified radius
void Primitive::plotCircle(double radius, int nstacks, int nsegments, bool segmented)
{
	int n, o;
	Vec3<GLfloat> r1, r2;
	double dphi, dpsi, cosphi1, sinphi1, cosphi2, sinphi2;

	type_ = GL_LINES;

	// Setup some variables
	dphi = TWOPI / nstacks;
	dpsi = dphi / nsegments;
	
	for (n=0; n<nstacks; ++n)
	{
		// Calculate position around circle
		if (segmented && (n+1)%2) continue;

		for (o=0; o<nsegments; ++o)
		{
			cosphi1 = cos(n*dphi+o*dpsi);
			sinphi1 = sin(n*dphi+o*dpsi);
			cosphi2 = cos(n*dphi+(o+1)*dpsi);
			sinphi2 = sin(n*dphi+(o+1)*dpsi);
			r1.set(cosphi1*radius, sinphi1*radius, 0.0);
			r2.set(cosphi2*radius, sinphi2*radius, 0.0);
	
			defineVertex(r1.x, r1.y, r1.z, 0.0, 0.0, 1.0);
			defineVertex(r2.x, r2.y, r2.z, 0.0, 0.0, 1.0);
		}
	}
}

// Create vertices of cross with specified width
void Primitive::plotCross(double halfWidth, Matrix& transform, Vec4<GLfloat>& colour)
{
	Vec3<double> v, centre(transform[12], transform[13], transform[14]);
	for (int i=0; i<3; ++i)
	{
		v = transform.columnAsVec3(i) * halfWidth;
		defineVertex(centre.x+v.x, centre.y+v.y, centre.z+v.z, 1.0, 1.0, 1.0, colour);
		defineVertex(centre.x-v.x, centre.y-v.y, centre.z-v.z, 1.0, 1.0, 1.0, colour);
	}
}

// Plot solid cube of specified size at specified origin, and with sides subdivided into triangles ( ntriangles = 2*nsubs )
void Primitive::plotCube(double size, int nSubs, double ox, double oy, double oz)
{
	// Create each face individually
	GLfloat delta = (GLfloat) size / nSubs, veca[3], vecb[3], vertex[3];
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
		for (i=0; i< nSubs; ++i)
		{
			for (j=0; j< nSubs; ++j)
			{
				vertex[0] = ox + i*veca[0] + j*vecb[0];
				vertex[1] = oy + i*veca[1] + j*vecb[1];
				vertex[2] = oz + i*veca[2] + j*vecb[2];
				// Define trangle vertices for 'lower' plane
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, -1*(plane == 1), -1*(plane == 2));
				defineVertex(vertex[0]+veca[0], vertex[1]+veca[1], vertex[2]+veca[2], plane == 0, -1*(plane == 1), -1*(plane == 2));
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, -1*(plane == 1), -1*(plane == 2));
				defineVertex(vertex[0], vertex[1], vertex[2], plane == 0, -1*(plane == 1), -1*(plane == 2));
				defineVertex(vertex[0]+vecb[0], vertex[1]+vecb[1], vertex[2]+vecb[2], plane == 0, -1*(plane == 1), -1*(plane == 2));
				defineVertex(vertex[0]+veca[0]+vecb[0], vertex[1]+veca[1]+vecb[1], vertex[2]+veca[2]+vecb[2], plane == 0, -1*(plane == 1), -1*(plane == 2));

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

// Create wireframe, crossed cube centred at zero
void Primitive::plotCrossedCube(double size, int nSubs, double ox, double oy, double oz)
{
	// Create wire cube to start with
	plotCube(size, nSubs, ox, oy, oz);

	// Add crosses to faces
	int i, j, sign;
	GLfloat r[3];
	for (i=0; i<3; ++i)
	{
		for (sign = 1; sign > -2; sign -=2)
		{
			// Determine single coordinate on positive face from which to determine all others
			for (j = 0; j<3; ++j) r[j] = (j == i ? 0.55*size*sign : 0.4*size);
			r[0] += ox;
			r[1] += oy;
			r[2] += oz;
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
