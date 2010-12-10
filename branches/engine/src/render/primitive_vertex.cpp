/*
	*** Rendering Primitive - Vertex Generation
	*** src/render/primitive_vertex.cpp
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

// Define next vertex and normal
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid)
{
	if ((currentVertexChunk_ == NULL) || (currentVertexChunk_->full()))
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,calcCentroid);
}

// Define next vertex and normal with colour (as array)
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid)
{
	if ((currentVertexChunk_ == NULL) || (currentVertexChunk_->full()))
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,colour,calcCentroid);
}

// Define next vertex and normal with colour
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid)
{
	if ((currentVertexChunk_ == NULL) || (currentVertexChunk_->full()))
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,r,g,b,a,calcCentroid);
}

// Define triangle
void Primitive::defineTriangle(GLfloat *vertices, GLfloat *normals, GLfloat *colour)
{
	// Add vertices to list
	defineVertex(vertices[0], vertices[1], vertices[2], normals[0], normals[1], normals[2], colour[0], colour[1], colour[2], colour[3], FALSE);
	defineVertex(vertices[3], vertices[4], vertices[5], normals[3], normals[4], normals[5], colour[4], colour[5], colour[6], colour[7], FALSE);
	defineVertex(vertices[6], vertices[7], vertices[8], normals[6], normals[7], normals[8], colour[8], colour[9], colour[10], colour[11], FALSE);
}

// Define triangle with same-coloured vertices
void Primitive::defineTriangleSingleColour(GLfloat *vertices, GLfloat *normals, GLfloat *colour)
{
	// Add vertices to list
	defineVertex(vertices[0], vertices[1], vertices[2], normals[0], normals[1], normals[2], colour[0], colour[1], colour[2], colour[3], FALSE);
	defineVertex(vertices[3], vertices[4], vertices[5], normals[3], normals[4], normals[5], colour[0], colour[1], colour[2], colour[3], FALSE);
	defineVertex(vertices[6], vertices[7], vertices[8], normals[6], normals[7], normals[8], colour[0], colour[1], colour[2], colour[3], FALSE);
}

// Create vertices of sphere with specified radius and quality
void Primitive::plotSphere(double radius, int nstacks, int nslices)
{
	msg.enter("Primitive::plotSphere");
	int i, j, count;
	double stack0, stack1, z0, zr0, z1, zr1, slice0, slice1, x0, y0, x1, y1;
	
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
	msg.exit("Primitive::plotSphere");
}

// Plot cylinder vertices from origin {ox,oy,oz}, following vector {vx,vy,vz}, with radii and quality specified
void Primitive::plotCylinder(GLfloat ox, GLfloat oy, GLfloat oz, GLfloat vx, GLfloat vy, GLfloat vz, double startradius, double endradius, int nstacks, int nslices)
{
	int n, m;
	Vec3<GLfloat> u, v, vert[4], normal[2], deltarj, rj;
	double d, dtheta, dradius;
	
	// Setup some variables
	rj.set(vx,vy,vz);
	dtheta = TWOPI / nslices;
	dradius = (startradius-endradius)/nstacks;
	deltarj = rj / nstacks;

	// Calculate orthogonal vectors
	u = rj.orthogonal();
	u.normalise();
	v = rj * u;
	v.normalise();

	// TODO Normal calculation for cones will be incorrect
	
	for (n=0; n<nstacks; ++n)
	{
//                 if (segmented && (n+1)%2) continue;
		for (m=0; m<nslices; ++m)
		{
			d = m * dtheta;
			normal[0] = u*cos(d) + v*sin(d);
			vert[0] = normal[0]*(startradius-n*dradius) + deltarj*n;
			vert[1] = normal[0]*(startradius-(n+1)*dradius) + deltarj*(n+1);
			d = (m+1) * dtheta;
			normal[1] = u*cos(d) + v*sin(d);
			vert[2] = normal[1]*(startradius-n*dradius) + deltarj*n;
			vert[3] = normal[1]*(startradius-(n+1)*dradius) + deltarj*(n+1);
			
			// Triangle 1
			defineVertex(ox+vert[0].x, oy+vert[0].y, oz+vert[0].z, normal[0].x, normal[0].y, normal[0].z);
			defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z);
			defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z);
 
			// Triangle 2
			defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z);
			defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z);
			defineVertex(ox+vert[3].x, oy+vert[3].y, oz+vert[3].z, normal[1].x, normal[1].y, normal[1].z);

		}
	}
}
