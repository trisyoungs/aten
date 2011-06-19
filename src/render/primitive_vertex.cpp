/*
	*** Primitive - Vertex Generation
	*** src/render/primitive_vertex.cpp
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

#include "render/primitive.h"
#include "base/constants.h"
#include "templates/vector3.h"
#include <math.h>

// Define next vertex and normal
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, bool calcCentroid)
{
	if (currentVertexChunk_ == NULL)
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	else if (currentVertexChunk_->full())
	{
		if (currentVertexChunk_->next == NULL)
		{
			currentVertexChunk_ = vertexChunks_.add();
			currentVertexChunk_->initialise(type_, colouredVertexData_);
		}
		else currentVertexChunk_ = currentVertexChunk_->next;
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,calcCentroid);
	++nDefinedVertices_;
}

// Define next vertex and normal with colour (as array)
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat *colour, bool calcCentroid)
{
	if (currentVertexChunk_ == NULL)
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	else if (currentVertexChunk_->full())
	{
		if (currentVertexChunk_->next == NULL)
		{
			currentVertexChunk_ = vertexChunks_.add();
			currentVertexChunk_->initialise(type_, colouredVertexData_);
		}
		else currentVertexChunk_ = currentVertexChunk_->next;
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,colour,calcCentroid);
	++nDefinedVertices_;
}

// Define next vertex and normal with colour
void Primitive::defineVertex(GLfloat x, GLfloat y, GLfloat z, GLfloat nx, GLfloat ny, GLfloat nz, GLfloat r, GLfloat g, GLfloat b, GLfloat a, bool calcCentroid)
{
	if (currentVertexChunk_ == NULL)
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	else if (currentVertexChunk_->full())
	{
		if (currentVertexChunk_->next == NULL)
		{
			currentVertexChunk_ = vertexChunks_.add();
			currentVertexChunk_->initialise(type_, colouredVertexData_);
		}
		else currentVertexChunk_ = currentVertexChunk_->next;
	}
	currentVertexChunk_->defineVertex(x,y,z,nx,ny,nz,r,g,b,a,calcCentroid);
	++nDefinedVertices_;
}

// Define next vertex, normal, and colour (as Vec3<double>s and array)
void Primitive::defineVertex(Vec3<double> &v, Vec3<double> &u, GLfloat *colour, bool calcCentroid)
{
	if (currentVertexChunk_ == NULL)
	{
		currentVertexChunk_ = vertexChunks_.add();
		currentVertexChunk_->initialise(type_, colouredVertexData_);
	}
	else if (currentVertexChunk_->full())
	{
		if (currentVertexChunk_->next == NULL)
		{
			currentVertexChunk_ = vertexChunks_.add();
			currentVertexChunk_->initialise(type_, colouredVertexData_);
		}
		else currentVertexChunk_ = currentVertexChunk_->next;
	}
	currentVertexChunk_->defineVertex(v.x,v.y,v.z,u.x,u.y,u.z,colour,calcCentroid);
	++nDefinedVertices_;
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

// Plot simple line between specified coordinates
void Primitive::plotLine(GLfloat x1, GLfloat y1, GLfloat z1, GLfloat x2, GLfloat y2, GLfloat z2)
{
	type_ = GL_LINES;
	defineVertex(x1,y1,z1,0.0f,0.0f,1.0f,FALSE);
	defineVertex(x2,y2,z2,0.0f,0.0f,1.0f,FALSE);
}

// Create vertices of sphere with specified radius and quality
void Primitive::plotSphere(double radius, int nstacks, int nslices)
{
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
			defineVertex(x0 * zr0 * radius, y0 * zr0 * radius, z0 * radius, x0 * zr0, y0 * zr0, z0, TRUE);
			defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1, TRUE);
			defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0, TRUE);
			
			// Second triangle - {x0,y0,z0},{x0,y0,z1},{x1,y1,z0}
			defineVertex(x0 * zr1 * radius, y0 * zr1 * radius, z1 * radius, x0 * zr1, y0 * zr1, z1, TRUE);
			defineVertex(x1 * zr0 * radius, y1 * zr0 * radius, z0 * radius, x1 * zr0, y1 * zr0, z0, TRUE);
			defineVertex(x1 * zr1 * radius, y1 * zr1 * radius, z1 * radius, x1 * zr1, y1 * zr1, z1, TRUE);
		}
	}
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
			defineVertex(ox+vert[0].x, oy+vert[0].y, oz+vert[0].z, normal[0].x, normal[0].y, normal[0].z, TRUE);
			defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z, TRUE);
			defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z, TRUE);
 
			// Triangle 2
			defineVertex(ox+vert[1].x, oy+vert[1].y, oz+vert[1].z, normal[0].x, normal[0].y, normal[0].z, TRUE);
			defineVertex(ox+vert[2].x, oy+vert[2].y, oz+vert[2].z, normal[1].x, normal[1].y, normal[1].z, TRUE);
			defineVertex(ox+vert[3].x, oy+vert[3].y, oz+vert[3].z, normal[1].x, normal[1].y, normal[1].z, TRUE);

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
				defineVertex(vert[0].x, vert[0].y, vert[0].z, normal[0].x, normal[0].y, normal[0].z, TRUE);
				defineVertex(vert[1].x, vert[1].y, vert[1].z, normal[1].x, normal[1].y, normal[1].z, TRUE);
				defineVertex(vert[2].x, vert[2].y, vert[2].z, normal[2].x, normal[2].y, normal[2].z, TRUE);
				
				// Triangle 2
				defineVertex(vert[1].x, vert[1].y, vert[1].z, normal[1].x, normal[1].y, normal[1].z, TRUE);
				defineVertex(vert[2].x, vert[2].y, vert[2].z, normal[2].x, normal[2].y, normal[2].z, TRUE);
				defineVertex(vert[3].x, vert[3].y, vert[3].z, normal[3].x, normal[3].y, normal[3].z, TRUE);
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
	
			defineVertex(r1.x, r1.y, r1.z, 0.0, 0.0, 1.0, TRUE);
			defineVertex(r2.x, r2.y, r2.z, 0.0, 0.0, 1.0, TRUE);
		}
	}
}

// Create vertices of cross with specified width
void Primitive::plotCross(double halfWidth, Matrix &transform, GLfloat colour[4])
{
	Vec3<double> v, centre(transform[12], transform[13], transform[14]);
	for (int i=0; i<3; ++i)
	{
		v = transform.columnAsVec3(i) * halfWidth;
		defineVertex(centre.x+v.x, centre.y+v.y, centre.z+v.z, 1.0, 1.0, 1.0, colour[0], colour[1], colour[2], colour[3], FALSE);
		defineVertex(centre.x-v.x, centre.y-v.y, centre.z-v.z, 1.0, 1.0, 1.0, colour[0], colour[1], colour[2], colour[3], FALSE);
	}
}
