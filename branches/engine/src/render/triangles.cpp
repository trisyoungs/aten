/*
	*** Polygon Class
	*** src/render/polygon.cpp
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

#include "render/triangles.h"

/*
// Triangles
*/

// Constructor
Triangles::Triangles()
{
	prev = NULL;
	next = NULL;
	createEmpty(GL_TRIANGLES, TRIANGLECHUNKSIZE*3, TRUE);
};

/*
// Triangle List
*/

// Constructor
TriangleList::TriangleList()
{
	currentTriangles_ = NULL;
}

// Forget all stored triangles (but leave structures and lists intact
void TriangleList::forgetAll()
{
	// Set current list pointer to first in list
	currentTriangles_ = triangles_.first();
	// Set number of triangles in each list to zero
	for (Triangles *t = triangles_.first(); t != NULL; t = t->next) t->forgetAll();
}

// Add triangle
void TriangleList::addTriangle(GLfloat *vertices, GLfloat *normals, GLfloat *colour)
{
	// Check current list for space
	if (currentTriangles_ == NULL) currentTriangles_ = triangles_.add();
	else if (currentTriangles_->full()) currentTriangles_ = triangles_.add();
	// Add triangle to list
	currentTriangles_->defineVertex(vertices[0], vertices[1], vertices[2], normals[0], normals[1], normals[2], colour[0], colour[1], colour[2], colour[3], FALSE);
	currentTriangles_->defineVertex(vertices[3], vertices[4], vertices[5], normals[3], normals[4], normals[5], colour[4], colour[5], colour[6], colour[7], FALSE);
	currentTriangles_->defineVertex(vertices[6], vertices[7], vertices[8], normals[6], normals[7], normals[8], colour[8], colour[9], colour[10], colour[11], FALSE);
}

// Add triangle with same-coloured vertices
void TriangleList::addTriangleSingleColour(GLfloat *vertices, GLfloat *normals, GLfloat *colour)
{
	// Check current list for space
	if (currentTriangles_ == NULL) currentTriangles_ = triangles_.add();
	else if (currentTriangles_->full()) currentTriangles_ = triangles_.add();
	// Add triangle to list
	currentTriangles_->defineVertex(vertices[0], vertices[1], vertices[2], normals[0], normals[1], normals[2], colour[0], colour[1], colour[2], colour[3], FALSE);
	currentTriangles_->defineVertex(vertices[3], vertices[4], vertices[5], normals[3], normals[4], normals[5], colour[0], colour[1], colour[2], colour[3], FALSE);
	currentTriangles_->defineVertex(vertices[6], vertices[7], vertices[8], normals[6], normals[7], normals[8], colour[0], colour[1], colour[2], colour[3], FALSE);
}

// Sent triangles to GL
void TriangleList::sendToGL()
{
	for (Triangles *t = triangles_.first(); t != NULL; t = t->next) t->sendToGL();
}

/*
// Triangle Chopper
*/

// Constructor
TriangleChopper::TriangleChopper()
{
	startZ_ = 0.0;
	endZ_ = 0.0;
	sliceWidth_ = 0.0;
	nSlices_ = 0;
	triangleLists_ = NULL;
}

// Destructor
TriangleChopper::~TriangleChopper()
{
	clear();
}

// Clear all existing triangle lists
void TriangleChopper::clear()
{
	if (triangleLists_ != NULL) delete[] triangleLists_;
	triangleLists_ = NULL;
}

// Empty all existing triangle lists
void TriangleChopper::emptyTriangles()
{
	for (int n=0; n<nSlices_; ++n) triangleLists_[n].forgetAll();
}

// Initialise structure
void TriangleChopper::initialise(double startz, double endz, double slicewidth)
{
	clear();
	// Store new range values and reallocate array
	startZ_ = startz;
	endZ_ = endz;
	sliceWidth_ = slicewidth;
	nSlices_ = (endZ_ - startZ_) / sliceWidth_ + 1;
	if (nSlices_ < 0) printf("Internal Error: Number of calculated slices in chopper is negative.\n");
	triangleLists_ = new TriangleList[nSlices_];
	msg.print(Messenger::Verbose, "Created %i bins for transparency correction.\n", nSlices_);
}

// Store primitive's triangles
void TriangleChopper::storeTriangles(PrimitiveInfo *pinfo)
{
	GLfloat *vertexData = pinfo->primitive()->vertexData();
	GLfloat *centroids = pinfo->primitive()->centroids();
	GLfloat newr[9], newn[9], norm[3], colour[12], *colourptr;
	// OPTIMISE - Do something better with norm[]
	int voff = 0, bin, m;

	// For speed, different loops depending on type of vertexData...
	if (pinfo->primitive()->colouredVertexData())
	{
		for (int n=0; n<pinfo->primitive()->nDefinedTypes(); ++n)
		{
			// Transform triangle centroid into world coordinates to decide bin
			pinfo->localTransform().multiply(&centroids[n*3], newr);
			bin = max(0,int((-newr[2]-startZ_)/sliceWidth_));
			if (bin >= nSlices_) bin = nSlices_-1;
			// Vertex data contains packed: colour[voff], normal[voff+4], vertex[voff+7]
			// Vertex 1
			for (m=0; m<4; ++m) colour[m] = vertexData[voff++];
			for (m=0; m<3; ++m) norm[m] = vertexData[voff++] + vertexData[voff-4];
			pinfo->localTransform().multiply(&vertexData[voff], newr);
			pinfo->localTransform().multiply(norm, newn);
			voff += 3;
			// Vertex 2
			for (m=4; m<8; ++m) colour[m] = vertexData[voff++];
			for (m=0; m<3; ++m) norm[m] = vertexData[voff++] + vertexData[voff-4];
			pinfo->localTransform().multiply(&vertexData[voff], &newr[3]);
			pinfo->localTransform().multiply(norm, &newn[3]);
			voff += 3;
			// Vertex 3
			for (m=8; m<12; ++m) colour[m] = vertexData[voff++];
			for (m=0; m<3; ++m) norm[m] = vertexData[voff++] + vertexData[voff-4];
			pinfo->localTransform().multiply(&vertexData[voff], &newr[6]);
			pinfo->localTransform().multiply(norm, &newn[6]);
			voff += 3;
			// Subtract transformed position from normal to get new normal
			for (m=0; m<9; ++m) newn[m] -= newr[m];
			// Finally, add triangle
			triangleLists_[bin].addTriangle(newr, newn, colour);
		}
	}
	else
	{
		colourptr = pinfo->colour();
		for (int n=0; n<pinfo->primitive()->nDefinedTypes(); ++n)
		{
			// Transform triangle centroid into world coordinates to decide bin
			pinfo->localTransform().multiply(&centroids[n*3], newr);
			bin = max(0,int((-newr[2]-startZ_)/sliceWidth_));
			if (bin >= nSlices_) bin = nSlices_-1;
			// Vertex data contains packed: normal[voff], vertex[voff+3]
			// Vertex 1
			for (m=0; m<3; ++m) norm[m] = vertexData[voff+3] + vertexData[voff++];
			pinfo->localTransform().multiply(&vertexData[voff], newr);
			pinfo->localTransform().multiply(norm, newn);
			voff += 3;
			// Vertex 2
			for (m=0; m<3; ++m) norm[m] = vertexData[voff+3] + vertexData[voff++];
			pinfo->localTransform().multiply(&vertexData[voff], &newr[3]);
			pinfo->localTransform().multiply(norm, &newn[3]);
			voff += 3;
			// Vertex 3
			for (m=0; m<3; ++m) norm[m] = vertexData[voff+3] + vertexData[voff++];
			pinfo->localTransform().multiply(&vertexData[voff], &newr[6]);
			pinfo->localTransform().multiply(norm, &newn[6]);
			voff += 3;
			// Subtract transformed position from normal to get new normal
			for (m=0; m<9; ++m) newn[m] -= newr[m];
			// Finally, add triangle
			triangleLists_[bin].addTriangleSingleColour(newr, newn, colourptr);
		}
	}
}

// Sent triangles to GL (in correct order)
void TriangleChopper::sendToGL()
{
	for (int n=nSlices_-1; n>=0; --n) triangleLists_[n].sendToGL();
}
