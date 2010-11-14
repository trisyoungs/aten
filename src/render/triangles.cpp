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
	createEmpty(GL_TRIANGLES, TRIANGLECHUNKSIZE*3);
};

/*
// Triangle List
*/

// Constructor
TriangleList::TriangleList()
{
	currentTriangles_ = triangles_.add();
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
void TriangleList::addTriangle(GLfloat *vertices, GLfloat *normals)
{
	// Check current list for space
	if (currentTriangles_->full()) currentTriangles_ = triangles_.add();
	// Add triangle to list
	currentTriangles_->addVertexAndNormal(vertices[0], vertices[1], vertices[2], normals[0], normals[1], normals[2], FALSE);
	currentTriangles_->addVertexAndNormal(vertices[3], vertices[4], vertices[5], normals[3], normals[4], normals[5], FALSE);
	currentTriangles_->addVertexAndNormal(vertices[6], vertices[7], vertices[8], normals[6], normals[7], normals[8], FALSE);
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
	printf("CREATED TRIANGLELIST ARRAY FOR %i SLICES.\n", nSlices_);
}

// Store primitive's triangles
void TriangleChopper::storeTriangles(PrimitiveInfo *pinfo)
{
// 	printf("Storing %i triangles present in primitive...\n", pinfo->primitive()->nDefinedVertices() / 3);
	GLfloat *vertices = pinfo->primitive()->vertices();
	GLfloat *normals = pinfo->primitive()->normals();
	GLfloat *centroids = pinfo->primitive()->centroids();
	GLfloat newr[9], newn[9];
	int voff = 0, bin;
	if (pinfo->matrixTransformDefined())
	{
		for (int n=0; n<pinfo->primitive()->nDefinedTypes(); ++n)
		{
			// Transform triangle centroid into world coordinates to decide bin
			pinfo->localTransform().multiply(&centroids[n], newr);
			bin = int((-newr[2]-startZ_)/sliceWidth_);
			if (bin >= nSlices_) bin = nSlices_-1;
			// Transform triangle vertices into world coordinates and stored
			pinfo->localTransform().multiply(&vertices[voff], newr);
			pinfo->localTransform().multiply(&normals[voff], newn);
			voff += 3;
			pinfo->localTransform().multiply(&vertices[voff], &newr[3]);
			pinfo->localTransform().multiply(&normals[voff], &newn[3]);
			voff += 3;
			pinfo->localTransform().multiply(&vertices[voff], &newr[6]);
			pinfo->localTransform().multiply(&normals[voff], &newn[6]);
			voff += 3;
			triangleLists_[bin].addTriangle(newr, newn);
		}
	}
	else
	{
	}
	
}

// Sent triangles to GL (in correct order)
void TriangleChopper::sendToGL()
{
	for (int n=nSlices_-1; n>=0; --n) triangleLists_[n].sendToGL();
}
