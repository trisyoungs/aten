/*
	*** Triangle Chopper
	*** src/render/triangles.cpp
	Copyright T. Youngs 2007-2012

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

#define NOMINMAX
#include "render/triangles.h"
#include "render/primitiveinfo.h"
#include <algorithm>

using namespace std;

/*
// Triangle Chopper
*/

// Constructor
TriangleChopper::TriangleChopper()
{
	startZ_ = 0.0;
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
void TriangleChopper::initialise(double startz, int nbins, double slicewidth)
{
	clear();
	// Store new range values and reallocate array
	startZ_ = startz;
	sliceWidth_ = slicewidth;
	nSlices_ = nbins;
	if (nSlices_ < 0) printf("Internal Error: Number of calculated slices in chopper is negative.\n");
	triangleLists_ = new Primitive[nSlices_];
	for (int n=0; n<nSlices_; ++n)
	{
		triangleLists_[n].setColourData(TRUE);
		triangleLists_[n].setNoInstances();
	}
	msg.print(Messenger::Verbose, "Created %i bins for transparency correction.\n", nSlices_);
}

// Store primitive's triangles
void TriangleChopper::storeTriangles(PrimitiveInfo* pinfo, Matrix& worldtransform)
{
	GLfloat *vertexData, *centroids;
	VertexChunk *chunk;
	Matrix transform = worldtransform * pinfo->localTransform();
	GLfloat newr[9], newn[9], norm[3], colour[12], *colourptr;
	int voff, bin, m;

	Primitive *prim = pinfo->primitive();
	if (prim == NULL) prim = pinfo->primitive(worldtransform);

	// For speed, different loops depending on type of vertexData...
	if (prim->colouredVertexData())
	{
		for (chunk = prim->vertexChunks(); chunk != NULL; chunk = chunk->next)
		{
			vertexData = chunk->vertexData();
			centroids = chunk->centroids();
			voff = 0;
			for (int n=0; n<chunk->nDefinedTypes(); ++n)
			{
				// Transform triangle centroid into world coordinates to decide bin
				transform.multiply(&centroids[n*3], newr);
				bin = max(0,int((-newr[2]-startZ_)/sliceWidth_));
				if (bin >= nSlices_) bin = nSlices_-1;
				// Vertex data contains packed: colour[voff], normal[voff+4], vertex[voff+7]
				// Vertex 1
				for (m=0; m<4; ++m) colour[m] = vertexData[voff++];
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff] + vertexData[voff+3]; ++voff; }
				transform.multiply(&vertexData[voff], newr);
				transform.multiply(norm, newn);
				voff += 3;
				// Vertex 2
				for (m=4; m<8; ++m) colour[m] = vertexData[voff++];
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff] + vertexData[voff+3]; ++voff; }
				transform.multiply(&vertexData[voff], &newr[3]);
				transform.multiply(norm, &newn[3]);
				voff += 3;
				// Vertex 3
				for (m=8; m<12; ++m) colour[m] = vertexData[voff++];
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff] + vertexData[voff+3]; ++voff; }
				transform.multiply(&vertexData[voff], &newr[6]);
				transform.multiply(norm, &newn[6]);
				voff += 3;
				// Subtract transformed position from normal to get new normal
				for (m=0; m<9; ++m) newn[m] -= newr[m];
				// Finally, add triangle
				triangleLists_[bin].defineTriangle(newr, newn, colour);
			}
		}
	}
	else
	{
		colourptr = pinfo->colour();
		for (chunk = prim->vertexChunks(); chunk != NULL; chunk = chunk->next)
		{
			vertexData = chunk->vertexData();
			centroids = chunk->centroids();
			voff = 0;
			for (int n=0; n<chunk->nDefinedTypes(); ++n)
			{
				// Transform triangle centroid into world coordinates to decide bin
				transform.multiply(&centroids[n*3], newr);
				bin = max(0,int((-newr[2]-startZ_)/sliceWidth_));
				if (bin >= nSlices_) bin = nSlices_-1;
				// Vertex data contains packed: normal[voff], vertex[voff+3]
				// Vertex 1
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff+3] + vertexData[voff]; ++voff; }
				transform.multiply(&vertexData[voff], newr);
				transform.multiply(norm, newn);
				voff += 3;
				// Vertex 2
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff+3] + vertexData[voff]; ++voff; }
				transform.multiply(&vertexData[voff], &newr[3]);
				transform.multiply(norm, &newn[3]);
				voff += 3;
				// Vertex 3
				for (m=0; m<3; ++m) { norm[m] = vertexData[voff+3] + vertexData[voff]; ++voff; }
				transform.multiply(&vertexData[voff], &newr[6]);
				transform.multiply(norm, &newn[6]);
				voff += 3;
				// Subtract transformed position from normal to get new normal
				for (m=0; m<9; ++m) newn[m] -= newr[m];
				// Finally, add triangle
				triangleLists_[bin].defineTriangleSingleColour(newr, newn, colourptr);
			}
		}
	}
}

// Sent triangles to GL (in correct order)
void TriangleChopper::sendToGL()
{
	for (int n=nSlices_-1; n>=0; --n) triangleLists_[n].sendToGL();
}
