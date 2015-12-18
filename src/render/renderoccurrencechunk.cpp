/*
	*** Render Occurrence Chunk
	*** src/render/renderoccurrencechunk.cpp
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

#include "render/renderoccurrencechunk.h"
#include "render/primitive.h"
#include "math/matrix.h"
#include <QOpenGLFunctions>

ATEN_USING_NAMESPACE

// Constructor
RenderOccurrenceChunk::RenderOccurrenceChunk()
{
	localTransform_ = new Matrix[OCCURRENCECHUNKSIZE];
	colourR_ = new GLfloat[OCCURRENCECHUNKSIZE];
	colourG_ = new GLfloat[OCCURRENCECHUNKSIZE];
	colourB_ = new GLfloat[OCCURRENCECHUNKSIZE];
	colourA_ = new GLfloat[OCCURRENCECHUNKSIZE];
	nDefined_ = 0;
}

// Destructor
RenderOccurrenceChunk::~RenderOccurrenceChunk()
{
	delete[] localTransform_;
	delete[] colourR_;
	delete[] colourG_;
	delete[] colourB_;
	delete[] colourA_;
}

/*
 * Data
 */

// Clear arrays (but do not delete)
void RenderOccurrenceChunk::clear()
{
	nDefined_ = 0;
}

// Return number of occurrences currently defined in array
int RenderOccurrenceChunk::nDefined() const
{
	return nDefined_;
}

// Return whether arrays are full
bool RenderOccurrenceChunk::isFull() const
{
	return nDefined_ == OCCURRENCECHUNKSIZE;
}

// Set next data (with colour as Vec4)
void RenderOccurrenceChunk::setNextData(Matrix& transform, Vec4<GLfloat>& colour)
{
	if (isFull())
	{
		printf("Internal Error: RenderoccurrenceChunk is full, so can't setNextData...\n");
		return;
	}

	localTransform_[nDefined_] = transform;
	colourR_[nDefined_] = colour.x;
	colourG_[nDefined_] = colour.y;
	colourB_[nDefined_] = colour.z;
	colourA_[nDefined_] = colour.w;

	++nDefined_;
}

// Set next data (with colour as GLfloat*)
void RenderOccurrenceChunk::setNextData(Matrix& transform, GLfloat colour[4])
{
	if (isFull())
	{
		printf("Internal Error: RenderoccurrenceChunk is full, so can't setNextData...\n");
		return;
	}

	localTransform_[nDefined_] = transform;
	colourR_[nDefined_] = colour[0];
	colourG_[nDefined_] = colour[1];
	colourB_[nDefined_] = colour[2];
	colourA_[nDefined_] = colour[3];

	++nDefined_;
}

// Set next data (with no colour)
void RenderOccurrenceChunk::setNextData(Matrix& transform)
{
	if (isFull())
	{
		printf("Internal Error: RenderoccurrenceChunk is full, so can't setNextData...\n");
		return;
	}

	localTransform_[nDefined_] = transform;

	++nDefined_;
}

// Return local transformations array
const Matrix* RenderOccurrenceChunk::localTransform()
{
	return localTransform_;
}

// Return red colour component array
const GLfloat* RenderOccurrenceChunk::red()
{
	return colourR_;
}

// Return green colour component array
const GLfloat* RenderOccurrenceChunk::green()
{
	return colourG_;
}

// Return blue colour component array
const GLfloat* RenderOccurrenceChunk::blue()
{
	return colourB_;
}

// Return alpha colour component array
const GLfloat* RenderOccurrenceChunk::alpha()
{
	return colourA_;
}
