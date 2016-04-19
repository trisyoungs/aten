/*
	*** Render Occurrence
	*** src/render/renderoccurrencechunk.h
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

#ifndef ATEN_RENDEROCCURRENCECHUNK_H
#define ATEN_RENDEROCCURRENCECHUNK_H

#include <QOpenGLFunctions>
#include "templates/list.h"
#include "templates/vector4.h"

// #define OCCURRENCECHUNKSIZE 65536
#define OCCURRENCECHUNKSIZE 256

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Primitive;
class Matrix;

// RenderOccurrenceChunk
class RenderOccurrenceChunk : public ListItem<RenderOccurrenceChunk>
{
	public:
	// Constructor
	RenderOccurrenceChunk();
	// Destructor
	~RenderOccurrenceChunk();


	/*
	 * Data
	 */
	private:
	// Local transformations
	Matrix* localTransform_;
	// Red colour component
	GLfloat* colourR_;
	// Green colour component
	GLfloat* colourG_;
	// Blue colour component
	GLfloat* colourB_;
	// Alpha colour component
	GLfloat* colourA_;
	// Number of occurrences currently defined in arrays
	int nDefined_;

	public:
	// Clear arrays (but do not delete)
	void clear();
	// Return number of occurrences currently defined in array
	int nDefined() const;
	// Return whether arrays are full
	bool isFull() const;
	// Set next data (with colour as Vec4)
	void setNextData(Matrix& transform, Vec4<GLfloat>& colour);
	// Set next data (with colour as GLfloat*)
	void setNextData(Matrix& transform, GLfloat colour[4]);
	// Set next data (with no colour)
	void setNextData(Matrix& transform);
	// Return local transformations array
	const Matrix* localTransform();
	// Return red colour component array
	const GLfloat* red();
	// Return green colour component array
	const GLfloat* green();
	// Return blue colour component array
	const GLfloat* blue();
	// Return alpha colour component array
	const GLfloat* alpha();
};

ATEN_END_NAMESPACE

#endif
