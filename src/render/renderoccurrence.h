/*
	*** Render Occurrence
	*** src/render/renderoccurrence.h
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

#ifndef ATEN_RENDEROCCURRENCE_H
#define ATEN_RENDEROCCURRENCE_H

#include "render/renderoccurrencechunk.h"
#include <QOpenGLFunctions>
#include "templates/list.h"
#include "templates/vector4.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Primitive;
class Matrix;

// RenderOccurrence
class RenderOccurrence : public QOpenGLFunctions, public ListItem<RenderOccurrence>
{
	public:
	// Constructor
	RenderOccurrence(Primitive& targetPrimitive);


	/*
	 * Primitive Data
	 */
	private:
	// Target Primitive
	Primitive& primitive_;
	// Occurrences of primitive to be rendered
	List<RenderOccurrenceChunk> chunks_;
	// Current RenderOccurrenceChunk target
	RenderOccurrenceChunk* currentChunk_;

	public:
	// Clear data, but do not free arrays
	void clear();
	// Return target primitive
	Primitive& primitive() const;
	// Add occurrence (with colour as Vec4)
	void addOccurrence(Matrix& transform, Vec4<GLfloat>& colour);
	// Add occurrence (with colour as GLfloat*)
	void addOccurrence(Matrix& transform, GLfloat* colour);
	// Add occurrence (with no colour)
	void addOccurrence(Matrix& transform);


	/*
	 * GL
	 */
	public:
	// Send to GL
	void sendToGL(Matrix& modelTransformationMatrix);
};

ATEN_END_NAMESPACE

#endif
