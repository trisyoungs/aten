/*
	*** Render List
	*** src/render/renderlist.h
	Copyright T. Youngs 2013-2017

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

#ifndef ATEN_RENDERLIST_H
#define ATEN_RENDERLIST_H

#include "render/renderoccurrence.h"
#include "base/prefs.h"
#include "templates/list.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Primitive;

// RenderList
class RenderList
{
	public:
	// Constructor
	RenderList();


	/*
	 * Primitive Data
	 */
	private:
	// Last chunksize passed through clear()
	int lastChunkSize_;
	// Occurrences of primitive to be rendered
	List<RenderOccurrence> targets_;

	private:
	// Return (or create) list for specified primitive
	RenderOccurrence* occurrenceForPrimitive(Primitive& primitive);

	public:
	// Clear chunk data (recreating with new chunksize if necessary)
	void clear(int chunkSize = MINIMUMOCCURRENCECHUNKSIZE);
	// Add primitive to internal lists
	void addOccurrence(Primitive& primitive, Matrix& transform, Vec4<GLfloat>& colour);
	// Add primitive to internal lists (without colour or linewidth)
	void addOccurrence(Primitive& primitive, Matrix& transform);


	/*
	 * GL
	 */
	public:
	// Send to GL
	void sendToGL(Matrix& modelTransformationMatrix);
};

ATEN_END_NAMESPACE

#endif
