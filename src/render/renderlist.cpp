/*
	*** Render List
	*** src/render/renderlist.cpp
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

#include "render/renderlist.h"

ATEN_USING_NAMESPACE

// Constructor
RenderList::RenderList()
{
}

/*
 * Primitive Data
 */

// Return (or create) list for specified primitive
RenderOccurrence* RenderList::occurrenceForPrimitive(Primitive& primitive)
{
	// Loop over existing targets...
	for (RenderOccurrence* occurrence = targets_.first(); occurrence != NULL; occurrence = occurrence->next) if (&primitive == &occurrence->primitive()) return occurrence;

	// No matching occurrence - so create new one and return it
	RenderOccurrence* occurrence = new RenderOccurrence(primitive, lastChunkSize_);
	targets_.own(occurrence);
	return occurrence;
}

// Clear chunk data (recreating with new chunksize if necessary)
void RenderList::clear(int chunkSize)
{
	// Clamp newChunkSize
	if (chunkSize < MINIMUMOCCURRENCECHUNKSIZE) chunkSize = MINIMUMOCCURRENCECHUNKSIZE;
	else if (chunkSize > MAXIMUMOCCURRENCECHUNKSIZE) chunkSize = MAXIMUMOCCURRENCECHUNKSIZE;

	lastChunkSize_ = chunkSize;
	for (RenderOccurrence* occurrence = targets_.first(); occurrence != NULL; occurrence = occurrence->next) occurrence->clear(lastChunkSize_);
}

// Add primitive to internal lists
void RenderList::addOccurrence(Primitive& primitive, Matrix& transform, Vec4<GLfloat>& colour)
{
	// Get (or create) occurrence for primitive
	RenderOccurrence* occurrence = occurrenceForPrimitive(primitive);

	occurrence->addOccurrence(transform, colour);
}

// Add primitive to internal lists (without colour)
void RenderList::addOccurrence(Primitive& primitive, Matrix& transform)
{
	// Get (or create) occurrence for primitive
	RenderOccurrence* occurrence = occurrenceForPrimitive(primitive);

	occurrence->addOccurrence(transform);
}

/*
 * GL
 */

// Send to GL
void RenderList::sendToGL(Matrix& modelTransformationMatrix)
{
	// Loop over Primitive targets in list
	for (RenderOccurrence* target = targets_.first(); target != NULL; target = target->next)
	{
		target->sendToGL(modelTransformationMatrix);
	}
}
