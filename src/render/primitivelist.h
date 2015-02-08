/*
	*** Rendering Primitive List
	*** src/render/primitivelist.h
	Copyright T. Youngs 2013-2014

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

#ifndef ATEN_PRIMITIVELIST_H
#define ATEN_PRIMITIVELIST_H

#include "render/primitive.h"
#include "templates/list.h"

// Forward Declarations
class QGLContext;
class Viewer;

// Rendering Primitive List
class PrimitiveList
{
	public:
	// Constructor / Destructor
	PrimitiveList();
	~PrimitiveList();


	/*
	 * Data
	 */
	private:
	// List of Primitives owned and managed by this list
	List<Primitive> primitives_;

	public:
	// Clear existing data
	void clear();
	// Forget all data, leaving arrays intact
	void forgetAll();
	// Reinitialise list so it is large enough to accomodate specified number of Primitives
	void reinitialise(int newSize, bool allowShrink, int maxVertices, int maxIndices, GLenum type, bool colourData);
	// Add a new primitive to the end of the list
	Primitive* addPrimitive(int maxVertices, int maxIndices, GLenum type, bool colourData);
	// Return total number of defined vertices
	int nDefinedVertices();
	// Return total number of defined indices
	int nDefinedIndices();
	// Push instance layer
	void pushInstance(const QGLContext *context, GLExtensions* extensions);
	// Pop topmost instance layer
	void popInstance(const QGLContext *context);
	// Return number of instances of topmost primitive
	int nInstances();
	// Send to OpenGL (i.e. render)
	void sendToGL();


	/*
	 * Operators
	 */
	public:
	// Element access operator
	Primitive *operator[](int index);
};

#endif
