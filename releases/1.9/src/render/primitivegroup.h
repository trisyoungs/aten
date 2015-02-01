/*
	*** Primitive Group
	*** src/render/primitivegroup.h
	Copyright T. Youngs 2007-2015

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

#ifndef ATEN_PRIMITIVEGROUP_H
#define ATEN_PRIMITIVEGROUP_H

#include "render/primitive.h"
#include "base/matrix.h"

// Forward Declarations
class QGLContext;

// Primitive Group
class PrimitiveGroup
{
	public:
	// Constructor / Destructor
	PrimitiveGroup();
	~PrimitiveGroup();

	private:
	// Array of Primitives, corresponding to different levels of detail
	Primitive *primitives_;
	// Number of levels of detail catered for by this primitive (number of primitives in array)
	int nLevelsOfDetail_;
	// Base name of primitive group (for bug-tracking)
	Dnchar name_;

	public:
	// Clear old primitives array and allocate new one
	void clear();
	// Create instance for all stored primitives in the group
	void pushInstance(const QGLContext *context);
	// Pop topmost instance
	void popInstance(const QGLContext *context);
	// Return primitive corresponding to level of detail specified
	Primitive &primitive(int lod);
	// Set name of primitive group
	void setName(const char *s);
	// Send to OpenGL (i.e. render) at specified level of detail
	void sendToGL(int lod);
};

#endif
