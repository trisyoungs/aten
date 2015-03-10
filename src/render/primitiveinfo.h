/*
	*** Primitive Info
	*** src/render/primitiveinfo.h
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

#ifndef ATEN_PRIMITIVEINFO_H
#define ATEN_PRIMITIVEINFO_H

#include "math/matrix.h"
#include "templates/list.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Primitive;

// Primitive Info
class PrimitiveInfo : public ListItem<PrimitiveInfo>
{
	public:
	// Constructors
	PrimitiveInfo(Primitive& prim, Matrix& transform, GLfloat lineWidth = -1.0);
	PrimitiveInfo(Primitive& prim, Matrix& transform, Vec4<GLfloat>& colour, GLfloat lineWidth = -1.0);

	private:
	// Target primitive
	Primitive& primitive_;
	// Local transformation of primitive
	Matrix localTransform_;
	// Colour of primitive (if vertexData_ doesn't contain colour information)
	GLfloat colour_[4];
	// Line width to use for primitive (if appropriate)
	GLfloat lineWidth_;
	
	public:
	// Return reference to stored primitive
	Primitive& primitive();
	// Return local transformation of primitive
	Matrix& localTransform();
	// Return colour array
	const GLfloat* colour() const;
	// Line width to use for primitive (if appropriate)
	GLfloat lineWidth() const;
};

ATEN_END_NAMESPACE

#endif
