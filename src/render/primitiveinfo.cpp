/*
	*** Primitive Info
	*** src/render/primitiveinfo.cpp
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

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#endif
#include "render/primitiveinfo.h"

ATEN_USING_NAMESPACE

// Constructor
PrimitiveInfo::PrimitiveInfo(Primitive& prim, Matrix& transform, GLfloat lineWidth) : ListItem<PrimitiveInfo>(), primitive_(prim), localTransform_(transform), lineWidth_(lineWidth)
{
	// Private variables
	colour_[0] = 0.0;
	colour_[1] = 0.0;
	colour_[2] = 0.0;
	colour_[3] = 1.0;
	if (lineWidth_ < 0.0) lineWidth_ = 1.0;
}

// Constructor
PrimitiveInfo::PrimitiveInfo(Primitive& prim, Matrix& transform, Vec4<GLfloat>& colour, GLfloat lineWidth) : ListItem<PrimitiveInfo>(), primitive_(prim), localTransform_(transform), lineWidth_(lineWidth)
{
	if (lineWidth_ < 0.0) lineWidth_ = 1.0;
	colour_[0] = colour.x;
	colour_[1] = colour.y;
	colour_[2] = colour.z;
	colour_[3] = colour.w;
}

// Return reference to primitive
Primitive& PrimitiveInfo::primitive()
{
	return primitive_;
}

// Return local transformation of primitive
Matrix& PrimitiveInfo::localTransform()
{
	return localTransform_;
}

// Return colour array
const GLfloat* PrimitiveInfo::colour() const
{
	return colour_;
}

// Line width to use for primitive (if appropriate)
GLfloat PrimitiveInfo::lineWidth() const
{
	return lineWidth_;
}
