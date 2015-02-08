/*
	*** Primitive Info
	*** src/render/primitiveinfo.cpp
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

#ifdef _WIN32
#include <windows.h>
#include <GL/gl.h>
#endif
#include "render/primitiveinfo.h"

// Constructor
PrimitiveInfo::PrimitiveInfo() : ListItem<PrimitiveInfo>()
{
	// Private variables
	primitive_ = NULL;
	fillMode_ = GL_FILL;
	lineWidth_ = 1.0f;
	colour_[0] = 0.0;
	colour_[1] = 0.0;
	colour_[2] = 0.0;
	colour_[3] = 1.0;
}

// Set primitive info data
void PrimitiveInfo::set(Primitive* prim, GLfloat* colour, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
{
	primitive_ = prim;
	localTransform_ = transform;
	fillMode_ = fillMode;
	lineWidth_ = lineWidth;
	if (colour != NULL) for (int n=0; n<4; ++n) colour_[n] = colour[n];
}

// Return pointer to primitive
Primitive* PrimitiveInfo::primitive()
{
	return primitive_;
}

// Return local transformation of primitive
Matrix& PrimitiveInfo::localTransform()
{
	return localTransform_;
}

// Return colour array
GLfloat* PrimitiveInfo::colour()
{
	return colour_;
}

// Return polygon fill mode
GLenum PrimitiveInfo::fillMode()
{
	return fillMode_;
}

// Return line width
GLfloat PrimitiveInfo::lineWidth()
{
	return lineWidth_;
}

