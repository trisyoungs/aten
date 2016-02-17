/*
	*** Rendering Primitive Instance
	*** src/render/primitiveinstance.cpp
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

// Need to include <QString>, <GL/glx.h>, and then "render/glextensions.h" in that order, to prevent hideous errors.
// #include <QString>
// #ifdef __unix__
// #include <GL/glx.h>
// #endif
#include "render/primitiveinstance.h"

ATEN_USING_NAMESPACE

// Static members
PrimitiveInstance::InstanceType PrimitiveInstance::instanceType_ = PrimitiveInstance::VBOInstance;

// Constructor
PrimitiveInstance::PrimitiveInstance() : ListItem<PrimitiveInstance>()
{
	// Private variables
	context_ = NULL;
	listObject_ = 0;
	vboVertexObject_ = 0;
	vboIndexObject_ = 0;
}

// Return global instance type to use
PrimitiveInstance::InstanceType PrimitiveInstance::instanceType()
{
	return instanceType_;
}

// Set global instance type to use
void PrimitiveInstance::setInstanceType(PrimitiveInstance::InstanceType instanceType)
{
	instanceType_ = instanceType;
}

// Return context to which primitive instance is associated
const QOpenGLContext* PrimitiveInstance::context()
{
	return context_;
}

// Set display list data
void PrimitiveInstance::setDisplayList(const QOpenGLContext* context, GLuint listObject)
{
	context_ = context;
	listObject_ = listObject;
}

// Set vbo object data
void PrimitiveInstance::setVBO(const QOpenGLContext* context, GLuint vertexObject, GLuint indexObject)
{
	context_ = context;
	vboVertexObject_ = vertexObject;
	vboIndexObject_ = indexObject;
}

// Return display list object for instance
GLuint PrimitiveInstance::listObject()
{
	return listObject_;
}

// Return VBO ID of vertex array for instance
GLuint PrimitiveInstance::vboVertexObject()
{
	return vboVertexObject_;
}

// Return VBO ID of index array for instance
GLuint PrimitiveInstance::vboIndexObject()
{
	return vboIndexObject_;
}
