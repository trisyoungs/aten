/*
	*** Render GroupRenderGroup
	*** src/render/rendergroup.cpp
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

#include "render/rendergroup.h"
#include "render/glextensions.h"
#include "base/messenger.h"
#ifdef __APPLE__
#include <OpenGL/gl.h>
#endif

ATEN_USING_NAMESPACE

// Constructor
RenderGroup::RenderGroup()
{
	extraNormalLines_.setNoInstances();
	extraNormalLines_.initialise(GL_LINES, true);
	extraBoldLines_.setNoInstances();
	extraBoldLines_.initialise(GL_LINES, true);
	extraSolidTriangles_.setNoInstances();
	extraSolidTriangles_.initialise(GL_TRIANGLES, true);
	extraWireTriangles_.setNoInstances();
	extraWireTriangles_.initialise(GL_TRIANGLES, true);
}

/*
 * Object Lists
 */

// Clear lists
void RenderGroup::clear()
{
	solidTrianglePrimitives_.clear();
	transparentTrianglePrimitives_.clear();
	wireTrianglePrimitives_.clear();
	normalLinePrimitives_.clear();
	boldLinePrimitives_.clear();
	extraNormalLines_.forgetAll();
	extraBoldLines_.forgetAll();
	extraSolidTriangles_.forgetAll();
	extraWireTriangles_.forgetAll();
}

// Add text primitive (2D)
void RenderGroup::addText2D(int x, int y, QString text, bool rightAlign)
{
	printf("Text rendering in 2D is broken.\n");
//         textPrimitives_.add(x, contextHeight_-y, text, addChar, rightalign);  ATEN2 TODO
}

// Add text primitive (3D)
void RenderGroup::addText3D(Vec3<double> pos, QString text, bool rightalign)
{
	printf("Text rendering in 3D is broken.\n");
//         textPrimitives_.add(x, contextHeight_-y, text, addChar, rightalign);  ATEN2 TODO
}

// Add triangle primitive in specified colour
void RenderGroup::addTriangles(Primitive& targetPrimitive, Matrix& transform, Vec4<GLfloat>& colour, GLenum fillMode, GLfloat lineWidth)
{
	// Check type of supplied primitive
	if (targetPrimitive.type() != GL_TRIANGLES)
	{
		printf("Warning: RenderGroup is rejecting primitive for triangle list, since it doesn't contain GL_TRIANGLES.\n");
		return;
	}

	// Check if supplied primitive contains colour data already
	if (targetPrimitive.colouredVertexData()) printf("Warning: Triangle primitive added to RenderGroup contains colour data and had colour supplied.\n");
	
	// Create new PrimitiveInfo in correct list
	PrimitiveInfo* pi = new PrimitiveInfo(targetPrimitive, transform, colour, lineWidth);
	if (fillMode == GL_FILL)
	{
		if (colour.w > 0.99) solidTrianglePrimitives_.own(pi);
		else transparentTrianglePrimitives_.own(pi);
	}
	else wireTrianglePrimitives_.own(pi);
}


// Add triangle primitive
void RenderGroup::addTriangles(Primitive& targetPrimitive, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
{
	// Check type of supplied primitive
	if (targetPrimitive.type() != GL_TRIANGLES)
	{
		printf("Warning: RenderGroup is rejecting primitive for triangle list, since it doesn't contain GL_TRIANGLES.\n");
		return;
	}

	// Check if supplied primitive contains colour data already
	if (!targetPrimitive.colouredVertexData()) printf("Warning: Triangle primitive added to RenderGroup contains no colour data and has no colour supplied.\n");
	
	// Create new PrimitiveInfo in correct list
	PrimitiveInfo* pi = new PrimitiveInfo(targetPrimitive, transform, lineWidth);
	if (fillMode == GL_FILL) solidTrianglePrimitives_.own(pi);
	else wireTrianglePrimitives_.own(pi);
}

// Add line primitive in specified colour
void RenderGroup::addLines(Primitive& targetPrimitive, Matrix& transform, Vec4<GLfloat>& colour, bool bold)
{
	// Check type of supplied primitive
	if (targetPrimitive.type() != GL_LINES)
	{
		printf("Warning: RenderGroup is rejecting primitive for lines list, since it doesn't contain GL_LINES.\n");
		return;
	}

	// Check if supplied primitive contains colour data already
	if (targetPrimitive.colouredVertexData()) printf("Warning: Line primitive added to RenderGroup contains colour data and had colour supplied.\n");
	
	// Create new PrimitiveInfo in correct list
	PrimitiveInfo* pi = new PrimitiveInfo(targetPrimitive, transform, colour);
	if (bold) normalLinePrimitives_.own(pi);
	else boldLinePrimitives_.own(pi);
}

// Add line primitive (which has it's own colour info)
void RenderGroup::addLines(Primitive& targetPrimitive, Matrix& transform, bool bold)
{
	// Check type of supplied primitive
	if (targetPrimitive.type() != GL_LINES)
	{
		printf("Warning: RenderGroup is rejecting primitive for lines list, since it doesn't contain GL_LINES.\n");
		return;
	}

	// Check if supplied primitive contains colour data already
	if (targetPrimitive.colouredVertexData()) printf("Warning: Line primitive added to RenderGroup does not contain colour data and had no colour supplied.\n");
	
	// Create new PrimitiveInfo in correct list
	PrimitiveInfo* pi = new PrimitiveInfo(targetPrimitive, transform);
	if (bold) normalLinePrimitives_.own(pi);
	else boldLinePrimitives_.own(pi);
}

// Sort and render filtered polygons by depth
void RenderGroup::sendToGL(Matrix& modelTransformationMatrix)
{
	Matrix A;

	// Solid triangles
	glEnable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	for (PrimitiveInfo* pi = solidTrianglePrimitives_.first(); pi != NULL; pi = pi->next)
	{
		Primitive& primitive = pi->primitive();
		if (!primitive.colouredVertexData()) glColor4fv(pi->colour());
		A = modelTransformationMatrix * pi->localTransform();
		glLoadMatrixd(A.matrix());
		primitive.sendToGL();
	}

	// Extra solid triangles
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraSolidTriangles_.sendToGL();

	// Wire triangles
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	for (PrimitiveInfo* pi = wireTrianglePrimitives_.first(); pi != NULL; pi = pi->next)
	{
		Primitive& primitive = pi->primitive();
		if (!primitive.colouredVertexData()) glColor4fv(pi->colour());
		glLineWidth(pi->lineWidth());
		A = modelTransformationMatrix * pi->localTransform();
		glLoadMatrixd(A.matrix());
		primitive.sendToGL();
	}

	// Extra wire triangles
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraWireTriangles_.sendToGL();

	// Normal lines
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glLineWidth(1.0f);
	for (PrimitiveInfo* pi = normalLinePrimitives_.first(); pi != NULL; pi = pi->next)
	{
		Primitive& primitive = pi->primitive();
		if (!primitive.colouredVertexData()) glColor4fv(pi->colour());
		A = modelTransformationMatrix * pi->localTransform();
		glLoadMatrixd(A.matrix());
		primitive.sendToGL();
	}

	// Extra normal lines
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraNormalLines_.sendToGL();

	// Bold lines
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glLineWidth(3.0f);
	for (PrimitiveInfo* pi = boldLinePrimitives_.first(); pi != NULL; pi = pi->next)
	{
		Primitive& primitive = pi->primitive();
		if (!primitive.colouredVertexData()) glColor4fv(pi->colour());
		A = modelTransformationMatrix * pi->localTransform();
		glLoadMatrixd(A.matrix());
		primitive.sendToGL();
	}

	// Extra bold lines
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraBoldLines_.sendToGL();
	
	// Transparent triangles
	glEnable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	for (PrimitiveInfo* pi = transparentTrianglePrimitives_.first(); pi != NULL; pi = pi->next)
	{
		Primitive& primitive = pi->primitive();
		if (!primitive.colouredVertexData()) glColor4fv(pi->colour());
		A = modelTransformationMatrix * pi->localTransform();
		glLoadMatrixd(A.matrix());
		primitive.sendToGL();
	}
}

