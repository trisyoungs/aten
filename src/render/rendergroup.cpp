/*
	*** Render Group
	*** src/render/rendergroup.cpp
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

#include "render/rendergroup.h"
#include "base/messenger.h"
#include "render/fontinstance.h"
#include <QOpenGLContext>

ATEN_USING_NAMESPACE

// Constructor
RenderGroup::RenderGroup()
{
	extraNormalLines_.setNoInstances();
	extraNormalLines_.initialise(GL_LINES, true);
	extraBoldLines_.setNoInstances();
	extraBoldLines_.initialise(GL_LINES, true);
	overlayLines_.setNoInstances();
	overlayLines_.initialise(GL_LINES, true);
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
	overlayLines_.forgetAll();
	textPrimitives_.clear();
	overlayTextPrimitives_.clear();
}

// Add text primitive
void RenderGroup::addText(QString text, Vec3<double> pos, double textSize, TextPrimitive::TextAnchor anchor, Vec3<double> globalAdjustment, bool flat)
{
	textPrimitives_.add(text, pos, textSize, anchor, globalAdjustment, flat);
}

// Add overlay text primitive
void RenderGroup::addOverlayText(QString text, Vec3<double> pos, double textSize, TextPrimitive::TextAnchor anchor, Vec3<double> globalAdjustment, bool flat)
{
	overlayTextPrimitives_.add(text, pos, textSize, anchor, globalAdjustment, flat);
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
	
	// Add occurrence to correct list
	if (fillMode == GL_FILL)
	{
		if (colour.w > 0.99) solidTrianglePrimitives_.addOccurrence(targetPrimitive, transform, colour);
		else transparentTrianglePrimitives_.addOccurrence(targetPrimitive, transform, colour);
	}
	else wireTrianglePrimitives_.addOccurrence(targetPrimitive, transform, colour);
}

// // Add triangle primitive
// void RenderGroup::addTriangles(Primitive& targetPrimitive, Matrix& transform, GLenum fillMode, GLfloat lineWidth)
// {
// 	// Check type of supplied primitive
// 	if (targetPrimitive.type() != GL_TRIANGLES)
// 	{
// 		printf("Warning: RenderGroup is rejecting primitive for triangle list, since it doesn't contain GL_TRIANGLES.\n");
// 		return;
// 	}
// 
// 	// Check if supplied primitive contains colour data already
// 	if (!targetPrimitive.colouredVertexData()) printf("Warning: Triangle primitive added to RenderGroup contains no colour data and has no colour supplied.\n");
// 	
// 	// Create new PrimitiveInfo in correct list
// 	PrimitiveInfo* pi = new PrimitiveInfo(targetPrimitive, transform, lineWidth);
// 	if (fillMode == GL_FILL) solidTrianglePrimitives_.addOccurrence(targetPrimitive, transform, colour
// 	else wireTrianglePrimitives_.own(pi);
// }

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
	if (bold) normalLinePrimitives_.addOccurrence(targetPrimitive, transform, colour);
	else boldLinePrimitives_.addOccurrence(targetPrimitive, transform, colour);
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
	if (bold) normalLinePrimitives_.addOccurrence(targetPrimitive, transform);
	else boldLinePrimitives_.addOccurrence(targetPrimitive, transform);
}

// Sort and render filtered polygons by depth
void RenderGroup::sendToGL(Matrix& modelTransformationMatrix)
{
	// Calculate inverse model matrix
	Matrix inverseMatrix = modelTransformationMatrix;
	inverseMatrix.removeTranslationAndScaling();
	inverseMatrix.invert();

	Matrix A;

	glEnable(GL_DEPTH_TEST);

	// Solid triangles
	glEnable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	solidTrianglePrimitives_.sendToGL(modelTransformationMatrix);

	// Extra solid triangles
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraSolidTriangles_.sendToGL(QOpenGLContext::currentContext());

	// Wire triangles
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glLineWidth(1.0f);
	wireTrianglePrimitives_.sendToGL(modelTransformationMatrix);

	// Extra wire triangles
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraWireTriangles_.sendToGL(QOpenGLContext::currentContext());

	// Normal lines
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glLineWidth(1.0f);
	normalLinePrimitives_.sendToGL(modelTransformationMatrix);

	// Extra normal lines
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraNormalLines_.sendToGL(QOpenGLContext::currentContext());

	// Bold lines
	glDisable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	glLineWidth(3.0f);
	boldLinePrimitives_.sendToGL(modelTransformationMatrix);

	// Extra bold lines
	glLoadMatrixd(modelTransformationMatrix.matrix());
	extraBoldLines_.sendToGL(QOpenGLContext::currentContext());

	// Transparent triangles
	glEnable(GL_LIGHTING);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glDepthMask(GL_FALSE);
	transparentTrianglePrimitives_.sendToGL(modelTransformationMatrix);
	glDepthMask(GL_TRUE);

	// Text
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_BLEND);
	GLfloat colour[4];
	prefs.copyColour(Prefs::ForegroundColour, colour);
	glColor4fv(colour);
	if (FontInstance::fontOK())
	{
		Matrix inverseMatrix = modelTransformationMatrix;
		inverseMatrix.removeTranslationAndScaling();
		inverseMatrix.invert();

		FontInstance::font()->FaceSize(1);
		textPrimitives_.renderAll(modelTransformationMatrix, inverseMatrix, 1.0);
	}

	/*
	 * Overlays
	 */

	glDisable(GL_DEPTH_TEST);

	// Overlay Lines
	glDisable(GL_LIGHTING);
	glLoadMatrixd(modelTransformationMatrix.matrix());
	overlayLines_.sendToGL(QOpenGLContext::currentContext());

	// Overlay Text
	glEnable(GL_MULTISAMPLE);
	glEnable(GL_BLEND);
	prefs.copyColour(Prefs::ForegroundColour, colour);
	glColor4fv(colour);
	if (FontInstance::fontOK())
	{
		FontInstance::font()->FaceSize(1);
		overlayTextPrimitives_.renderAll(modelTransformationMatrix, inverseMatrix, 1.0);
	}
}

