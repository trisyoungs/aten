/*
	*** Text Primitive List
	*** src/render/textprimitivelist.cpp
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

#include "render/textprimitivelist.h"

ATEN_USING_NAMESPACE

// Constructor
TextPrimitiveList::TextPrimitiveList()
{
}

// Clear list
void TextPrimitiveList::clear()
{
	textPrimitives_.clear();
}

// Set data from literal coordinates and text
void TextPrimitiveList::add(QString text, Vec3<double> anchorPoint, TextPrimitive::TextAnchor anchorPosition, Vec3<double> adjustmentVector, Matrix& rotation, double textSize)
{
	TextPrimitive* primitive = textPrimitives_.add();
	primitive->set(text, anchorPoint, anchorPosition, adjustmentVector, rotation, textSize);
}

// Update global bounding cuboid for all text primitives in the list
Cuboid TextPrimitiveList::boundingCuboid(ViewPane& pane, bool flatLabels, double baseFontSize, Cuboid startingCuboid)
{
	Cuboid result = startingCuboid;
	Matrix textMatrix;
	Vec3<double> corners[4], local;
	for (TextPrimitive* primitive = textPrimitives_.first(); primitive != NULL; primitive = primitive->next)
	{
		// Get transformation matrix and bounding box for text
		textMatrix = primitive->transformationMatrix(baseFontSize);
		primitive->boundingBox(corners[0], corners[1]);
		corners[2].set(corners[0].x, corners[1].y, 0.0);
		corners[3].set(corners[1].x, corners[0].y, 0.0);

		// Transform the four corners of the bounding box with the text primitive's transformation matrix
		// and determine the extreme x, y, and z coordinates of the primitives in the local frame
		for (int m=0; m<4; ++m)
		{
			local = textMatrix*corners[m];
			result.updateExtremes(local);
		}
	}

	return result;
}

// Render all primitives in list
void TextPrimitiveList::renderAll(Matrix viewMatrix, bool flatLabels, double baseFontSize)
{
	for (TextPrimitive* primitive = textPrimitives_.first(); primitive != NULL; primitive = primitive->next) primitive->render(viewMatrix, flatLabels, baseFontSize);
}

