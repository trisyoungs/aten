/*
	*** Text Primitive List
	*** src/render/textprimitivelist.cpp
	Copyright T. Youngs 2013-2018

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
void TextPrimitiveList::add(QString text, Vec3<double> anchorPoint, double textSize, TextPrimitive::TextAnchor anchorPosition, Vec3<double> globalAdjustment, bool flat)
{
	TextPrimitive* primitive = textPrimitives_.add();
	primitive->set(text, anchorPoint, textSize, anchorPosition, globalAdjustment, flat);
}

// Render all primitives in list
void TextPrimitiveList::renderAll(const Matrix& viewMatrix, const Matrix& viewMatrixInverse, double sizeScale, bool depthScaling)
{
	for (TextPrimitive* primitive = textPrimitives_.first(); primitive != NULL; primitive = primitive->next) primitive->render(viewMatrix, viewMatrixInverse, sizeScale, depthScaling);
}

