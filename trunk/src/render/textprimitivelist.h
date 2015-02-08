/*
	*** Text Primitive List
	*** src/render/textprimitivelist.h
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

#ifndef ATEN_TEXTPRIMITIVELIST_H
#define ATEN_TEXTPRIMITIVELIST_H

#include "render/textprimitive.h"
#include "math/matrix.h"
#include "math/cuboid.h"
#include "templates/vector3.h"
#include "templates/list.h"
#include <QtCore/QString>

// Forward Declarations
class ViewPane;

// Text Primitive List
class TextPrimitiveList
{
	public:
	// Constructor
	TextPrimitiveList();

	private:
	// List of text primitive chunks
	List<TextPrimitive> textPrimitives_;

	public:
	// Clear list
	void clear();
	// Add primitive to list
	void add(QString text, Vec3<double> anchorPoint, TextPrimitive::TextAnchor anchorPosition, Vec3<double> adjustmentVector, Matrix& rotation, double textSize);
	// Update global bounding cuboid for all text primitives in the list
	Cuboid boundingCuboid(ViewPane& pane, bool flatLabels, double baseFontSize, Cuboid startingCuboid = Cuboid());
	// Render all primitives in list
	void renderAll(Matrix viewMatrix, bool flatLabels, double baseFontSize);
};

#endif
