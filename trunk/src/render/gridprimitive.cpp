/*
	*** Grid Primitive
	*** src/render/gridprimitive.cpp
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

#include "render/gridprimitive.h"

// Constructor
GridPrimitive::GridPrimitive(Grid *source) : ListItem<GridPrimitive>()
{
	// Private variables
	source_ = source;
	primaryIsTransparent_ = FALSE;
	secondaryIsTransparent_ = FALSE;
	
	// Set both primitives to use no instances
	primaryPrimitive_.setNoInstances();
	secondaryPrimitive_.setNoInstances();
	for (int n=0; n<3; ++n)
	{
		axisLinePrimitives_[n].setType(GL_LINES);
		axisLinePrimitives_[n].setNoInstances();
	}
}

// Return primary primitive
Primitive &GridPrimitive::primaryPrimitive()
{
	return primaryPrimitive_;
}

// Return secondary primitive
Primitive &GridPrimitive::secondaryPrimitive()
{
	return secondaryPrimitive_;
}

// Set source grid pointer
void GridPrimitive::setSource(Grid *g)
{
	source_ = g;
}

// Return source grid pointer
Grid *GridPrimitive::source()
{
	return source_;
}

// Return whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
bool GridPrimitive::primaryIsTransparent()
{
	return primaryIsTransparent_;
}

// Return whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
bool GridPrimitive::secondaryIsTransparent()
{
	return secondaryIsTransparent_;
}

// Return axis line primitive specified
Primitive &GridPrimitive::axisLinePrimitive(int axis)
{
	return axisLinePrimitives_[axis];
}

// Return axis text primitive list specified
List<TextPrimitive3D> &GridPrimitive::axisTextPrimitives(int axis)
{
	return axisTextPrimitives_[axis];
}
