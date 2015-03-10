/*
	*** Grid Primitive
	*** src/render/gridprimitive.h
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

#ifndef ATEN_GRIDPRIMITIVE_H
#define ATEN_GRIDPRIMITIVE_H

#include "render/primitive.h"
#include "render/textprimitive.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Grid;

// Grid Primitive
class GridPrimitive : public ListItem<GridPrimitive>
{
	public:
	// Constructor
	GridPrimitive(Grid* source = NULL);

	private:
	// Primitive containing generated porimary surface
	Primitive primaryPrimitive_;
	// Primitive containing generated secondary surface
	Primitive secondaryPrimitive_;
	// Grid from which primitive was created
	Grid* source_;
	// Whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool primaryIsTransparent_;
	// Whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool secondaryIsTransparent_;
	// Axes (line) primitives
	Primitive axisLinePrimitives_[3];
	// Axes (text) primitives
	List<TextPrimitive> axisTextPrimitives_[3];
	
	public:
	// Return primary primitive
	Primitive& primaryPrimitive();
	// Return secondary primitive
	Primitive& secondaryPrimitive();
	// Set source grid pointer
	void setSource(Grid* g);
	// Return source grid pointer
	Grid* source();
	// Return whether primary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool primaryIsTransparent();
	// Return whether secondary primitive contains any transparent triangles (and must be rendered through the chopper)
	bool secondaryIsTransparent();
	// Create 2D (heightmap-style) surface
	void createSurface2D();
	// Create 3D isosurface using Marching Cubes algorithm
	void createSurfaceMarchingCubes();
	// Create axes
	void createAxes();
	// Return axis line primitive specified
	Primitive& axisLinePrimitive(int axis);
	// Return axis text primitive list specified
	List<TextPrimitive>& axisTextPrimitives(int axis);
};

ATEN_END_NAMESPACE

#endif
