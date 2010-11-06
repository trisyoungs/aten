/*
	*** Polygon Class
	*** src/render/polygon.cpp
	Copyright T. Youngs 2007-2010

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

#include "render/polygon.h"

/*
// Polygon Vertex
*/

// Constructor
PolygonVertex::PolygonVertex()
{
	r[0] = 0.0;
	r[1] = 0.0;
	r[2] = 0.0;
	normal[0] = 0.0;
	normal[1] = 0.0;
	normal[2] = 0.0;
	colour[0] = 0.0;
	colour[1] = 0.0;
	colour[2] = 0.0;
	colour[3] = 0.0;
}

/*
// Polygon
*/

// Constructor
Polygon::Polygon()
{
	prev = NULL;
	next = NULL;
	nVertices = 0;
}
