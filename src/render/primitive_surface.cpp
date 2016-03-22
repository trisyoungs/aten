/*
	*** Primitive - Surface Generation
	*** src/render/primitive_surface.cpp
	Copyright T. Youngs 2007-2016

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

#include "render/primitive.h"
#include "model/model.h"
#include "base/prefs.h"
#include "base/grid.h"
#include "base/wrapint.h"
#include "base/sysfunc.h"
#include "templates/array.h"

/*
	Marching Cube vertex and [edge] numbering

		7---------6			128------64
	       /|        /|		       /|        /|
	      / |       / |		      / |       / |
	     3---------2  |		     8---------4  |
	     |  |      |  |		     |  |      |  |
	     |  4------|--5		     |  16-----|-32
 y z	     | /       | /		     | /       | /
 |/	     |/        |/		     |/        |/
 o->x	     0---------1		     1---------2

     4---[4]---5	3     2     6     7		   7--[6]--6
 [8]/         /		|[3]  |[1]  |[5]  |[7]	      [10]/       /
   /         /[9]	|     |     |     |		 /       /[11]
  0---[0]---1		0     1     5     4		3--[2]--2
*/

// Marching Cube Edge Vertex Lookup Table
int edgevertices[12][2] = { { 0,1 }, { 1,2 }, { 2,3 }, { 0,3 },
	{ 4,5 }, { 5,6 }, { 6,7 }, { 4,7 },
	{ 0,4 }, { 1,5 }, { 3,7 }, { 2,6 } };

Vec3<double> vertexPos[8] = { Vec3<double>(0,0,0), Vec3<double>(1,0,0), Vec3<double>(1,1,0), Vec3<double>(0,1,0),
	Vec3<double>(0,0,1), Vec3<double>(1,0,1), Vec3<double>(1,1,1), Vec3<double>(0,1,1) };

// Mappings for cube vertices (shown above) onto slice data (0,3,4,7 = back slice, 1,2,5,6 = front slice
Vec3<int> masterEdgeData[12][2] = {
			{ Vec3<int>(0, 0, 0), Vec3<int>(1, 0, 0 ) },	// = 0,1
			{ Vec3<int>(1, 0, 0), Vec3<int>(1, 1, 0 ) },	// = 1,2
			{ Vec3<int>(1, 1, 0), Vec3<int>(0, 1, 0 ) },	// = 2,3
			{ Vec3<int>(0, 0, 0), Vec3<int>(0, 1, 0 ) },	// = 0,3
			{ Vec3<int>(0, 0, 1), Vec3<int>(1, 0, 1 ) },	// = 4,5
			{ Vec3<int>(1, 0, 1), Vec3<int>(1, 1, 1 ) },	// = 5,6
			{ Vec3<int>(1, 1, 1), Vec3<int>(0, 1, 1 ) },	// = 6,7
			{ Vec3<int>(0, 0, 1), Vec3<int>(0, 1, 1 ) },	// = 4,7
			{ Vec3<int>(0, 0, 0), Vec3<int>(0, 0, 1 ) },	// = 0,4
			{ Vec3<int>(1, 0, 0), Vec3<int>(1, 0, 1 ) },	// = 1,5
			{ Vec3<int>(0, 1, 0), Vec3<int>(0, 1, 1 ) },	// = 3,7
			{ Vec3<int>(1, 1, 0), Vec3<int>(1, 1, 1 ) }	// = 2,6
			};

// double vertexpos[8][3] = { {1.0,0.0,0.0}, {0.0,1.0,0.0}, {-1.0,0.0,0.0}, {0.0,-1.0,0.0},
// 	{1.0,0.0,0.0}, {0.0,1.0,0.0}, {-1.0,0.0,0.0}, {0.0,-1.0,0.0},
// 	{0.0,0.0,1.0}, {0.0,0.0,1.0}, {0.0,0.0,1.0}, {0.0,0.0,1.0} };

// double vertexPos[8][3] = { {0,0,0}, {1,0,0}, {1,1,0}, {0,1,0},
// 	{0,0,1}, {1,0,1}, {1,1,1}, {0,1,1} };

// Marching Cube Face Triplet Lookup Table
int facetriples[256][15] = {
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 1, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 8, 3, 1, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 9, 2, 11, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 2, 8, 3, 2, 11, 8, 11, 9, 8, -1, -1, -1, -1, -1, -1},
	{ 3, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 10, 2, 8, 10, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 1, 9, 0, 2, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 1, 10, 2, 1, 9, 10, 9, 8, 10, -1, -1, -1, -1, -1, -1},
	{ 3, 11, 1, 10, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 11, 1, 0, 8, 11, 8, 10, 11, -1, -1, -1, -1, -1, -1},
	{ 3, 9, 0, 3, 10, 9, 10, 11, 9, -1, -1, -1, -1, -1, -1},
	{ 9, 8, 11, 11, 8, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 11, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 4, 7, 3, 0, 4, 1, 2, 11, -1, -1, -1, -1, -1, -1}, 
	{ 9, 2, 11, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1}, 
	{ 2, 11, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1}, 
	{ 8, 4, 7, 3, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 10, 4, 7, 10, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1}, 
	{ 9, 0, 1, 8, 4, 7, 2, 3, 10, -1, -1, -1, -1, -1, -1}, 
	{ 4, 7, 10, 9, 4, 10, 9, 10, 2, 9, 2, 1, -1, -1, -1}, 
	{ 3, 11, 1, 3, 10, 11, 7, 8, 4, -1, -1, -1, -1, -1, -1}, 
	{ 1, 10, 11, 1, 4, 10, 1, 0, 4, 7, 10, 4, -1, -1, -1}, 
	{ 4, 7, 8, 9, 0, 10, 9, 10, 11, 10, 0, 3, -1, -1, -1}, 
	{ 4, 7, 10, 4, 10, 9, 9, 10, 11, -1, -1, -1, -1, -1, -1}, 
	{ 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 11, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 0, 8, 1, 2, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1}, 
	{ 5, 2, 11, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1}, 
	{ 2, 11, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1}, 
	{ 9, 5, 4, 2, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1},
	{ 0, 10, 2, 0, 8, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1}, 
	{ 0, 5, 4, 0, 1, 5, 2, 3, 10, -1, -1, -1, -1, -1, -1}, 
	{ 2, 1, 5, 2, 5, 8, 2, 8, 10, 4, 8, 5, -1, -1, -1}, 
	{ 11, 3, 10, 11, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1}, 
	{ 4, 9, 5, 0, 8, 1, 8, 11, 1, 8, 10, 11, -1, -1, -1}, 
	{ 5, 4, 0, 5, 0, 10, 5, 10, 11, 10, 0, 3, -1, -1, -1}, 
	{ 5, 4, 8, 5, 8, 11, 11, 8, 10, -1, -1, -1, -1, -1, -1}, 
	{ 9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1}, 
	{ 0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1}, 
	{ 1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 7, 8, 9, 5, 7, 11, 1, 2, -1, -1, -1, -1, -1, -1}, 
	{ 11, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1}, 
	{ 8, 0, 2, 8, 2, 5, 8, 5, 7, 11, 5, 2, -1, -1, -1}, 
	{ 2, 11, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1}, 
	{ 7, 9, 5, 7, 8, 9, 3, 10, 2, -1, -1, -1, -1, -1, -1}, 
	{ 9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 10, -1, -1, -1}, 
	{ 2, 3, 10, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1}, 
	{ 10, 2, 1, 10, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1}, 
	{ 9, 5, 8, 8, 5, 7, 11, 1, 3, 11, 3, 10, -1, -1, -1}, 
	{ 5, 7, 10, 5, 10, 11, 1, 0, 9, -1, -1, -1, -1, -1, -1}, 
	{ 10, 11, 5, 10, 5, 7, 8, 0, 3, -1, -1, -1, -1, -1, -1}, 
	{ 10, 11, 5, 7, 10, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 11, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 8, 3, 5, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 0, 1, 5, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 8, 3, 1, 9, 8, 5, 11, 6, -1, -1, -1, -1, -1, -1}, 
	{ 1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1}, 
	{ 9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1}, 
	{ 5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1}, 
	{ 2, 3, 10, 11, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 10, 0, 8, 10, 2, 0, 11, 6, 5, -1, -1, -1, -1, -1, -1}, 
	{ 0, 1, 9, 2, 3, 10, 5, 11, 6, -1, -1, -1, -1, -1, -1}, 
	{ 5, 11, 6, 1, 9, 2, 9, 10, 2, 9, 8, 10, -1, -1, -1}, 
	{ 6, 3, 10, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1}, 
	{ 0, 8, 10, 0, 10, 5, 0, 5, 1, 5, 10, 6, -1, -1, -1}, 
	{ 3, 10, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1}, 
	{ 6, 5, 9, 6, 9, 10, 10, 9, 8, -1, -1, -1, -1, -1, -1}, 
	{ 5, 11, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 3, 0, 4, 7, 3, 6, 5, 11, -1, -1, -1, -1, -1, -1}, 
	{ 1, 9, 0, 5, 11, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1}, 
	{ 11, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1}, 
	{ 6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1}, 
	{ 8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1}, 
	{ 7, 3, 2, 7, 2, 6, 5, 9, 4, -1, -1, -1, -1, -1, -1}, 
	{ 3, 10, 2, 7, 8, 4, 11, 6, 5, -1, -1, -1, -1, -1, -1}, 
	{ 5, 11, 6, 4, 7, 2, 4, 2, 0, 2, 7, 10, -1, -1, -1}, 
	{ 0, 1, 9, 4, 7, 8, 2, 3, 10, 5, 11, 6, -1, -1, -1}, 
	{ 9, 4, 5, 11, 2, 1, 7, 10, 6, -1, -1, -1, -1, -1, -1}, 
	{ 8, 4, 7, 3, 10, 5, 3, 5, 1, 5, 10, 6, -1, -1, -1}, 
	{ 5, 1, 0, 5, 0, 4, 7, 10, 6, -1, -1, -1, -1, -1, -1}, 
	{ 0, 3, 8, 4, 5, 9, 10, 6, 7, -1, -1, -1, -1, -1, -1}, 
	{ 4, 5, 9, 7, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 11, 4, 9, 6, 4, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 11, 6, 4, 9, 11, 0, 8, 3, -1, -1, -1, -1, -1, -1}, 
	{ 11, 0, 1, 11, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1}, 
	{ 8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 11, -1, -1, -1}, 
	{ 1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1}, 
	{ 3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1}, 
	{ 0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1}, 
	{ 11, 4, 9, 11, 6, 4, 10, 2, 3, -1, -1, -1, -1, -1, -1}, 
	{ 0, 8, 2, 2, 8, 10, 4, 9, 11, 4, 11, 6, -1, -1, -1}, 
	{ 3, 10, 2, 0, 1, 6, 0, 6, 4, 6, 1, 11, -1, -1, -1}, 
	{ 6, 4, 8, 6, 8, 10, 2, 1, 11, -1, -1, -1, -1, -1, -1}, 
	{ 9, 6, 4, 9, 3, 6, 9, 1, 3, 10, 6, 3, -1, -1, -1}, 
	{ 8, 10, 6, 8, 6, 4, 9, 1, 0, -1, -1, -1, -1, -1, -1}, 
	{ 3, 10, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1}, 
	{ 6, 4, 8, 10, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 7, 11, 6, 7, 8, 11, 8, 9, 11, -1, -1, -1, -1, -1, -1}, 
	{ 0, 7, 3, 0, 11, 7, 0, 9, 11, 6, 7, 11, -1, -1, -1}, 
	{ 11, 6, 7, 1, 11, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1}, 
	{ 11, 6, 7, 11, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1}, 
	{ 2, 6, 7, 2, 7, 3, 0, 9, 1, -1, -1, -1, -1, -1, -1}, 
	{ 7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1}, 
	{ 7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 2, 3, 10, 11, 6, 8, 11, 8, 9, 8, 6, 7, -1, -1, -1}, 
	{ 2, 0, 9, 2, 9, 11, 6, 7, 10, -1, -1, -1, -1, -1, -1}, 
	{ 1, 11, 2, 3, 8, 0, 6, 7, 10, -1, -1, -1, -1, -1, -1}, 
	{ 11, 2, 1, 6, 7, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 8, 9, 1, 8, 1, 3, 10, 6, 7, -1, -1, -1, -1, -1, -1}, 
	{ 0, 9, 1, 10, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 8, 0, 10, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 7, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 7, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 0, 8, 10, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 1, 9, 10, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 8, 1, 9, 8, 3, 1, 10, 7, 6, -1, -1, -1, -1, -1, -1}, 
	{ 11, 1, 2, 6, 10, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 11, 3, 0, 8, 6, 10, 7, -1, -1, -1, -1, -1, -1}, 
	{ 2, 9, 0, 2, 11, 9, 6, 10, 7, -1, -1, -1, -1, -1, -1}, 
	{ 2, 10, 3, 11, 8, 6, 11, 9, 8, 8, 7, 6, -1, -1, -1}, 
	{ 7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1}, 
	{ 2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1}, 
	{ 1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1}, 
	{ 11, 7, 6, 11, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1}, 
	{ 11, 7, 6, 1, 7, 11, 1, 8, 7, 1, 0, 8, -1, -1, -1}, 
	{ 0, 3, 7, 0, 7, 11, 0, 11, 9, 6, 11, 7, -1, -1, -1}, 
	{ 7, 6, 11, 7, 11, 8, 8, 11, 9, -1, -1, -1, -1, -1, -1}, 
	{ 6, 8, 4, 10, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 6, 10, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1}, 
	{ 8, 6, 10, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 4, 6, 9, 6, 3, 9, 3, 1, 10, 3, 6, -1, -1, -1}, 
	{ 6, 8, 4, 6, 10, 8, 2, 11, 1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 2, 10, 0, 6, 1, 0, 4, 6, 6, 11, 1, -1, -1, -1}, 
	{ 0, 2, 8, 2, 10, 8, 4, 11, 9, 4, 6, 11, -1, -1, -1}, 
	{ 11, 9, 4, 11, 4, 6, 10, 3, 2, -1, -1, -1, -1, -1, -1}, 
	{ 8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1}, 
	{ 0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 8, 0, 1, 9, 2, 2, 9, 4, 2, 4, 6, -1, -1, -1}, 
	{ 1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1}, 
	{ 8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 11, 1, -1, -1, -1}, 
	{ 11, 1, 0, 11, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1}, 
	{ 4, 6, 11, 4, 11, 9, 0, 3, 8, -1, -1, -1, -1, -1, -1}, 
	{ 11, 9, 4, 6, 11, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 9, 5, 7, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 8, 3, 4, 9, 5, 10, 7, 6, -1, -1, -1, -1, -1, -1}, 
	{ 5, 0, 1, 5, 4, 0, 7, 6, 10, -1, -1, -1, -1, -1, -1}, 
	{ 8, 7, 4, 3, 5, 10, 3, 1, 5, 5, 6, 10, -1, -1, -1}, 
	{ 9, 5, 4, 11, 1, 2, 7, 6, 10, -1, -1, -1, -1, -1, -1}, 
	{ 0, 9, 1, 4, 8, 7, 2, 10, 3, 5, 6, 11, -1, -1, -1}, 
	{ 5, 6, 11, 4, 2, 7, 4, 0, 2, 2, 10, 7, -1, -1, -1}, 
	{ 3, 2, 10, 7, 4, 8, 11, 5, 6, -1, -1, -1, -1, -1, -1}, 
	{ 7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1}, 
	{ 8, 7, 4, 9, 5, 0, 0, 5, 6, 0, 6, 2, -1, -1, -1}, 
	{ 1, 5, 2, 5, 6, 2, 3, 4, 0, 3, 7, 4, -1, -1, -1}, 
	{ 6, 2, 1, 6, 1, 5, 4, 8, 7, -1, -1, -1, -1, -1, -1}, 
	{ 11, 5, 6, 1, 7, 9, 1, 3, 7, 7, 4, 9, -1, -1, -1}, 
	{ 1, 0, 9, 5, 6, 11, 8, 7, 4, -1, -1, -1, -1, -1, -1}, 
	{ 4, 0, 3, 4, 3, 7, 6, 11, 5, -1, -1, -1, -1, -1, -1}, 
	{ 5, 6, 11, 4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 6, 9, 5, 6, 10, 9, 10, 8, 9, -1, -1, -1, -1, -1, -1}, 
	{ 3, 6, 10, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1}, 
	{ 0, 10, 8, 0, 5, 10, 0, 1, 5, 5, 6, 10, -1, -1, -1}, 
	{ 6, 10, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1}, 
	{ 5, 6, 11, 1, 2, 9, 9, 2, 10, 9, 10, 8, -1, -1, -1}, 
	{ 0, 9, 1, 2, 10, 3, 5, 6, 11, -1, -1, -1, -1, -1, -1}, 
	{ 10, 8, 0, 10, 0, 2, 11, 5, 6, -1, -1, -1, -1, -1, -1}, 
	{ 2, 10, 3, 11, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1}, 
	{ 9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1}, 
	{ 1, 5, 6, 1, 6, 2, 3, 8, 0, -1, -1, -1, -1, -1, -1}, 
	{ 1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 3, 8, 1, 8, 9, 5, 6, 11, -1, -1, -1, -1, -1, -1}, 
	{ 9, 1, 0, 5, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 3, 8, 5, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 11, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 10, 5, 11, 7, 5, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 10, 5, 11, 10, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1}, 
	{ 5, 10, 7, 5, 11, 10, 1, 9, 0, -1, -1, -1, -1, -1, -1}, 
	{ 9, 8, 5, 8, 7, 5, 11, 3, 1, 11, 10, 3, -1, -1, -1}, 
	{ 10, 1, 2, 10, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1}, 
	{ 2, 10, 3, 0, 8, 1, 1, 8, 7, 1, 7, 5, -1, -1, -1}, 
	{ 9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 10, 7, -1, -1, -1}, 
	{ 7, 5, 9, 7, 9, 8, 3, 2, 10, -1, -1, -1, -1, -1, -1}, 
	{ 2, 5, 11, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1}, 
	{ 8, 2, 0, 8, 5, 2, 8, 7, 5, 11, 2, 5, -1, -1, -1}, 
	{ 11, 2, 1, 9, 0, 5, 5, 0, 3, 5, 3, 7, -1, -1, -1}, 
	{ 9, 8, 7, 9, 7, 5, 11, 2, 1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1}, 
	{ 9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1}, 
	{ 9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 5, 8, 4, 5, 11, 8, 11, 10, 8, -1, -1, -1, -1, -1, -1}, 
	{ 5, 0, 4, 5, 10, 0, 5, 11, 10, 10, 3, 0, -1, -1, -1}, 
	{ 4, 5, 9, 0, 1, 8, 8, 1, 11, 8, 11, 10, -1, -1, -1}, 
	{ 11, 10, 3, 11, 3, 1, 9, 4, 5, -1, -1, -1, -1, -1, -1}, 
	{ 2, 5, 1, 2, 8, 5, 2, 10, 8, 4, 5, 8, -1, -1, -1}, 
	{ 0, 4, 5, 0, 5, 1, 2, 10, 3, -1, -1, -1, -1, -1, -1}, 
	{ 0, 2, 10, 0, 10, 8, 4, 5, 9, -1, -1, -1, -1, -1, -1}, 
	{ 9, 4, 5, 2, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 2, 5, 11, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1}, 
	{ 5, 11, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1}, 
	{ 3, 8, 0, 1, 11, 2, 4, 5, 9, -1, -1, -1, -1, -1, -1}, 
	{ 1, 11, 2, 9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 4, 5, 0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 10, 7, 4, 9, 10, 9, 11, 10, -1, -1, -1, -1, -1, -1}, 
	{ 4, 8, 7, 9, 10, 0, 9, 11, 10, 10, 3, 0, -1, -1, -1}, 
	{ 1, 11, 10, 1, 10, 4, 1, 4, 0, 7, 4, 10, -1, -1, -1}, 
	{ 3, 1, 11, 3, 11, 10, 7, 4, 8, -1, -1, -1, -1, -1, -1}, 
	{ 4, 10, 7, 9, 10, 4, 9, 2, 10, 9, 1, 2, -1, -1, -1}, 
	{ 9, 1, 0, 8, 7, 4, 2, 10, 3, -1, -1, -1, -1, -1, -1}, 
	{ 10, 7, 4, 10, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1}, 
	{ 8, 7, 4, 3, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 2, 9, 11, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1}, 
	{ 9, 11, 2, 9, 2, 0, 8, 7, 4, -1, -1, -1, -1, -1, -1}, 
	{ 3, 7, 4, 3, 4, 0, 1, 11, 2, -1, -1, -1, -1, -1, -1}, 
	{ 1, 11, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1}, 
	{ 0, 9, 1, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 9, 11, 8, 11, 10, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 0, 9, 3, 9, 10, 10, 9, 11, -1, -1, -1, -1, -1, -1}, 
	{ 0, 1, 11, 0, 11, 8, 8, 11, 10, -1, -1, -1, -1, -1, -1}, 
	{ 3, 1, 11, 10, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 2, 10, 1, 10, 9, 9, 10, 8, -1, -1, -1, -1, -1, -1}, 
	{ 1, 0, 9, 2, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 2, 10, 8, 0, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 3, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 2, 3, 8, 2, 8, 11, 11, 8, 9, -1, -1, -1, -1, -1, -1}, 
	{ 9, 11, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 3, 8, 1, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}, 
	{ 0, 3, 8, 11, 1, 2, 6, 10, 7, 4, 9, 5, -1, -1, -1 }
// 	{ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
};

ATEN_USING_NAMESPACE

// Plot single marching cube
void Primitive::plotEdge(const int cubeType, const double lowerCutoff, const double valueA, const double valueB, const Vec3<double> gradientA, const Vec3<double> gradientB, const Vec3<double> cubeLLC, const int edgeIndexA, const int edgeIndexB, const int colourScale)
{
	Vec3<double> normal;
	double ipol;
	Vec3<double> r;
	Vec4<GLfloat> colour;
	int* faces;
	faces = facetriples[cubeType];

	// Get edge vectors, interpolate, and set tri-points
	ipol = ((lowerCutoff) - valueA) / (valueB-valueA);
	if (ipol > 1.0) ipol = 1.0;
	if (ipol < 0.0) ipol = 0.0;

	// Grab cube vertex coordinates (using existing edgevertices array to map onto vertexpos array)
	r = (vertexPos[edgeIndexB] - vertexPos[edgeIndexA]) * ipol + vertexPos[edgeIndexA] + cubeLLC;
	normal = -(gradientA + (gradientB - gradientA) * ipol);
	normal.normalise();

	// Set triangle coordinates and add cube position
	if (colourScale != -1)
	{
		prefs.colourScale[colourScale].colour((valueA+valueB)/2.0, colour);
		defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z, colour);
	}
	else defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z);
}

// Render volumetric isosurface with Marching Cubes
void Primitive::marchingCubes(Grid* source, double lowerCutoff, double upperCutoff, int colourScale)
{
	int outer, ii, jj, kk, n, cubeType, *faces, xx, yy, zz, front, back;
	int outerStart, outerLimit, outerDelta, jjStart, jjLimit, jjDelta, kkStart, kkLimit, kkDelta;
	int xxStart, xxLimit, xxDelta, yyStart, yyLimit, yyDelta, zzStart, zzLimit, zzDelta;	
	Vec3<double> r, cubeLLC;
	Vec3<int> nPoints = source->nXYZ(), shift = source->shift(), indexMapped, shiftIJK, newSliceSize;
	WrapInt indexLocal[3];
	double*** data, twodx, twody, twodz, value;
	data = source->data3d();
	bool periodic = source->periodic();
	bool fillVolume = source->fillEnclosedVolume();
	// 3D slice region for necessary data, and last initialised sizes
	static Vec3<double>*** gradients = NULL;
	static double*** values = NULL;
	static bool*** flags = NULL;
	static Vec3<int> sliceSize;
	bool frontSwap;

	// Get distances between grid points
	r = source->lengths();
	twodx = r.x / nPoints.x * 2.0;
	twody = r.y / nPoints.y * 2.0;
	twodz = r.z / nPoints.z * 2.0;

	// Initialise primitive
	initialise(GL_TRIANGLES, colourScale != -1);

	// ii, jj, and kk are cube index variables, not value index variables.
	// 'ii' represents our principal direction, along which we are taking slices of the data in order to generate the data

	// Set up our directions array
	//  dirs[0] == main axis for traversing data (principal loop variable 'ii')
	//  dirs[1] == first axis for slice data
	//  dirs[2] == second axis for slice data

	// Determine which axis/axes overlap most with cardinal Z (i.e. perpendicular to view)
	Vec3<int> dirs = Vec3<int>(-1, -1, -1);
	Vec3<int> signs = Vec3<int>(1, 1, 1);
	
	// Grab components of the view matrix column vectors in sequential directions/rows, finding the largest component each time
	for (n=0; n<3; ++n)
	{
		// Grab components of the view matrix column vectors in the (2-n) direction/row
		Vec3<double> components = source->parent()->modelViewMatrix().rowAsVec3(2-n);

		// Find next largest component (in a vector direction we haven't used yet...)
		int maxEl = -1;
		double maxVal = -1.0;
		for (ii=0; ii<3; ++ii)
		{
			// For direction 'ii', check that it has not yet been entered into the dirs array
			for (jj=0; jj<n; ++jj) if (dirs[jj] == ii) break;
			if (jj < n) continue;

			// Check absolute value of this element against our stored value
			if (abs(components[ii]) > maxVal)
			{
				maxVal = abs(components[ii]);
				maxEl = ii;
			}
		}

		dirs[n] = maxEl;
		signs[n] = components[dirs[n]] < 0 ? -1 : 1;
	}
	// Reorder last two loops so that we are always ordered in x-y-z precedence
	if (dirs.z < dirs.y)
	{
		ii = dirs.z;
		dirs.z = dirs.y;
		dirs.y = ii;
		ii = signs.z;
		signs.z = signs.y;
		signs.y = ii;
	}

// 	printf("Axes order is : "); dirs.print();
// 	printf("Axes signs are : "); signs.print();

	// Set up the wrapped integers for safe access to the data[][][] array
	indexLocal[0].setLimits(0, nPoints.x-1);
	indexLocal[1].setLimits(0, nPoints.y-1);
	indexLocal[2].setLimits(0, nPoints.z-1);

	// Grab shift values in the new dir order for convenience
	shiftIJK.x = shift[dirs.x];
	shiftIJK.y = shift[dirs.y];
	shiftIJK.z = shift[dirs.z];

	// Set start values and limits for ii, jj, and kk control variables
	if (periodic)
	{
		outerStart = (signs.x == 1 ? 0 : nPoints[dirs.x]);
		outerLimit = (signs.x == 1 ? nPoints[dirs.x]+1 : -1);
		outerDelta = signs.x;
		jjStart = 0;
		jjLimit = nPoints[dirs.y]+1;
		jjDelta = 1;
		kkStart = 0;
		kkLimit = nPoints[dirs.z]+1;
		kkDelta = 1;
	}
	else
	{
		outerStart = (signs.x == 1 ? 1 : nPoints[dirs.x]-2);
		outerLimit = (signs.x == 1 ? nPoints[dirs.x]-1 : 0);
		outerDelta = signs.x;
		jjStart = 1;
		jjLimit = nPoints[dirs.y]-1;
		jjDelta = 1;
		kkStart = 1;
		kkLimit = nPoints[dirs.z]-1;
		kkDelta = 1;
	}

	// Work out nx and ny from the loop limits we have just set.
	// When constructing cubes, we will always use loop ranges 1, (sliceNX-1)
	newSliceSize[dirs.x] = 2;
	newSliceSize[dirs.y] = jjLimit - jjStart;
	newSliceSize[dirs.z] = kkLimit - kkStart;

	// (Re)initialise arrays
	if ((gradients == NULL) || (newSliceSize.x != sliceSize.x) || (newSliceSize.y != sliceSize.y) || (newSliceSize.z != sliceSize.z))
	{
		// Reallocate arrays if necessary
		if (gradients != NULL)
		{
			for (ii = 0; ii < sliceSize.x; ++ii)
			{
				for (jj = 0; jj < sliceSize.y; ++jj)
				{
					delete[] gradients[ii][jj];
					delete[] values[ii][jj];
					delete[] flags[ii][jj];
				}
				delete[] gradients[ii];
				delete[] values[ii];
				delete[] flags[ii];
			}
			delete[] gradients;
			delete[] values;
			delete[] flags;
		}

		// New arrays
		sliceSize = newSliceSize;
		gradients = new Vec3<double>**[sliceSize.x];
		values = new double**[sliceSize.x];
		flags = new bool**[sliceSize.x];
		for (ii = 0; ii < sliceSize.x; ++ii)
		{
			gradients[ii] = new Vec3<double>*[sliceSize.y];
			values[ii] = new double*[sliceSize.y];
			flags[ii] = new bool*[sliceSize.y];
			for (jj = 0; jj < sliceSize.y; ++jj)
			{
				gradients[ii][jj] = new Vec3<double>[sliceSize.z];
				values[ii][jj] = new double[sliceSize.z];
				flags[ii][jj] = new bool[sliceSize.z];
			}
		}
	}

// 	printf("Slice Size = "); sliceSize.print();

	// Make a copy of the masterEdgeData so that we can modify it to reflect back/front switches
	Vec3<int> edgeData[12][2];
	for (ii = 0; ii < 12; ++ii)
	{
		edgeData[ii][0] = masterEdgeData[ii][0];
		edgeData[ii][1] = masterEdgeData[ii][1];
	}

	cubeLLC[dirs.x] = outerStart - (signs.x == 1 ? 1 : 0);
	indexLocal[dirs.x] = (outerStart - shiftIJK.x);
	frontSwap = true;
	for (outer = outerStart; outer != outerLimit; outer += outerDelta, indexLocal[dirs.x] += outerDelta, cubeLLC[dirs.x] += outerDelta)
	{
		// Get swap variables, and switch back/front indices for our principal direction
		frontSwap = !frontSwap;
		front = (frontSwap ? 0 : 1);
		back = (frontSwap ? 1 : 0);
		indexMapped[dirs.x] = (signs.x == 1 ? front : back);
		for (ii = 0; ii <12; ++ii)
		{
			edgeData[ii][0][dirs.x] = frontSwap ? (masterEdgeData[ii][0][dirs.x] == 1 ? 0 : 1) : masterEdgeData[ii][0][dirs.x];
			edgeData[ii][1][dirs.x] = frontSwap ? (masterEdgeData[ii][1][dirs.x] == 1 ? 0 : 1) : masterEdgeData[ii][1][dirs.x];
		}

		// Calculate gradients, validity, and store values in the current 'front' arrays
		// We will construct the slice using standard 0-N loops. Negative directions in jj and kk will be accounted for in the cube generation loops
		indexMapped[dirs.y] = 0;
		indexLocal[dirs.y] = jjStart-shiftIJK.y;
		for (jj=jjStart; jj != jjLimit; jj += jjDelta, indexMapped[dirs.y] += jjDelta, indexLocal[dirs.y] += jjDelta)
		{
			indexMapped[dirs.z] = 0;
			indexLocal[dirs.z] = kkStart-shiftIJK.z;

			for (kk=kkStart; kk != kkLimit; kk+=kkDelta, indexMapped[dirs.z] += kkDelta, indexLocal[dirs.z] += kkDelta)
			{
				// Calculate gradient
// 				printf("Setting data for %i %i %i (local = %i %i %i)\n", indexMapped.x, indexMapped.y, indexMapped.z, indexLocal[0].value(), indexLocal[1].value(), indexLocal[2].value());
				gradients[indexMapped.x][indexMapped.y][indexMapped.z].x = (data[indexLocal[0]+1][indexLocal[1]][indexLocal[2]] - data[indexLocal[0]-1][indexLocal[1]][indexLocal[2]]) / twodx;
				gradients[indexMapped.x][indexMapped.y][indexMapped.z].y = (data[indexLocal[0]][indexLocal[1]+1][indexLocal[2]] - data[indexLocal[0]][indexLocal[1]-1][indexLocal[2]]) / twody;
				gradients[indexMapped.x][indexMapped.y][indexMapped.z].z = (data[indexLocal[0]][indexLocal[1]][indexLocal[2]+1] - data[indexLocal[0]][indexLocal[1]][indexLocal[2]-1]) / twodz;
				// Grab value and test against cutoffs
				value = data[indexLocal[0]][indexLocal[1]][indexLocal[2]];
				values[indexMapped.x][indexMapped.y][indexMapped.z] = value;
				flags[indexMapped.x][indexMapped.y][indexMapped.z] = ((value >= lowerCutoff) && (value <= upperCutoff));
// 				printf("Gradient = %f %f %f at %i %i %i, local = %i %i %i\n", gradients[indexMapped.x][indexMapped.y][indexMapped.z].x, gradients[indexMapped.x][indexMapped.y][indexMapped.z].y, gradients[indexMapped.x][indexMapped.y][indexMapped.z].z, indexMapped.x, indexMapped.y, indexMapped.z, indexLocal[0].value(), indexLocal[1].value(), indexLocal[2].value());
			}
		}

		// Loop again to generate second set of values if outer = outerStart
		if (outer == outerStart) continue;

// 			   7---------6			128------64
// 			  /|        /|		       /|        /|
// 			 / |       / |		      / |       / |
// 			3---------2  |		     8---------4  |
// 			|  |      |  |		     |  |      |  |
// 			|  4------|--5		     |  16-----|-32
// 		y z	| /       | /		     | /       | /
// 		|/	|/        |/		     |/        |/
// 		o->x	0---------1		     1---------2
	
		// Create cube vertex information based on the gradients, values, and flags for our sliced data
		// We'll use tailored loops to avoid having a wasteful single-valued loop somewhere, and to keep value / vertex probing simpler
		// After selecting the loop based on the principal direction, we must still refer to the second and third loop variables as 'y' and 'z'
		// when using the dirs and signs variables, and when setting cubeLLC elements. Other than that, the loops operate in local (i.e. data)
		// x, y, z space, using xx, yy and zz variables as appropriate.
		if (dirs.x == 0)
		{
			// Principal direction is X : slice is in YZ
			cubeLLC[dirs.y] = (signs.y == 1 ? jjStart : jjLimit-2);

			yyStart = (signs.y == 1 ? 0 : sliceSize[dirs.y] - 2);
			yyLimit = (signs.y == 1 ? sliceSize[dirs.y] - 1 : -1);
			yyDelta = signs.y;	
			zzStart = (signs.z == 1 ? 0 : sliceSize[dirs.z] - 2);
			zzLimit = (signs.z == 1 ? sliceSize[dirs.z] - 1 : -1);
			zzDelta = signs.z;

			for (yy=yyStart; yy != yyLimit; yy += yyDelta, cubeLLC[dirs.y] += yyDelta)
			{
				cubeLLC[dirs.z] = (signs.z == 1 ? kkStart : kkLimit-2);
				for (zz = zzStart; zz != zzLimit; zz += zzDelta, cubeLLC[dirs.z] += zzDelta)
				{
					// Determine cube type
					cubeType = 0;
					if (flags[back][yy][zz]) cubeType += 1;
					if (flags[front][yy][zz]) cubeType += 2;
					if (flags[front][yy+1][zz]) cubeType += 4;
					if (flags[back][yy+1][zz]) cubeType += 8;
					if (flags[back][yy][zz+1]) cubeType += 16;
					if (flags[front][yy][zz+1]) cubeType += 32;
					if (flags[front][yy+1][zz+1]) cubeType += 64;
					if (flags[back][yy+1][zz+1]) cubeType += 128;
// 					printf("XXX Cube # %i %i, LLC=%f %f %f, type = %i\n", yy, zz, cubeLLC.x, cubeLLC.y, cubeLLC.z, cubeType);

					// Quick check for 'whole cube'
					if (cubeType == 255) { if (fillVolume) plotCube(1.0, 1, cubeLLC.x, cubeLLC.y, cubeLLC.z); }
					else
					{
						// Get edges from list and draw triangles or points
						faces = facetriples[cubeType];
						for (n = 0; n<15; ++n)
						{
							if (faces[n] == -1) break;
							plotEdge(cubeType, lowerCutoff,
								values[ edgeData[faces[n]][0].x ][ edgeData[faces[n]][0].y+yy ][ edgeData[faces[n]][0].z+zz ],
								values[ edgeData[faces[n]][1].x ][ edgeData[faces[n]][1].y+yy ][ edgeData[faces[n]][1].z+zz ],
								gradients[ edgeData[faces[n]][0].x ][ edgeData[faces[n]][0].y+yy ][ edgeData[faces[n]][0].z+zz ],
								gradients[ edgeData[faces[n]][1].x ][ edgeData[faces[n]][1].y+yy ][ edgeData[faces[n]][1].z+zz ],
								cubeLLC, edgevertices[faces[n]][0], edgevertices[faces[n]][1], colourScale);
						}
					}
				}
			}
		}
		else if (dirs.x == 1)
		{
			// Principal direction is Y : slice is in XZ
			cubeLLC[dirs.y] = (signs.y == 1 ? jjStart : jjLimit-2);

			xxStart = (signs.y == 1 ? 0 : sliceSize[dirs.y] - 2);
			xxLimit = (signs.y == 1 ? sliceSize[dirs.y] - 1 : -1);
			xxDelta = signs.y;
			zzStart = (signs.z == 1 ? 0 : sliceSize[dirs.z] - 2);
			zzLimit = (signs.z == 1 ? sliceSize[dirs.z] - 1 : -1);
			zzDelta = signs.z;

			for (xx=xxStart; xx != xxLimit; xx += xxDelta, cubeLLC[dirs.y] += xxDelta)
			{
				cubeLLC[dirs.z] = (signs.z == 1 ? kkStart : kkLimit-2);
				for (zz = zzStart; zz != zzLimit; zz += zzDelta, cubeLLC[dirs.z] += zzDelta)
				{
					// Determine cube type
					cubeType = 0;
					if (flags[xx][back][zz]) cubeType += 1;
					if (flags[xx+1][back][zz]) cubeType += 2;
					if (flags[xx+1][front][zz]) cubeType += 4;
					if (flags[xx][front][zz]) cubeType += 8;
					if (flags[xx][back][zz+1]) cubeType += 16;
					if (flags[xx+1][back][zz+1]) cubeType += 32;
					if (flags[xx+1][front][zz+1]) cubeType += 64;
					if (flags[xx][front][zz+1]) cubeType += 128;
// 					printf("YYY Cube %i # %i, LLC=%f %f %f, type = %i\n", xx, zz, cubeLLC.x, cubeLLC.y, cubeLLC.z, cubeType);

					// Quick check for 'whole cube'
					if (cubeType == 255) { if (fillVolume) plotCube(1.0, 1, cubeLLC.x, cubeLLC.y, cubeLLC.z); }
					else
					{
						// Get edges from list and draw triangles or points
						faces = facetriples[cubeType];
						for (n = 0; n<15; ++n)
						{
							if (faces[n] == -1) break;
							plotEdge(cubeType, lowerCutoff,
								values[ edgeData[faces[n]][0].x+xx ][ edgeData[faces[n]][0].y ][ edgeData[faces[n]][0].z+zz ],
								values[ edgeData[faces[n]][1].x+xx ][ edgeData[faces[n]][1].y ][ edgeData[faces[n]][1].z+zz ],
								gradients[ edgeData[faces[n]][0].x+xx ][ edgeData[faces[n]][0].y ][ edgeData[faces[n]][0].z+zz ],
								gradients[ edgeData[faces[n]][1].x+xx ][ edgeData[faces[n]][1].y ][ edgeData[faces[n]][1].z+zz ],
								cubeLLC, edgevertices[faces[n]][0], edgevertices[faces[n]][1], colourScale);
						}
					}
				}
			}
		}
		else if (dirs.x == 2)
		{
			// Principal direction is Z : slice is in XY
			cubeLLC[dirs.y] = (signs.y == 1 ? jjStart : jjLimit-2);

			xxStart = (signs.y == 1 ? 0 : sliceSize[dirs.y] - 2);
			xxLimit = (signs.y == 1 ? sliceSize[dirs.y] - 1 : -1);
			xxDelta = signs.y;
			yyStart = (signs.z == 1 ? 0 : sliceSize[dirs.z] - 2);
			yyLimit = (signs.z == 1 ? sliceSize[dirs.z] - 1 : -1);
			yyDelta = signs.z;

			for (xx=xxStart; xx != xxLimit; xx += xxDelta, cubeLLC[dirs.y] += xxDelta)
			{
				cubeLLC[dirs.z] = (signs.z == 1 ? kkStart : kkLimit-2);
				for (yy = yyStart; yy != yyLimit; yy += yyDelta, cubeLLC[dirs.z] += yyDelta)
				{
					// Determine cube type
					cubeType = 0;
					if (flags[xx][yy][back]) cubeType += 1;
					if (flags[xx+1][yy][back]) cubeType += 2;
					if (flags[xx+1][yy+1][back]) cubeType += 4;
					if (flags[xx][yy+1][back]) cubeType += 8;
					if (flags[xx][yy][front]) cubeType += 16;
					if (flags[xx+1][yy][front]) cubeType += 32;
					if (flags[xx+1][yy+1][front]) cubeType += 64;
					if (flags[xx][yy+1][front]) cubeType += 128;
// 					printf("ZZZ Cube %i %i #, LLC=%f %f %f, type = %i\n", xx, yy, cubeLLC.x, cubeLLC.y, cubeLLC.z, cubeType);

					// Quick check for 'whole cube'
					if (cubeType == 255) { if (fillVolume) plotCube(1.0, 1, cubeLLC.x, cubeLLC.y, cubeLLC.z); }
					else
					{
						// Get edges from list and draw triangles or points
						faces = facetriples[cubeType];
						for (n = 0; n<15; ++n)
						{
							if (faces[n] == -1) break;
							plotEdge(cubeType, lowerCutoff,
								values[ edgeData[faces[n]][0].x+xx ][ edgeData[faces[n]][0].y+yy ][ edgeData[faces[n]][0].z ],
								values[ edgeData[faces[n]][1].x+xx ][ edgeData[faces[n]][1].y+yy ][ edgeData[faces[n]][1].z ],
								gradients[ edgeData[faces[n]][0].x+xx ][ edgeData[faces[n]][0].y+yy ][ edgeData[faces[n]][0].z ],
								gradients[ edgeData[faces[n]][1].x+xx ][ edgeData[faces[n]][1].y+yy ][ edgeData[faces[n]][1].z ],
								cubeLLC, edgevertices[faces[n]][0], edgevertices[faces[n]][1], colourScale);
						}
					}
				}
			}
		}
	}

	updateMesh();
}

// Render volumetric isosurface with Marching Cubes ORIGINAL
// void Primitive::marchingCubesOriginal(Grid* source, double lowerCutoff, double upperCutoff, int colourScale)
// {
// 	int ii, jj, kk, n, cubeType, *faces;
// 	Vec3<GLfloat> normal, gradient[8];
// 	Vec3<double> r, v1, v2;
// 	Vec3<int> nPoints = source->nXYZ(), shift = source->shift();
// 	WrapInt i, j, k;
// 	double*** data, vertex[8], ipol, a, b, twodx, twody, twodz, mult = 1.0;
// 	Vec4<GLfloat> colour;
// 	data = source->data3d();
// 	bool periodic = source->periodic();
// 	bool fillVolume = source->fillEnclosedVolume();
// 
// 	// Get distances between grid points
// 	r = source->lengths();
// 	twodx = r.x / nPoints.x * 2.0;
// 	twody = r.y / nPoints.y * 2.0;
// 	twodz = r.z / nPoints.z * 2.0;
// 
// 	// Initialise
// 	initialise(GL_TRIANGLES, colourScale != -1);
// 
// 	// Loops here will go over 0 to npoints-1, but actual array indices will be based on this plus shift amounts, folded into limits
// 	// Set up the wrapped integers for this purpose
// 	i.setLimits(0, nPoints.x-1);
// 	j.setLimits(0, nPoints.y-1);
// 	k.setLimits(0, nPoints.z-1);
// 
// 	// Generate isosurface
// 	for (ii=0; ii< nPoints.x-1; ++ii)
// 	{
// 		if (!periodic && ((ii < 2) || (ii > (nPoints.x-3)))) continue;
// 		i = ii-shift.x;
// 		for (jj=0; jj< nPoints.y-3; ++jj)
// 		{
// 			if (!periodic && ((jj < 2) || (jj > (nPoints.y-3)))) continue;
// 			j = jj-shift.y;
// 			for (kk=0; kk< nPoints.z-1; ++kk)
// 			{
// 				if (!periodic && ((kk < 2) || (kk > (nPoints.z-3)))) continue;
// 				k = kk-shift.z;
// 
// 				// Grab values that form vertices of cube.
// 				vertex[0] = data[i][j][k];
// 				vertex[1] = data[i+1][j][k];
// 				vertex[2] = data[i+1][j+1][k];
// 				vertex[3] = data[i][j+1][k];
// 				vertex[4] = data[i][j][k+1];
// 				vertex[5] = data[i+1][j][k+1];
// 				vertex[6] = data[i+1][j+1][k+1];
// 				vertex[7] = data[i][j+1][k+1];
//
// 				// Calculate gradients at the cube vertices
// 				gradient[0].x = (vertex[1] - data[i-1][j][k]) / twodx;
// 				gradient[0].y = (vertex[3] - data[i][j-1][k]) / twody;
// 				gradient[0].z = (vertex[4] - data[i][j][k-1]) / twodz;
// 				gradient[1].x = (data[i+2][j][k] - vertex[0]) / twodx;
// 				gradient[1].y = (vertex[2] - data[i+1][j-1][k]) / twody;
// 				gradient[1].z = (vertex[5] - data[i+1][j][k-1]) / twodz;
// 				gradient[2].x = (data[i+2][j+1][k] - vertex[3]) / twodx;
// 				gradient[2].y = (data[i+1][j+2][k] - vertex[1]) / twody;
// 				gradient[2].z = (vertex[6] - data[i+1][j+1][k-1]) / twodz;
// 				gradient[3].x = (vertex[2] - data[i-1][j+1][k]) / twodx;
// 				gradient[3].y = (data[i][j+2][k] - vertex[0]) / twody;
// 				gradient[3].z = (vertex[7] - data[i][j+1][k-1]) / twodz;
// 				gradient[4].x = (vertex[5] - data[i-1][j][k+1]) / twodx;
// 				gradient[4].y = (vertex[7] - data[i][j-1][k+1]) / twody;
// 				gradient[4].z = (data[i][j][k+2] - vertex[0]) / twodz;
// 				gradient[5].x = (data[i+2][j][k+1] - vertex[4]) / twodx;
// 				gradient[5].y = (vertex[6] - data[i+1][j-1][k+1]) / twody;
// 				gradient[5].z = (data[i+1][j][k+2] - vertex[1]) / twodz;
// 				gradient[6].x = (data[i+2][j+1][k+1] - vertex[7]) / twodx;
// 				gradient[6].y = (data[i+1][j+2][k+1] - vertex[5]) / twody;
// 				gradient[6].z = (data[i+1][j+1][k+2] - vertex[2]) / twodz;
// 				gradient[7].x = (vertex[6] - data[i-1][j+1][k+1]) / twodx;
// 				gradient[7].y = (data[i][j+2][k+1] - vertex[4]) / twody;
// 				gradient[7].z = (data[i][j+1][k+2] - vertex[3]) / twodz;
// 
// 				// Determine cube type
// 				cubeType = 0;
// 				if ((vertex[0] >= lowerCutoff) && (vertex[0] <= upperCutoff)) cubeType += 1;
// 				if ((vertex[1] >= lowerCutoff) && (vertex[1] <= upperCutoff)) cubeType += 2;
// 				if ((vertex[2] >= lowerCutoff) && (vertex[2] <= upperCutoff)) cubeType += 4;
// 				if ((vertex[3] >= lowerCutoff) && (vertex[3] <= upperCutoff)) cubeType += 8;
// 				if ((vertex[4] >= lowerCutoff) && (vertex[4] <= upperCutoff)) cubeType += 16;
// 				if ((vertex[5] >= lowerCutoff) && (vertex[5] <= upperCutoff)) cubeType += 32;
// 				if ((vertex[6] >= lowerCutoff) && (vertex[6] <= upperCutoff)) cubeType += 64;
// 				if ((vertex[7] >= lowerCutoff) && (vertex[7] <= upperCutoff)) cubeType += 128;
// 				
// 				printf("CubeType %i %i %i = %i\n", ii, jj, kk, cubeType);
// 				if (cubeType == 255)
// 				{
// 					if (fillVolume) plotCube(1.0, 1, ii, jj, kk);
// 				}
// 				else if (cubeType != 0)
// 				{
// 					// Get edges from list and draw triangles or points
// 					faces = facetriples[cubeType];
// 					for (n = 0; n<15; n++)
// 					{
// 						if (faces[n] == -1) break;
// 
// 						// Get edge vectors, interpolate, and set tri-points
// 						a = vertex[edgevertices[faces[n]][0]];
// 						b = vertex[edgevertices[faces[n]][1]];
// 						ipol = ((mult*lowerCutoff) - a) / (b-a);
// 						if (ipol> 1.0) ipol = 1.0;
// 						if (ipol < 0.0) ipol = 0.0;
// 						v1 = vertexPos[edgevertices[faces[n]][0]];
// 						v2 = vertexPos[edgevertices[faces[n]][1]];
// 						r.set(v2[0]-v1[0], v2[1]-v1[1], v2[2]-v1[2]);
// 						r *= ipol;
// 						printf("Gradient A = "); gradient[edgevertices[faces[n]][0]].print();
// 						printf("Gradient B = "); gradient[edgevertices[faces[n]][1]].print();
// 						normal = (gradient[edgevertices[faces[n]][0]] + (gradient[edgevertices[faces[n]][1]] - gradient[edgevertices[faces[n]][0]]) * ipol) * -mult;
// 						normal.normalise();
// 						r.add(ii+v1[0], jj+v1[1], kk+v1[2]);
// 
// 						// Set triangle coordinates and add cube position
// 						if (colourScale != -1)
// 						{
// 							prefs.colourScale[colourScale].colour((a+b)/2.0, colour);
// 							defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z, colour);
// 						}
// 						else defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z);
// 					}
// 				}
// 			}
// 		}
// 	}
// 
// 	updateMesh();
// }

// Render normal '2D' surface 
void Primitive::createSurface(Grid* source, Vec4<GLfloat> colour, int colourScale)
{
	int i, j, n;
	Vec3<double> normal[4];
	Vec3<int> nPoints = source->nXYZ();
	GLfloat minAlpha;
	double** data;
	bool usez = source->useDataForZ();

	// Grab the data pointer and surface cutoff
	data = source->data2d();

	// Grab colours (if not using a colourscale) and determine colour mode to use
	initialise(GL_TRIANGLES, colourScale != -1);
	minAlpha = 1.0f;

	int di = 0, di2 = 2, dj, dj2;
	for (i = 0; i< nPoints.x-1; ++i)
	{
		if (i == nPoints.x-2) di2 = 1;
		dj = 0;
		dj2 = 2;

		for (j = 0; j< nPoints.y-1; ++j)
		{
			if (j == nPoints.y-2) dj2 = 1;
			// Calculate normals...
			normal[0] = Vec3<double>(1.0,0.0,(data[i+1][j] - data[i-di][j])*0.5) * Vec3<double>(0.0,1.0,(data[i][j+1] - data[i][j-dj])*0.5);	// N(i,j)
			normal[1] = Vec3<double>(1.0,0.0,(data[i+di2][j] - data[i][j])*0.5) * Vec3<double>(0.0,1.0,(data[i+1][j+1] - data[i+1][j-dj])*0.5);	// N(i+1,j)
			normal[2] = Vec3<double>(1.0,0.0,(data[i+1][j+1] - data[i-di][j+1])*0.5) * Vec3<double>(0.0,1.0,(data[i][j+dj2] - data[i][j])*0.5);	// N(i,j+1)
			normal[3] = Vec3<double>(1.0,0.0,(data[i+di2][j+1] - data[i][j+1])*0.5) * Vec3<double>(0.0,1.0,(data[i+1][j+dj2] - data[i+1][j])*0.5);	// N(i+1,j+1)
			for (n=0; n<4; ++n) normal[n].normalise();
			
			// Set triangle coordinates and add cube position
			if (colourScale != -1)
			{
				// First triangle
				prefs.colourScale[colourScale].colour(data[i][j], colour);
				defineVertex(i, j, usez ? data[i][j] : 0.0, normal[0].x, normal[0].y, normal[0].z, colour);
				if (colour[3] < minAlpha) minAlpha = colour[3];
				
				prefs.colourScale[colourScale].colour(data[i+1][j], colour);
				defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z, colour);
				if (colour[3] < minAlpha) minAlpha = colour[3];
				
				prefs.colourScale[colourScale].colour(data[i][j+1], colour);
				defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z, colour);
				if (colour[3] < minAlpha) minAlpha = colour[3];
				
				// Second triangle
				defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z, colour);
				
				prefs.colourScale[colourScale].colour(data[i+1][j], colour);
				defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z, colour);
				if (colour[3] < minAlpha) minAlpha = colour[3];

				prefs.colourScale[colourScale].colour(data[i+1][j+1], colour);
				defineVertex(i+1, j+1, usez ? data[i+1][j+1] : 0.0, normal[3].x, normal[3].y, normal[3].z, colour);
				if (colour[3] < minAlpha) minAlpha = colour[3];
			}
			else
			{
				// First triangle
				defineVertex(i, j, usez ? data[i][j] : 0.0, normal[0].x, normal[0].y, normal[0].z);
				defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z);
				defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z);
				
				// Second triangle
				defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z);
				defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z);
				defineVertex(i+1, j+1, usez ? data[i+1][j+1] : 0.0, normal[3].x, normal[3].y, normal[3].z);	
			}
			dj = 1;

		}
		di = 1;
	}

	updateMesh();
}

/*
// Create volume axes
void RenderGroup::createVolumeAxes(Grid* source)
{
	// Get some useful values before we start...
	Vec3<double> origin, axisRealMajorSpacing, axisRealMinorSpacing, gridDelta, axisRealMin, axisRealMax, axisRealRange, realToGrid;
	Vec3<int> axisMinorNTicks;
	double majorTickSize = 0.1, tickFactor = 1.0;
	int n, i, j, tickCount;
	bool majorTick;
	
	// Get useful values from source Grid
	for (n=0; n<3; ++n)
	{
		gridDelta[n] = source_->axes().columnMagnitude(n);
		axisRealMin[n] = source_->dataMinimum()[n];
		axisRealMax[n] = source_->dataMaximum()[n];
		axisRealRange[n] = axisRealMax[n] - axisRealMin[n]; 

		// Calculate conversion factor
		realToGrid[n] = source_->nXYZ()[n] / axisRealRange[n];
	}
	axisRealMajorSpacing = source_->axisMajorSpacing();
	axisMinorNTicks = source_->axisMinorTicks() + 1;

	// Loop over axes
	for (n=0; n<3; ++n)
	{
		// Clear any old data
		axisLinePrimitives_[n].forgetAll();
		axisTextPrimitives_[n].clear();

		double x, value;
		double tickSize1 = majorTickSize / gridDelta[(n+1)%3], tickSize2 = majorTickSize / gridDelta[(n+2)%3];
		Vec3<double> v;
// 		printf("(axis %i) gridDelta = %f, real min/max = %f/%f, xSpacing = %f\n", n, gridDelta[n], axisRealMin[n], axisRealMax[n], axisRealRange[n], axisRealMajorSpacing[n]);

		// Set origin coordinates for axis (in grid units)
		origin.zero();
		for (i=1; i<3; ++i)
		{
			j = (n+i)%3;
			origin.set(j, (source_->axisPosition(n)[j] - axisRealMin[j]) / gridDelta[j]);
		}

		// Render axis line
		axisLinePrimitives_[n].defineVertex(origin.x, origin.y, origin.z, 1.0, 0.0, 0.0);
		v = origin;
		v.add(n, axisRealRange[n]*realToGrid[n]);
		axisLinePrimitives_[n].defineVertex(v.x, v.y, v.z, 1.0, 0.0, 0.0);

		// Determine starting axis value and tickCount...
		if (axisMinorNTicks[n] < 1) axisMinorNTicks[n] = 1;
		axisRealMinorSpacing[n] = axisRealMajorSpacing[n] /= axisMinorNTicks[n];
		// -- First, set value to start at next whole spacing...
		value = int(axisRealMin[n] / axisRealMajorSpacing[n])*axisRealMajorSpacing[n] + (axisRealMin[n] > 0.0 ? 1.0 : 0.0);
		// -- Now, do we have any minor tick values to put in the gap between the minimum and 'value'
		tickCount = int( (value - axisRealMin[n]) / axisRealMinorSpacing[n] );
		value -= tickCount * axisRealMinorSpacing[n];
		tickCount = axisMinorNTicks[n] - tickCount;

		// Draw axis...
		while (value <= axisRealMax[n])
		{
			// Convert current value to Grid-coordinates
			x = (value - axisRealMin[n]) * realToGrid[n];
			v = origin;
			v.add(n, x);

			// Major / minor tick?
			majorTick = (tickCount%axisMinorNTicks[n] == 0);
			tickFactor = majorTickSize * (tickCount%axisMinorNTicks[n] == 0 ? 1.0 : 0.5);
			
			// Draw line in first other axis direction
			i = (n+1)%3;
			v.add(i, -tickFactor*tickSize1);
			axisLinePrimitives_[n].defineVertex(v.x, v.y, v.z, 1.0, 0.0, 0.0);
			v.add(i, 2.0*tickFactor*tickSize1);
			axisLinePrimitives_[n].defineVertex(v.x, v.y, v.z, 1.0, 0.0, 0.0);

			// Reset position back to centre of original line
			v[i] = origin.get(i);

			// Draw line in second other axis direction
			i = (n+2)%3;
			v.add(i, -tickFactor*tickSize2);
			axisLinePrimitives_[n].defineVertex(v.x, v.y, v.z, 1.0, 0.0, 0.0);
			v.add(i, 2.0*tickFactor*tickSize2);
			axisLinePrimitives_[n].defineVertex(v.x, v.y, v.z, 1.0, 0.0, 0.0);

			// Reset position back to centre of original line
			v[i] = origin.get(i);

			// Draw label (if a major tick)
			if (majorTick)
			{
				i = (n+1)%3;
				v.add(i, origin[i] < 0.5*(axisRealRange[i] + axisRealMin[i]) ? -tickSize1 : tickSize1);
				i = (n+2)%3;
				v.add(i, origin[i] < 0.5*(axisRealRange[i] + axisRealMin[i]) ? -tickSize2 : tickSize2);
// 				axisTextPrimitives_[n].add()->set(QString::number(value, "%3.0f"), v, TextPrimitive::MiddleLeftAnchor, Vec3<double>(), Matrix(), 1.0); // ATEN2 TODO
			}

			// Add on delta
			value += axisRealMinorSpacing[n];
			++tickCount;
		}
	}
}*/