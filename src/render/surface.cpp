/*
	*** Primitive - Surface Generation
	*** src/render/primitive_surface.cpp
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
#include "base/grid.h"
#include "base/prefs.h"
#include "base/grid.h"
#include "base/wrapint.h"
#include <base/sysfunc.h>

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

//double vertexpos[8][3] = { {1.0,0.0,0.0}, {0.0,1.0,0.0}, {-1.0,0.0,0.0}, {0.0,-1.0,0.0},
//	{1.0,0.0,0.0}, {0.0,1.0,0.0}, {-1.0,0.0,0.0}, {0.0,-1.0,0.0},
//	{0.0,0.0,1.0}, {0.0,0.0,1.0}, {0.0,0.0,1.0}, {0.0,0.0,1.0} };

double vertexpos[8][3] = { {0,0,0}, {1,0,0}, {1,1,0}, {0,1,0},
	{0,0,1}, {1,0,1}, {1,1,1}, {0,1,1} };

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

// Render volumetric isosurface with Marching Cubes
void GridPrimitive::createSurfaceMarchingCubes()
{
	int ii, jj, kk, n, cubetype, *faces, cscale;
	Vec3<GLfloat> normal, gradient[8];
	Vec3<double> r;
	Vec3<int> npoints = source_->nXYZ(), shift = source_->shift();
	WrapInt i, j, k;
	Vec4<GLfloat> col1, col2;
	GLfloat minalpha1, minalpha2;
	double ***data, vertex[8], ipol, a, b, *v1, *v2, twodx, twody, twodz, mult;
	data = source_->data3d();
	bool secondary = source_->useSecondary(), periodic = source_->periodic();
	cscale = source_->useColourScale() ? source_->colourScale() : -1;
	bool fillVolume = source_->fillEnclosedVolume();

	mult = 1.0;
	// Get distances between grid points
	r = source_->lengths();
	twodx = r.x / npoints.x * 2.0;
	twody = r.y / npoints.y * 2.0;
	twodz = r.z / npoints.z * 2.0;

	// Grab colours (if not using a colourscale) and determine colour mode to use
	if (cscale == -1)
	{
		source_->copyPrimaryColour(col1);
		source_->copySecondaryColour(col2);
// 		primaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
// 		secondaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
		minalpha1 = col1[3];
		minalpha2 = col2[3];
	}
	else
	{
// 		primaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
// 		secondaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
		minalpha1 = 1.0f;
		minalpha2 = 1.0f;
	}

	// Clear primitive data
	primaryPrimitive_.forgetAll();
	secondaryPrimitive_.forgetAll();
	
	// Loops here will go over 0 to npoints-1, but actual array indices will be based on this plus shift amounts, folded into limits
	// Set up the wrapped integers for this purpose
	i.setLimits(0, npoints.x-1);
	j.setLimits(0, npoints.y-1);
	k.setLimits(0, npoints.z-1);
	
	// Generate isosurface
	for (ii=0; ii<npoints.x; ++ii)
	{
		if (!periodic && ((ii < 2) || (ii > (npoints.x-3)))) continue;
		i = ii-shift.x;
		for (jj=0; jj<npoints.y; ++jj)
		{
			if (!periodic && ((jj < 2) || (jj > (npoints.y-3)))) continue;
			j = jj-shift.y;
			for (kk=0; kk<npoints.z; ++kk)
			{
				if (!periodic && ((kk < 2) || (kk > (npoints.z-3)))) continue;
				k = kk-shift.z;
				// Grab values that form vertices of cube.
				vertex[0] = data[i][j][k];
				vertex[1] = data[i+1][j][k];
				vertex[2] = data[i+1][j+1][k];
				vertex[3] = data[i][j+1][k];
				vertex[4] = data[i][j][k+1];
				vertex[5] = data[i+1][j][k+1];
				vertex[6] = data[i+1][j+1][k+1];
				vertex[7] = data[i][j+1][k+1];
				// Calculate gradients at the cube vertices
				gradient[0].x = (vertex[1] - data[i-1][j][k]) / twodx;
				gradient[0].y = (vertex[3] - data[i][j-1][k]) / twody;
				gradient[0].z = (vertex[4] - data[i][j][k-1]) / twodz;
				gradient[1].x = (data[i+2][j][k] - vertex[0]) / twodx;
				gradient[1].y = (vertex[2] - data[i+1][j-1][k]) / twody;
				gradient[1].z = (vertex[5] - data[i+1][j][k-1]) / twodz;
				gradient[2].x = (data[i+2][j+1][k] - vertex[3]) / twodx;
				gradient[2].y = (data[i+1][j+2][k] - vertex[1]) / twody;
				gradient[2].z = (vertex[6] - data[i+1][j+1][k-1]) / twodz;
				gradient[3].x = (vertex[2] - data[i-1][j+1][k]) / twodx;
				gradient[3].y = (data[i][j+2][k] - vertex[0]) / twody;
				gradient[3].z = (vertex[7] - data[i][j+1][k-1]) / twodz;
				gradient[4].x = (vertex[5] - data[i-1][j][k+1]) / twodx;
				gradient[4].y = (vertex[7] - data[i][j-1][k+1]) / twody;
				gradient[4].z = (data[i][j][k+2] - vertex[0]) / twodz;
				gradient[5].x = (data[i+2][j][k+1] - vertex[4]) / twodx;
				gradient[5].y = (vertex[6] - data[i+1][j-1][k+1]) / twody;
				gradient[5].z = (data[i+1][j][k+2] - vertex[1]) / twodz;
				gradient[6].x = (data[i+2][j+1][k+1] - vertex[7]) / twodx;
				gradient[6].y = (data[i+1][j+2][k+1] - vertex[5]) / twody;
				gradient[6].z = (data[i+1][j+1][k+2] - vertex[2]) / twodz;
				gradient[7].x = (vertex[6] - data[i-1][j+1][k+1]) / twodx;
				gradient[7].y = (data[i][j+2][k+1] - vertex[4]) / twody;
				gradient[7].z = (data[i][j+1][k+2] - vertex[3]) / twodz;
				// Determine cube type (primary cutoff)
				cubetype = 0;
				if (source_->withinPrimaryCutoff(vertex[0])) cubetype += 1;
				if (source_->withinPrimaryCutoff(vertex[1])) cubetype += 2;
				if (source_->withinPrimaryCutoff(vertex[2])) cubetype += 4;
				if (source_->withinPrimaryCutoff(vertex[3])) cubetype += 8;
				if (source_->withinPrimaryCutoff(vertex[4])) cubetype += 16;
				if (source_->withinPrimaryCutoff(vertex[5])) cubetype += 32;
				if (source_->withinPrimaryCutoff(vertex[6])) cubetype += 64;
				if (source_->withinPrimaryCutoff(vertex[7])) cubetype += 128;
				if (cubetype == 255)
				{
					if (fillVolume) primaryPrimitive_.plotCube(1.0, 1, ii, jj, kk);
				}
				else if (cubetype != 0)
				{
					// Get edges from list and draw triangles or points
					faces = facetriples[cubetype];
					for (n = 0; n<15; n++)
					{
						if (faces[n] == -1) break;
						// Get edge vectors, interpolate, and set tri-points
						a = vertex[edgevertices[faces[n]][0]];
						b = vertex[edgevertices[faces[n]][1]];
						ipol = ((mult*source_->lowerPrimaryCutoff()) - a) / (b-a);
						if (ipol> 1.0) ipol = 1.0;
						if (ipol < 0.0) ipol = 0.0;
						v1 = vertexpos[edgevertices[faces[n]][0]];
						v2 = vertexpos[edgevertices[faces[n]][1]];
						r.set(v2[0]-v1[0], v2[1]-v1[1], v2[2]-v1[2]);
						r *= ipol;
						normal = (gradient[edgevertices[faces[n]][0]] + (gradient[edgevertices[faces[n]][1]] - gradient[edgevertices[faces[n]][0]]) * ipol) * -mult;
						normal.normalise();
						r.add(ii+v1[0], jj+v1[1], kk+v1[2]);
						// Set triangle coordinates and add cube position
						if (cscale != -1)
						{
							prefs.colourScale[cscale].colour((a+b)/2.0, col1);
							primaryPrimitive_.defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z, col1);
						}
						else primaryPrimitive_.defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z);
						// Store minimal alpha value in order to determine transparency
						if (col1[3] < minalpha1) minalpha1 = col1[3];
					}
				}
				if (!secondary) continue;
				// Determine cube type (secondary cutoff)
				cubetype = 0;
				if (source_->withinSecondaryCutoff(vertex[0])) cubetype += 1;
				if (source_->withinSecondaryCutoff(vertex[1])) cubetype += 2;
				if (source_->withinSecondaryCutoff(vertex[2])) cubetype += 4;
				if (source_->withinSecondaryCutoff(vertex[3])) cubetype += 8;
				if (source_->withinSecondaryCutoff(vertex[4])) cubetype += 16;
				if (source_->withinSecondaryCutoff(vertex[5])) cubetype += 32;
				if (source_->withinSecondaryCutoff(vertex[6])) cubetype += 64;
				if (source_->withinSecondaryCutoff(vertex[7])) cubetype += 128;
				if (cubetype != 0)
				{
					// Get edges from list and draw triangles or points
					faces = facetriples[cubetype];
					for (n = 0; n<15; n++)
					{
						if (faces[n] == -1) break;
						// Get edge vectors, interpolate, and set tri-points
						a = vertex[edgevertices[faces[n]][0]];
						b = vertex[edgevertices[faces[n]][1]];
						ipol = ((mult*source_->lowerSecondaryCutoff()) - a) / (b-a);
						if (ipol> 1.0) ipol = 1.0;
						if (ipol < 0.0) ipol = 0.0;
						v1 = vertexpos[edgevertices[faces[n]][0]];
						v2 = vertexpos[edgevertices[faces[n]][1]];
						r.set(v2[0]-v1[0], v2[1]-v1[1], v2[2]-v1[2]);
						r *= ipol;
						normal = (gradient[edgevertices[faces[n]][0]] + (gradient[edgevertices[faces[n]][1]] - gradient[edgevertices[faces[n]][0]]) * ipol) * -mult;
						normal.normalise();
						r.add(ii+v1[0], jj+v1[1], kk+v1[2]);
						// Set triangle coordinates and add cube position
						if (cscale != -1)
						{
							prefs.colourScale[cscale].colour((a+b)/2.0, col2);
							secondaryPrimitive_.defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z, col2);
						}
						else secondaryPrimitive_.defineVertex(r.x, r.y, r.z, normal.x, normal.y, normal.z);
						
						// Store minimal alpha value in order to determine transparency
						if (col2[3] < minalpha2) minalpha2 = col2[3];
					}
				}
			}
		}
	}

	// Set transparency flags
	primaryIsTransparent_ = (minalpha1 <= 0.99f);
	secondaryIsTransparent_ = (minalpha2 <= 0.99f);	
}

// Render normal '2D' surface 
void GridPrimitive::createSurface2D()
{
	int i, j, n;
	Vec3<double> normal[4];
	int cscale = source_->useColourScale() ? source_->colourScale() : -1;
	Vec3<int> npoints = source_->nXYZ();
	Vec4<GLfloat> col1, col2;
	GLfloat minalpha1, minalpha2;
	double **data;
	bool usez = source_->useDataForZ();

	// Grab the data pointer and surface cutoff
	data = source_->data2d();

	// Grab colours (if not using a colourscale) and determine colour mode to use
	if (cscale == -1)
	{
		source_->copyPrimaryColour(col1);
		source_->copySecondaryColour(col2);
// 		primaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
// 		secondaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
		minalpha1 = col1[3];
		minalpha2 = col2[3];
	}
	else
	{
// 		primaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
// 		secondaryPrimitive_.initialise(0, 0, ); ATEN2 TODO
		minalpha1 = 1.0f;
		minalpha2 = 1.0f;
	}

	// Clear primitive data
	primaryPrimitive_.forgetAll();
	secondaryPrimitive_.forgetAll();
	
	int di = 0, di2 = 2, dj, dj2;
	for (i = 0; i<npoints.x-1; ++i)
	{
		if (i == npoints.x-2) di2 = 1;
		dj = 0;
		dj2 = 2;
		for (j = 0; j<npoints.y-1; ++j)
		{
			if (j == npoints.y-2) dj2 = 1;
			// Calculate normals...
			normal[0] = Vec3<double>(1.0,0.0,(data[i+1][j] - data[i-di][j])*0.5) * Vec3<double>(0.0,1.0,(data[i][j+1] - data[i][j-dj])*0.5);	// N(i,j)
			normal[1] = Vec3<double>(1.0,0.0,(data[i+di2][j] - data[i][j])*0.5) * Vec3<double>(0.0,1.0,(data[i+1][j+1] - data[i+1][j-dj])*0.5);	// N(i+1,j)
			normal[2] = Vec3<double>(1.0,0.0,(data[i+1][j+1] - data[i-di][j+1])*0.5) * Vec3<double>(0.0,1.0,(data[i][j+dj2] - data[i][j])*0.5);	// N(i,j+1)
			normal[3] = Vec3<double>(1.0,0.0,(data[i+di2][j+1] - data[i][j+1])*0.5) * Vec3<double>(0.0,1.0,(data[i+1][j+dj2] - data[i+1][j])*0.5);	// N(i+1,j+1)
			for (n=0; n<4; ++n) normal[n].normalise();
			
			// Set triangle coordinates and add cube position
			if (cscale != -1)
			{
				// First triangle
				prefs.colourScale[cscale].colour(data[i][j], col1);
				primaryPrimitive_.defineVertex(i, j, usez ? data[i][j] : 0.0, normal[0].x, normal[0].y, normal[0].z, col1);
				if (col1[3] < minalpha1) minalpha1 = col1[3];
				
				prefs.colourScale[cscale].colour(data[i+1][j], col1);
				primaryPrimitive_.defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z, col1);
				if (col1[3] < minalpha1) minalpha1 = col1[3];
				
				prefs.colourScale[cscale].colour(data[i][j+1], col1);
				primaryPrimitive_.defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z, col1);
				if (col1[3] < minalpha1) minalpha1 = col1[3];
				
				// Second triangle
				primaryPrimitive_.defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z, col1);
				
				prefs.colourScale[cscale].colour(data[i+1][j], col1);
				primaryPrimitive_.defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z, col1);
				if (col1[3] < minalpha1) minalpha1 = col1[3];

				prefs.colourScale[cscale].colour(data[i+1][j+1], col1);
				primaryPrimitive_.defineVertex(i+1, j+1, usez ? data[i+1][j+1] : 0.0, normal[3].x, normal[3].y, normal[3].z, col1);
				if (col1[3] < minalpha1) minalpha1 = col1[3];
			}
			else
			{
				// First triangle
				primaryPrimitive_.defineVertex(i, j, usez ? data[i][j] : 0.0, normal[0].x, normal[0].y, normal[0].z);
				primaryPrimitive_.defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z);
				primaryPrimitive_.defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z);
				
				// Second triangle
				primaryPrimitive_.defineVertex(i, j+1, usez ? data[i][j+1] : 0.0, normal[2].x, normal[2].y, normal[2].z);
				primaryPrimitive_.defineVertex(i+1, j, usez ? data[i+1][j] : 0.0, normal[1].x, normal[1].y, normal[1].z);
				primaryPrimitive_.defineVertex(i+1, j+1, usez ? data[i+1][j+1] : 0.0, normal[3].x, normal[3].y, normal[3].z);	
			}
			dj = 1;

		}
		di = 1;
	}

	// Set transparency flags
	primaryIsTransparent_ = (minalpha1 <= 0.99f);
	secondaryIsTransparent_ = (minalpha2 <= 0.99f);	
}

// Create axes
void GridPrimitive::createAxes()
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
				axisTextPrimitives_[n].add()->set(ftoa(value, "%3.0f"), v, TextPrimitive::MiddleLeftAnchor, Vec3<double>(), Matrix(), 1.0); // ATEN2 TODO
			}

			// Add on delta
			value += axisRealMinorSpacing[n];
			++tickCount;
		}
	}
}