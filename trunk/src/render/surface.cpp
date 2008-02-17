/*
	*** Surface rendering
	*** src/render/surface.cpp
	Copyright T. Youngs 2007

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

#include "base/master.h"
#include "gui/canvas.h"
#ifdef IS_MAC
	#include <GLUT/glut.h>
#else
	#include <GL/glut.h>
#endif

/*
	Marching Cube vertex / <edge> numbering

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
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}
};

// Render grid points (no surface)
void render_surface_grid(grid *g)
{
	int i, j, k;
	vec3<int> npoints = g->get_npoints();
	double ***data, **xdata, *ydata, cutoff;
	// Grab the data pointer and cutoff
	data = g->get_data();
	cutoff = g->get_cutoff();
	glBegin(GL_POINTS);
	  for (i=0; i<npoints.x; i++)
	  {
		xdata = data[i];
		for (j=0; j<npoints.y; j++)
		{
			ydata = xdata[j];
			for (k=0; k<npoints.z; k++)
			{
				if (ydata[k] < cutoff) continue;
				glVertex3i(i, j, k);
			}
		}
	  }
	glEnd();
}

// Render isosurface with Marching Cubes
void cube_it(grid *g, surface_style ss)
{
	int i, j, k, n, cubetype, *faces;
	vec3<double> r, normal;
	vec3<int> npoints = g->get_npoints();
	double ***data, **xdata, *ydata, cutoff, vertex[8], ipol, a, b, *evec, *v1, *v2;
	// Grab the data pointer and surface cutoff
	data = g->get_data();
	cutoff = g->get_cutoff();
	// Set glBegin based on the surface style
	switch (ss)
	{
		case (SS_POINTS):
			glBegin(GL_POINTS);
			break;
		case (SS_TRIANGLES):
			glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
			glBegin(GL_TRIANGLES);
			break;
		case (SS_SOLID):
			glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
			glBegin(GL_TRIANGLES);
			break;
	}
	// Set colour / transparency for surface
	glMaterialiv(GL_FRONT, GL_SPECULAR, prefs.get_colour(COL_SPECREFLECT));
	glMateriali(GL_FRONT, GL_SHININESS, prefs.get_shininess());
	glMaterialiv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, g->get_colour());
	// Generate surface
	for (i=1; i<npoints.x-1; i++)
	{
		xdata = data[i];
		for (j=1; j<npoints.y-1; j++)
		{
			ydata = xdata[j];
			for (k=1; k<npoints.z-1; k++)
			{
				// Grab values that form vertices of cube.
				vertex[0] = data[i][j][k];
				vertex[1] = data[i+1][j][k];
				vertex[2] = data[i+1][j+1][k];
				vertex[3] = data[i][j+1][k];
				vertex[4] = data[i][j][k+1];
				vertex[5] = data[i+1][j][k+1];
				vertex[6] = data[i+1][j+1][k+1];
				vertex[7] = data[i][j+1][k+1];
				// Determine cube type
				cubetype = 0;
				if (vertex[0] >= cutoff) cubetype += 1;
				if (vertex[1] >= cutoff) cubetype += 2;
				if (vertex[2] >= cutoff) cubetype += 4;
				if (vertex[3] >= cutoff) cubetype += 8;
				if (vertex[4] >= cutoff) cubetype += 16;
				if (vertex[5] >= cutoff) cubetype += 32;
				if (vertex[6] >= cutoff) cubetype += 64;
				if (vertex[7] >= cutoff) cubetype += 128;
				// Get edges from list and draw triangles or points
				faces = facetriples[cubetype];
				for (n = 0; n<15; n++)
				{
					if (faces[n] == -1) break;
					// Get edge vectors, interpolate, and set tri-points
					a = vertex[edgevertices[faces[n]][0]];
					b = vertex[edgevertices[faces[n]][1]];
					ipol = (cutoff - a) / (b-a);
					//printf("IPOL %f %f %f %f\n",a,b,cutoff,ipol);
					//ipol = 1.0;
					v1 = vertexpos[edgevertices[faces[n]][0]];
					v2 = vertexpos[edgevertices[faces[n]][1]];
					r.set(v2[0]-v1[0], v2[1]-v1[1], v2[2]-v1[2]);
					r *= ipol;
					normal = r;
					r.add(i+v1[0], j+v1[1], k+v1[2]);
					// Set triangle coordinates and add cube position
					glNormal3d(normal.x, normal.y, normal.z);
					glVertex3d(r.x, r.y, r.z);
				}
			}
		}
	  }
	glEnd();
}

// Render surfaces
void canvas::render_surfaces()
{
	dbg_begin(DM_CALLS,"canvas::render_surfaces");
	// Loop over surfaces held by the master, rendering those that are visible.
	// If the log of a particular surface is out of data, recreate its display list first
	static GLuint list;
	static vec3<double> origin;
	static double axes[16];
	static mat4<double> mat;
	for (grid *g = master.get_grids(); g != NULL; g = g->next)
	{
		// Check visibility
		if (!g->get_visible()) continue;
		// Get GL display list and check render point
		list = g->get_displaylist();

		if (g->should_rerender())
		{
			if (list != -1) glDeleteLists(list,1);
			glNewList(list,GL_COMPILE);
			  switch (g->get_style())
			  {
				case (SS_GRID):
					render_surface_grid(g);
					break;
				default:
					cube_it(g, g->get_style());
					break;
			  }
			glEndList();
			g->update_renderpoint();
		}
		// Draw surface - push current GL matrix
		glPushMatrix();
		  // Centre on cell origin (lower left-hand corner)
		  origin = g->get_origin();
		  glTranslated(origin.x, origin.y, origin.z);
		  // Apply matrix transform to get proper grid axes / shear
		  mat = g->get_axes();
		  mat.get_column_major(axes);
		  glMatrixMode(GL_MODELVIEW);
		  glMultMatrixd(axes);
		  // Call the display list
		  glCallList(list);
		glPopMatrix();
	}
	dbg_end(DM_CALLS,"canvas::render_surfaces");
}
