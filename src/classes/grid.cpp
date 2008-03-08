/*
	*** Grid data structure
	*** src/classes/grid.cpp
	Copyright T. Youngs 2007,2008

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

#include "classes/grid.h"
#include "base/debug.h"
#include "base/constants.h"
#ifdef IS_MAC
	#include <OpenGL/gl.h>
#else
	#include <GL/gl.h>
#endif

// Constructor
grid::grid()
{
	prev = NULL;
	next = NULL;
	data = NULL;
	datafull = FALSE;
	min = 10000.0;
	max = -10000.0;
	cutoff = 0.0;
	log = -1;
	style = SS_SOLID;
	displaylist = 0;
	render_point = -1;
	visible = TRUE;
	colour[0] = INT_MAX;
	colour[1] = 0;
	colour[2] = 0;
	colour[3] = INT_MAX / 2;
	looporder.set(0,1,2);
}

// Destructor
grid::~grid()
{
	clear();
}

// Create data array (from npoints vector)
void grid::create()
{
	dbg_begin(DM_CALLS,"grid::create");
	clear();
	int i, j;
	if (data != NULL) clear();
	data = new double**[npoints.x];
	for (i = 0; i<npoints.x; i++)
	{
		data[i] = new double*[npoints.y];
		for (j = 0; j<npoints.y; j++) data[i][j] = new double[npoints.z];
	}
	dbg_end(DM_CALLS,"grid::create");
}

// Clear data array
void grid::clear()
{
	dbg_begin(DM_CALLS,"grid::clear");
	datafull = FALSE;
	min = 10000.0;
	max = -10000.0;
	cutoff = 0.0;
	currentpoint.zero();
	visible = TRUE;
	if (data == NULL) return;
	int i, j;
	for (i = 0; i<npoints.x; i++)
	{
		for (j = 0; j<npoints.y; j++) delete[] data[i][j];
		delete[] data[i];
	}
	delete[] data;
	data = NULL;
	dbg_end(DM_CALLS,"grid::clear");
}

// Set spacing for a cubic grid
void grid::set_axes(double r)
{
	cell.set( vec3<double>(r,r,r), vec3<double>(90.0, 90.0, 90.0) );
	log++;
}

// Set spacing for an orthorhombic grid
void grid::set_axes(const vec3<double> v)
{
	cell.set( v, vec3<double>(90.0, 90.0, 90.0) );
	log++;
}

// Set spacing for a parallelepiped grid
void grid::set_axes(const mat3<double> m)
{
	cell.set(m);
	log++;
}

// Set grid extent (and data[])
void grid::set_npoints(vec3<int> v)
{
	dbg_begin(DM_CALLS,"grid::set_npoints");
	npoints = v;
	log ++;
	create();
	dbg_end(DM_CALLS,"grid::set_npoints");
}

// Update minimum / maximum based on supplied value
void grid::set_limits(double d)
{
	if (d < min) min = d;
	else if (d > max) max = d;
	cutoff = (max - min) * 0.5 + min;
}

// Set specific point in data array
void grid::set_data(int x, int y, int z, double d)
{
	// Check limits against npoints vector
	if ((x < 0) || (x >= npoints.x))
	{
		msg(DM_NONE,"grid::set_data(x,y,z) - X index is outside array bounds.\n");
		return;
	}
	else if ((y < 0) || (y >= npoints.y))
	{
		msg(DM_NONE,"grid::set_data(x,y,z) - Y index is outside array bounds.\n");
		return;
	}
	else if ((z < 0) || (z >= npoints.z))
	{
		msg(DM_NONE,"grid::set_data(x,y,z) - Z index is outside array bounds.\n");
		return;
	}
	// Okay, so store data
	data[x][y][z] = d;
	// Set new minimum / maximum
	set_limits(d);
}

// Set 'next' point in data array
void grid::set_next_data(double d)
{
	// Check limit
	if (datafull == TRUE)
	{
		msg(DM_NONE,"grid::set_next_data - Array already full.\n");
		return;
	}
	// Set current point referenced by currentpoint
	data[currentpoint.x][currentpoint.y][currentpoint.z] = d;
	// Increase currentpoint
	currentpoint.set(looporder.x, currentpoint.get(looporder.x) + 1);
	if (currentpoint.get(looporder.x) == npoints.get(looporder.x))
	{
		currentpoint.set(looporder.x, 0);
		currentpoint.set(looporder.y, currentpoint.get(looporder.y) + 1);
		if (currentpoint.get(looporder.y) == npoints.get(looporder.y))
		{
			currentpoint.set(looporder.y, 0);
			currentpoint.set(looporder.z, currentpoint.get(looporder.z) + 1);
			if (currentpoint.get(looporder.z) == npoints.get(looporder.z)) datafull = TRUE;
		}
	}
	// Set new minimum / maximum
	set_limits(d);
}

// Set surface colour
void grid::set_colour(int r, int g, int b)
{
	colour[0] = r;
	colour[1] = g;
	colour[2] = b;
	log ++;
}

void grid::set_colour(double r, double g, double b)
{
	colour[0] = (GLint) (r * INT_MAX);
	colour[1] = (GLint) (g * INT_MAX);
	colour[2] = (GLint) (b * INT_MAX);
	log ++;
}

// Convert Bohr to Angstrom
void grid::bohr_to_angstrom()
{
	// Only the axes and origin need to be modified...
	mat3<double> newaxes = cell.get_axes();
	newaxes *= ANGBOHR;
	cell.set(newaxes);
	origin *= ANGBOHR;
}

// Return displaylist (create first if necessary)
GLuint grid::get_displaylist()
{
	if (displaylist == 0)
	{
		displaylist = glGenLists(1);
		if (displaylist == 0) printf("Critical - couldn't generate display list for grid data.\n");
	}
	return displaylist;
}
