/*
	*** Surface (grid data) structure
	*** src/classes/surface.cpp

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

#include "classes/surface.h"
#include "base/debug.h"
#include "base/constants.h"
#ifdef IS_MAC
	#include <OpenGL/gl.h>
#else
	#include <GL/gl.h>
#endif

// Constructor
surface::surface()
{
	prev = NULL;
	next = NULL;
	data = NULL;
	datafull = FALSE;
	min = 10000.0;
	max = -10000.0;
	cutoff = 0.0;
	log = -1;
	style = SS_GRID;
	displaylist = -1;
	colour[0] = INT_MAX;
	colour[1] = 0;
	colour[2] = 0;
	colour[3] = INT_MAX / 2;
}

// Destructor
surface::~surface()
{
	clear();
}

// Create data array (from npoints vector)
void surface::create()
{
	dbg_begin(DM_CALLS,"surface::create");
	clear();
	int i, j;
	data = new double**[npoints.x];
	for (i = 0; i<npoints.x; i++)
	{
		data[i] = new double*[npoints.y];
		for (j = 0; j<npoints.y; j++) data[i][j] = new double[npoints.z];
	}
	dbg_end(DM_CALLS,"surface::create");
}

// Clear data array
void surface::clear()
{
	dbg_begin(DM_CALLS,"surface::clear");
	datafull = FALSE;
	min = 10000.0;
	max = -10000.0;
	cutoff = 0.0;
	currentpoint.zero();
	visible = TRUE;
	if (data == NULL) return;
	int i, j;
	for (i = 0; i<npoints.y; i++)
	{
		for (j = 0; j<npoints.z; j++) delete[] data[i][j];
		delete[] data[i];
	}
	delete[] data;
	dbg_end(DM_CALLS,"surface::clear");
}

// Set grid extent (and data[])
void surface::set_npoints(vec3<int> v)
{
	dbg_begin(DM_CALLS,"surface::set_npoints");
	npoints = v;
	log ++;
	create();
	dbg_end(DM_CALLS,"surface::set_npoints");
}

// Update minimum / maximum based on supplied value
void surface::set_limits(double d)
{
	if (d < min) min = d;
	else if (d > max) max = d;
}

// Set specific point in data array
void surface::set_data(int x, int y, int z, double d)
{
	// Check limits against npoints vector
	if ((x < 1) || (x > npoints.x))
	{
		msg(DM_NONE,"surface::set_data(x,y,z) - X index is outside array bounds.\n");
		return;
	}
	else if ((y < 1) || (y > npoints.y))
	{
		msg(DM_NONE,"surface::set_data(x,y,z) - Y index is outside array bounds.\n");
		return;
	}
	else if ((z < 1) || (z > npoints.z))
	{
		msg(DM_NONE,"surface::set_data(x,y,z) - Z index is outside array bounds.\n");
		return;
	}
	// Okay, so store data
	data[x][y][z] = d;
	// Set new minimum / maximum
	set_limits(d);
}

// Set 'next' point in data array
void surface::set_next_data(double d)
{
	// Check limit
	if (datafull == TRUE)
	{
		msg(DM_NONE,"surface::set_next_data - Array already full.\n");
		return;
	}
	// Set current point referenced by currentpoint
	data[currentpoint.x][currentpoint.y][currentpoint.z] = d;
	// Increase currentpoint
	currentpoint.x ++;
	if (currentpoint.x == npoints.x)
	{
		currentpoint.x = 0;
		currentpoint.y ++;
		if (currentpoint.y == npoints.y)
		{
			currentpoint.y = 0;
			currentpoint.z ++;
			if (currentpoint.z == npoints.z) datafull = TRUE;
		}
	}
	// Set new minimum / maximum
	set_limits(d);
}

