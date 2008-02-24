/*
	** Monte Carlo region
	*** src/classes/region.cpp
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

#include "classes/region.h"
#include "classes/cell.h"
#include "templates/reflist.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include "methods/mc.h"

// MC Regions
const char *RS_strings[RS_NITEMS] = { "Cell", "Cuboid", "Spheroid", "Cylinder" };
const char *text_from_RS(region_shape i)
	{ return RS_strings[i]; }
region_shape RS_from_text(const char *s)
	{ return (region_shape) enum_search("region shape",RS_NITEMS,RS_strings,s); }
const char **get_RS_strings()
	{ return RS_strings; }

// Constructor
region::region()
{
	shape = RS_CELL;
	centre.zero();
	allowoverlap = TRUE;
	size.set(5.0,5.0,5.0);
	length = 5.0;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_REGION] ++;
	#endif
}

// Destructor
region::~region()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_REGION] ++;
	#endif
}

// Overlap check
bool region::check_overlap(const vec3<double> &v, unitcell *cell, component *firstc)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	dbg_begin(DM_CALLS,"region::check_overlap");
	static vec3<double> tempv;
	bool result = FALSE;
	region *r;
	for (component *c = firstc; c != NULL; c = c->next)
	{
		r = &c->area;
		if (r == this) continue;
		if (r->check_coords(v,cell)) result = TRUE;
		//printf("Overlap with region '%s' is %s.\n",text_from_RS(r->get_shape()),(result ? "TRUE" : "FALSE"));
		if (result) break;
	}
	dbg_end(DM_CALLS,"region::check_overlap");
	return result;
}

// Check that specified coordinates are inside region
bool region::check_coords(const vec3<double> &v, unitcell *cell)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	dbg_begin(DM_CALLS,"region::check_coords");
	static vec3<double> tempv;
	bool result = TRUE;
	switch (shape)
	{
		case (RS_CELL):
			break;
		case (RS_CUBOID):
			tempv = v - centre;
			if (fabs(tempv.x) > 0.5*size.x) result = FALSE;
			else if (fabs(tempv.y) > 0.5*size.y) result = FALSE;
			else if (fabs(tempv.z) > 0.5*size.z) result = FALSE;
			break;
		case (RS_SPHEROID):
			tempv = v - centre;
			// Scale test point by spheroid size
			tempv /= size;
			if (tempv.magnitude() > 1.0) result = FALSE;
			break;
		case (RS_CYLINDER):
			printf("region::check_coords - Not done yet for this type.\n");
			break;
	}
	dbg_end(DM_CALLS,"region::check_coords");
	return result;
}

// Random coordinate in region
vec3<double> region::random_coords(unitcell *cell, component *c)
{
	dbg_begin(DM_CALLS,"region::random_coords");
	static vec3<double> v, tempv;
	static int nattempts;
	bool done = FALSE;
	nattempts = 0;
	// Generate random coords inside this region...
	do
	{
		// Increment the attempts counter
		nattempts ++;
		switch (shape)
		{
			case (RS_CELL):
				v.x = cs_random();
				v.y = cs_random();
				v.z = cs_random();
				v *= cell->get_transpose();
				break;
			case (RS_CUBOID):
				v = size;
				v.x *= cs_random() - 0.5;
				v.y *= cs_random() - 0.5;
				v.z *= cs_random() - 0.5;
				v += centre;
				break;
			case (RS_SPHEROID):
				//tempv.set(cs_random(),(cs_random()-0.5)*PI,(cs_random()-0.5)*PI);
				tempv.set(cs_random(),(cs_random()*2.0-1.0)*PI,(cs_random()-0.5)*PI);
				v.x = tempv.x * sin(tempv.y) * cos(tempv.z) * size.x;
				v.y = tempv.x * sin(tempv.y) * sin(tempv.z) * size.y;
				v.z = tempv.x * cos(tempv.y)                * size.z;
				v += centre;
				break;
			case (RS_CYLINDER):
				printf("region::random_coords - Cylinder moves not done yet...\n");
				break;
		}
		// Now, check that this random coordinate doesn't overlap with others (if this is required)
		if (!allowoverlap)
		{
			if (!check_overlap(v,cell,c)) done = TRUE;
		}
		else done = TRUE;
		if ((!done) && (nattempts == 100))
		{
			printf("Failed to find position in region '%s' that doesn't overlap within %i trials.\n", text_from_RS(shape), 100);
			done = TRUE;
		}
	} while (!done);
	dbg_end(DM_CALLS,"region::random_coords");
	return v;
}
