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

// MC ComponentRegions
const char *RS_strings[RS_NITEMS] = { "Cell", "Cuboid", "Spheroid", "Cylinder" };
const char *text_from_RS(ComponentRegionShape i)
	{ return RS_strings[i]; }
ComponentRegionShape RS_from_text(const char *s)
	{ return (ComponentRegionShape) enumSearch("region shape",RS_NITEMS,RS_strings,s); }
const char **get_RS_strings()
	{ return RS_strings; }

// Constructor
ComponentRegion::ComponentRegion()
{
	// Private variables
	shape_ = RS_CELL;
	centre_.zero();
	allowOverlap_ = TRUE;
	size_.set(5.0,5.0,5.0);
	length_ = 5.0;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Sets the shape of the ComponentRegion for the component
void ComponentRegion::setShape(ComponentRegionShape r)
{
	shape_ = r;
}

// Returns the ComponentRegion defined for the component
ComponentRegionShape ComponentRegion::shape()
{
	return shape_;
}

// Sets the centre of the defined ComponentRegion
void ComponentRegion::setCentre(Vec3<double> v)
{
	centre_ = v;
}

// Returns the centre of the defined ComponentRegion
Vec3<double> ComponentRegion::centre()
{
	return centre_;
}

// Sets the size_.of the defined ComponentRegion
void ComponentRegion::setSize(Vec3<double> v)
{
	size_ = v;
}

// Returns the size_.of the defined ComponentRegion
Vec3<double> ComponentRegion::size()
{
	return size_;
}

// Sets the length of the ComponentRegion (for some ComponentRegion types)
void ComponentRegion::setLength(double v)
{
	length_ = v;
}

// Returns the ComponentRegion length
double ComponentRegion::length()
{
	return length_;
}

// Sets whether to allow overlap with other ComponentRegions
void ComponentRegion::setAllowOverlap(bool b)
{
	allowOverlap_ = b;
}

// Returns whether to allow overlap over other ComponentRegions when inserting
bool ComponentRegion::allowOverlap()
{
	return allowOverlap_;
}

// Overlap check
bool ComponentRegion::checkOverlap(const Vec3<double> &v, Cell *cell, Component *firstc)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	dbgBegin(DM_CALLS,"ComponentRegion::checkOverlap");
	static Vec3<double> tempv;
	bool result = FALSE;
	ComponentRegion *r;
	for (Component *c = firstc; c != NULL; c = c->next)
	{
		r = &c->area;
		if (r == this) continue;
		if (r->checkCoords(v,cell)) result = TRUE;
		//printf("Overlap with region '%s' is %s.\n",text_from_RS(r->get_shape()),(result ? "TRUE" : "FALSE"));
		if (result) break;
	}
	dbgEnd(DM_CALLS,"ComponentRegion::checkOverlap");
	return result;
}

// Check that specified coordinates are inside region
bool ComponentRegion::checkCoords(const Vec3<double> &v, Cell *cell)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	dbgBegin(DM_CALLS,"ComponentRegion::checkCoords");
	static Vec3<double> tempv;
	bool result = TRUE;
	switch (shape_)
	{
		case (RS_CELL):
			break;
		case (RS_CUBOID):
			tempv = v - centre_;
			if (fabs(tempv.x) > 0.5*size_.x) result = FALSE;
			else if (fabs(tempv.y) > 0.5*size_.y) result = FALSE;
			else if (fabs(tempv.z) > 0.5*size_.z) result = FALSE;
			break;
		case (RS_SPHEROID):
			tempv = v - centre_;
			// Scale test point by spheroid size
			tempv /= size_;
			if (tempv.magnitude() > 1.0) result = FALSE;
			break;
		case (RS_CYLINDER):
			printf("ComponentRegion::checkCoords - Not done yet for this type.\n");
			break;
	}
	dbgEnd(DM_CALLS,"ComponentRegion::checkCoords");
	return result;
}

// Random coordinate in region
Vec3<double> ComponentRegion::randomCoords(Cell *cell, Component *c)
{
	dbgBegin(DM_CALLS,"ComponentRegion::randomCoords");
	static Vec3<double> v, tempv;
	static int nattempts;
	bool done = FALSE;
	nattempts = 0;
	// Generate random coords inside this region...
	do
	{
		// Increment the attempts counter
		nattempts ++;
		switch (shape_)
		{
			case (RS_CELL):
				v.x = csRandom();
				v.y = csRandom();
				v.z = csRandom();
				v *= cell->transpose();
				break;
			case (RS_CUBOID):
				v = size_;
				v.x *= csRandom() - 0.5;
				v.y *= csRandom() - 0.5;
				v.z *= csRandom() - 0.5;
				v += centre_;
				break;
			case (RS_SPHEROID):
				//tempv.set(csRandom(),(csRandom()-0.5)*PI,(csRandom()-0.5)*PI);
				tempv.set(csRandom(),(csRandom()*2.0-1.0)*PI,(csRandom()-0.5)*PI);
				v.x = tempv.x * sin(tempv.y) * cos(tempv.z) * size_.x;
				v.y = tempv.x * sin(tempv.y) * sin(tempv.z) * size_.y;
				v.z = tempv.x * cos(tempv.y)                * size_.z;
				v += centre_;
				break;
			case (RS_CYLINDER):
				printf("ComponentRegion::randomCoords - Cylinder moves not implemented yet...\n");
				break;
		}
		// Now, check that this random coordinate doesn't overlap with others (if this is required)
		if (!allowOverlap_)
		{
			if (!checkOverlap(v,cell,c)) done = TRUE;
		}
		else done = TRUE;
		if ((!done) && (nattempts == 100))
		{
			printf("Failed to find position in region '%s' that doesn't overlap within %i trials.\n", text_from_RS(shape_), 100);
			done = TRUE;
		}
	} while (!done);
	dbgEnd(DM_CALLS,"ComponentRegion::randomCoords");
	return v;
}
