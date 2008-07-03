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
#include "model/model.h"

// MC ComponentRegions
const char *RegionShapeKeywords[ComponentRegion::nRegionShapes] = { "Cell", "Cuboid", "Spheroid", "Cylinder" };
const char *ComponentRegion::regionShape(ComponentRegion::RegionShape i)
{
	return RegionShapeKeywords[i];
}
ComponentRegion::RegionShape ComponentRegion::regionShape(const char *s)
{
	return (ComponentRegion::RegionShape) enumSearch("region shape", ComponentRegion::nRegionShapes, RegionShapeKeywords, s);
}

// Constructor
ComponentRegion::ComponentRegion()
{
	// Private variables
	shape_ = ComponentRegion::WholeCell;
	centre_.zero();
	allowOverlap_ = TRUE;
	size_.set(5.0,5.0,5.0);
	length_ = 5.0;
	centreFrac_ = FALSE;
	sizeFrac_ = FALSE;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Sets the shape of the ComponentRegion for the component
void ComponentRegion::setShape(ComponentRegion::RegionShape r)
{
	shape_ = r;
}

// Returns the ComponentRegion defined for the component
ComponentRegion::RegionShape ComponentRegion::shape()
{
	return shape_;
}

// Sets the centre of the defined ComponentRegion
void ComponentRegion::setCentre(Vec3<double> v)
{
	centre_ = v;
	centreFrac_ = FALSE;
}

// Sets the centre of the defined ComponentRegion in fractional coordinates
void ComponentRegion::setCentreFrac(Vec3<double> v)
{
	centre_ = v;
	centreFrac_ = TRUE;
}

// Returns the centre of the defined ComponentRegion
Vec3<double> ComponentRegion::centre()
{
	return centre_;
}

// Returns whether the centre was set in real or fractional coordinates
bool ComponentRegion::isCentreFrac()
{
	return centreFrac_;
}

// Sets the size_.of the defined ComponentRegion
void ComponentRegion::setSize(Vec3<double> v)
{
	size_ = v;
	sizeFrac_ = FALSE;
}

// Sets the size of the defined ComponentRegion in fractional coordinates
void ComponentRegion::setSizeFrac(Vec3<double> v)
{
	size_ = v;
	sizeFrac_ = TRUE;
}

// Returns the size_.of the defined ComponentRegion
Vec3<double> ComponentRegion::size()
{
	return size_;
}

// Returns whether the size of the region was set in real or fractional coordinates
bool ComponentRegion::isSizeFrac()
{
	return sizeFrac_;
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
bool ComponentRegion::checkOverlap(const Vec3<double> &v, Cell *cell, Reflist<Model,int> &components)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	msg.enter("ComponentRegion::checkOverlap");
	static Vec3<double> tempv;
	bool result = FALSE;
	ComponentRegion *r;
	Refitem<Model,int> *ri;
	for (ri = components.first(); ri != NULL; ri = ri->next)
	{
		r = &ri->item->area;
		if (r == this) continue;
		if (r->checkCoords(v,cell)) result = TRUE;
		//printf("Overlap with region '%s' is %s.\n",text_from_RS(r->get_shape()),(result ? "TRUE" : "FALSE"));
		if (result) break;
	}
	msg.exit("ComponentRegion::checkOverlap");
	return result;
}

// Check that specified coordinates are inside region
bool ComponentRegion::checkCoords(const Vec3<double> &v, Cell *cell)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	msg.enter("ComponentRegion::checkCoords");
	static Vec3<double> tempv, realsize;
	bool result = TRUE;
	switch (shape_)
	{
		case (ComponentRegion::WholeCell):
			break;
		case (ComponentRegion::CuboidRegion):
			tempv = v - (centreFrac_ ? cell->fracToReal(centre_) : centre_);
			realsize = (sizeFrac_ ? cell->fracToReal(size_) : size_);
			if (fabs(tempv.x) > 0.5*realsize.x) result = FALSE;
			else if (fabs(tempv.y) > 0.5*realsize.y) result = FALSE;
			else if (fabs(tempv.z) > 0.5*realsize.z) result = FALSE;
			break;
		case (ComponentRegion::SpheroidRegion):
			tempv = v - (centreFrac_ ? cell->fracToReal(centre_) : centre_);
			// Scale test point by spheroid size
			tempv /= (sizeFrac_ ? cell->fracToReal(size_) : size_);
			if (tempv.magnitude() > 1.0) result = FALSE;
			break;
		case (ComponentRegion::CylinderRegion):
			printf("ComponentRegion::checkCoords - Not done yet for this type.\n");
			break;
	}
	msg.exit("ComponentRegion::checkCoords");
	return result;
}

// Random coordinate in region
Vec3<double> ComponentRegion::randomCoords(Cell *cell, Reflist<Model,int> &components)
{
	msg.enter("ComponentRegion::randomCoords");
	static Vec3<double> v, tempv, realsize;
	static int nAttempts;
	bool done = FALSE;
	nAttempts = 0;
	// Generate random coords inside this region...
	do
	{
		// Increment the attempts counter
		nAttempts ++;
		switch (shape_)
		{
			case (ComponentRegion::WholeCell):
				v.x = csRandom();
				v.y = csRandom();
				v.z = csRandom();
				v *= cell->transpose();
				break;
			case (ComponentRegion::CuboidRegion):
				v = sizeFrac_ ? cell->fracToReal(size_) : size_;
				v.x *= csRandom() - 0.5;
				v.y *= csRandom() - 0.5;
				v.z *= csRandom() - 0.5;
				v += centreFrac_ ? cell->fracToReal(centre_) : centre_;
				break;
			case (ComponentRegion::SpheroidRegion):
				//tempv.set(csRandom(),(csRandom()-0.5)*PI,(csRandom()-0.5)*PI);
				tempv.set(csRandom(),(csRandom()*2.0-1.0)*PI,(csRandom()-0.5)*PI);
				realsize = (sizeFrac_ ? cell->fracToReal(size_) : size_);
				v.x = tempv.x * sin(tempv.y) * cos(tempv.z) * size_.x;
				v.y = tempv.x * sin(tempv.y) * sin(tempv.z) * size_.y;
				v.z = tempv.x * cos(tempv.y)                * size_.z;
				v += centreFrac_ ? cell->fracToReal(centre_) : centre_;
				break;
			case (ComponentRegion::CylinderRegion):
				printf("ComponentRegion::randomCoords - Cylinder moves not implemented yet...\n");
				break;
		}
		// Now, check that this random coordinate doesn't overlap with others (if this is required)
		if (!allowOverlap_)
		{
			if (!checkOverlap(v,cell,components)) done = TRUE;
		}
		else done = TRUE;
		if ((!done) && (nAttempts == 100))
		{
			printf("Failed to find position in region '%s' that doesn't overlap within %i trials.\n", ComponentRegion::regionShape(shape_), 100);
			done = TRUE;
		}
	} while (!done);
	msg.exit("ComponentRegion::randomCoords");
	return v;
}
