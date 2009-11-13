/*
	** Monte Carlo region
	*** src/base/region.cpp
	Copyright T. Youngs 2007-2009

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

#include "base/region.h"
#include "base/cell.h"
#include "base/sysfunc.h"
#include "model/model.h"

// MC ComponentRegions
const char *RegionShapeKeywords[ComponentRegion::nRegionShapes] = { "Cell", "Cuboid", "Spheroid", "Cylinder" };
const char *ComponentRegion::regionShape(ComponentRegion::RegionShape i)
{
	return RegionShapeKeywords[i];
}
ComponentRegion::RegionShape ComponentRegion::regionShape(const char *s, bool reporterror)
{
	ComponentRegion::RegionShape cr = (ComponentRegion::RegionShape) enumSearch("region shape", ComponentRegion::nRegionShapes, RegionShapeKeywords, s);
	if ((cr == ComponentRegion::nRegionShapes) && reporterror) enumPrintValid(ComponentRegion::nRegionShapes,RegionShapeKeywords);
	return cr;
}

// Constructor
ComponentRegion::ComponentRegion()
{
	// Private variables
	parent_ = NULL;
	shape_ = ComponentRegion::WholeCell;
	centre_.zero();
	allowOverlap_ = TRUE;
	geometry_.set(5.0,5.0,5.0);
	rotations_.zero();
	centreFrac_ = FALSE;
	geometryFrac_ = FALSE;
	rotateRegion_ = TRUE;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set parent model
void ComponentRegion::setParent(Model *m)
{
	parent_ = m;
}

// Return parent model
Model *ComponentRegion::parent()
{
	return parent_;
}

// Sets the shape of the region for the component
void ComponentRegion::setShape(ComponentRegion::RegionShape r)
{
	shape_ = r;
}

// Returns the region defined for the component
ComponentRegion::RegionShape ComponentRegion::shape()
{
	return shape_;
}

// Sets the centre of the defined region
void ComponentRegion::setCentre(Vec3<double> v)
{
	centre_ = v;
	centreFrac_ = FALSE;
}

// Sets the centre of the defined region in fractional coordinates
void ComponentRegion::setCentreFrac(Vec3<double> v)
{
	centre_ = v;
	centreFrac_ = TRUE;
}

// Returns the centre of the defined region
Vec3<double> ComponentRegion::centre()
{
	return centre_;
}

// Returns whether the centre was set in real or fractional coordinates
bool ComponentRegion::isCentreFrac()
{
	return centreFrac_;
}

// Sets the geometry of the defined region
void ComponentRegion::setGeometry(Vec3<double> v)
{
	geometry_ = v;
	geometryFrac_ = FALSE;
}

// Sets the geometry of the defined region in fractional coordinates
void ComponentRegion::setGeometryFrac(Vec3<double> v)
{
	geometry_ = v;
	geometryFrac_ = TRUE;
}

// Returns the geometry of the defined region
Vec3<double> ComponentRegion::geometry()
{
	return geometry_;
}

// Returns whether the geometry of the region was set in real or fractional coordinates
bool ComponentRegion::isGeometryFrac()
{
	return geometryFrac_;
}

// Set the rotations for this region
void ComponentRegion::setRotations(Vec3<double> v)
{
	rotations_ = v;
	// Recalculate rotation matrices
	rotationMatrix_.createRotationXY( rotations_.x, rotations_.y );
	inverseRotationMatrix_ = rotationMatrix_;
	inverseRotationMatrix_.invert();
}

// Returns the rotations of the defined region
Vec3<double> ComponentRegion::rotations()
{
	return rotations_;
}

// Returns whether the region should be rotated
bool ComponentRegion::rotateRegion()
{
	return rotateRegion_;
}

// Set whether the region should be rotated
void ComponentRegion::setRotateRegion(bool b)
{
	rotateRegion_ = b;
}

// Sets whether to allow overlap with other regions
void ComponentRegion::setAllowOverlap(bool b)
{
	allowOverlap_ = b;
}

// Returns whether to allow overlap over other regions when inserting
bool ComponentRegion::allowOverlap()
{
	return allowOverlap_;
}

// Overlap check
bool ComponentRegion::pointOverlaps(const Vec3<double> &v, Cell *cell, Reflist<Model,int> &components)
{
	// Check whether the supplied coordinates overlap with other regions in the list bar this one
	msg.enter("ComponentRegion::pointOverlaps");
	static Vec3<double> tempv;
	bool result = FALSE;
	ComponentRegion *r;
	Refitem<Model,int> *ri;
// 	printf("Number of components in list is %i\n",components.nItems());
	for (ri = components.first(); ri != NULL; ri = ri->next)
	{
		r = ri->item->region();
		if (r == this) continue;
		if (r->coordsInRegion(v,cell)) result = TRUE;
// 		printf("Overlap of region '%s' with region '%s' is %i.\n", regionShape(shape_), regionShape(r->shape()), r->coordsInRegion(v,cell));
		if (result) break;
	}
	msg.exit("ComponentRegion::pointOverlaps");
	return result;
}

// Check that specified coordinates are inside this region
bool ComponentRegion::coordsInRegion(const Vec3<double> &v, Cell *cell)
{
	msg.enter("ComponentRegion::coordsInRegion");
	if (shape_ == ComponentRegion::WholeCell)
	{
		msg.exit("ComponentRegion::coordsInRegion");
		return FALSE;
	}
	static Vec3<double> tempv, realgeometry;
	static Mat3<double> rot;
	bool result = TRUE;
	// Get position of point relative to centre of region
	tempv = v - (centreFrac_ ? cell->fracToReal(centre_) : centre_);
	switch (shape_)
	{
		case (ComponentRegion::CuboidRegion):
			if (rotateRegion_) tempv *= inverseRotationMatrix_;
			realgeometry = (geometryFrac_ ? cell->fracToReal(geometry_) : geometry_);
			if (fabs(tempv.x) > 0.5*realgeometry.x) result = FALSE;
			else if (fabs(tempv.y) > 0.5*realgeometry.y) result = FALSE;
			else if (fabs(tempv.z) > 0.5*realgeometry.z) result = FALSE;
			break;
		case (ComponentRegion::SpheroidRegion):
			if (rotateRegion_) tempv *= inverseRotationMatrix_;
			// Scale test point by spheroid size
			tempv /= (geometryFrac_ ? cell->fracToReal(geometry_) : geometry_);
			if (tempv.magnitude() > 1.0) result = FALSE;
			break;
		case (ComponentRegion::CylinderRegion):
			// We rotate into the frame of the cylinder, so we can do line distance along 0,0,1
			if (rotateRegion_) tempv *= inverseRotationMatrix_;
			realgeometry = (geometryFrac_ ? cell->fracToReal(geometry_) : geometry_);
			// 'Normalise' coordinate w.r.t. cylinder X/Y radii
			tempv.x = tempv.x / realgeometry.x;
			tempv.y = tempv.y / realgeometry.y;
			// See: A Programmers Geometry, Bowyer and Woodwark, Butterworths (pub.), 1983, p99
			// Since the point 'tempv' is already relative to the centre, and since the cylinder is along 0,0,1, method is simplified...
			// Check z coordinate first....
			if (fabs(tempv.z) > 0.5*realgeometry.z) result = FALSE;
			else if ((tempv.x*tempv.x + tempv.y*tempv.y) > 1.0) result = FALSE;
			break;
		default:
			printf("ComponentRegion::coordsInRegion - Not done yet for this type.\n");
			break;
	}
	msg.exit("ComponentRegion::coordsInRegion");
	return result;
}

// Random coordinate in region
Vec3<double> ComponentRegion::randomCoords(Cell *cell, Reflist<Model,int> &components)
{
	msg.enter("ComponentRegion::randomCoords");
	static Vec3<double> v, tempv, geometry;
	static int nAttempts;
	double x;
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
				v = geometryFrac_ ? cell->fracToReal(geometry_) : geometry_;
				v.x *= csRandom() - 0.5;
				v.y *= csRandom() - 0.5;
				v.z *= csRandom() - 0.5;
				if (rotateRegion_) v *= rotationMatrix_;
				v += centreFrac_ ? cell->fracToReal(centre_) : centre_;
				break;
			case (ComponentRegion::SpheroidRegion):
				geometry = (geometryFrac_ ? cell->fracToReal(geometry_) : geometry_);
				do
				{
					v.x = csRandom() * 2.0 - 1.0;
					v.y = csRandom() * 2.0 - 1.0;
					v.z = csRandom() * 2.0 - 1.0;
				} while ( (v.x*v.x + v.y*v.y + v.z*v.z) > 1.0);
				v.multiply(geometry);
				if (rotateRegion_) v *= rotationMatrix_;
				v += centreFrac_ ? cell->fracToReal(centre_) : centre_;
				break;
			case (ComponentRegion::CylinderRegion):
				geometry = (geometryFrac_ ? cell->fracToReal(geometry_) : geometry_);
				do
				{
					v.x = csRandom() * 2.0 - 1.0;
					v.y = csRandom() * 2.0 - 1.0;
				} while ( (v.x*v.x + v.y*v.y) > 1.0);
				v.z = (csRandom()-0.5);
				v.multiply(geometry);
				if (rotateRegion_) v *= rotationMatrix_;
				v += centreFrac_ ? cell->fracToReal(centre_) : centre_;
				break;
			default:
				msg.print("ComponentRegion::randomCoords - Move not yet implemented.\n");
				break;
		}
		// Now, check that this random coordinate doesn't overlap with others (if this is required)
		if ((!allowOverlap_) && (pointOverlaps(v,cell,components))) done = FALSE;
		else done = TRUE;
		if ((!done) && (nAttempts == 100))
		{
			msg.print("Failed to find position in region '%s' that doesn't overlap with others within %i trials.\n", ComponentRegion::regionShape(shape_), 100);
			done = TRUE;
		}
	} while (!done);
	msg.exit("ComponentRegion::randomCoords");
	return v;
}
