/*
	** Monte Carlo Region
	*** src/base/region.h
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

#ifndef ATEN_REGION_H
#define ATEN_REGION_H

#include "templates/vector3.h"
#include "templates/reflist.h"

// Forward Declarations
class Cell;
class Component;
class Model;

// ComponentRegion
class ComponentRegion
{
	public:
	// Constructor
	ComponentRegion();
	// List pointers
	ComponentRegion *prev, *next;
	// ComponentRegion Shapes
	enum RegionShape { WholeCell, CuboidRegion, SpheroidRegion, CylinderRegion, nRegionShapes };
	static const char *regionShape(RegionShape);
	static RegionShape regionShape(const char*);

	private:
	// Type of ComponentRegion the component is limited to
	RegionShape shape_;
	// Coordinates of the centre of any defined ComponentRegion
	Vec3<double> centre_;
	// Whether the region centre is defined in fractional coordinates
	bool centreFrac_;
	// Geometry of the ComponentRegion
	Vec3<double> geometry_;
	// Whether the region size is defined in fractional coordinates
	bool geometryFrac_;
	// Rotations of the region
	Vec3<double> rotations_;
	// Whether the region should be rotated
	bool rotateRegion_;
	// Rotation matrix for region (f the region is to be rotated)
	Mat3<double> rotationMatrix_;
	// Inverse rotation matrix for region (f the region is to be rotated)
	Mat3<double> inverseRotationMatrix_;
	// Length of cylindrical ComponentRegion
	double length_;
	// Whether to allow overlap with other ComponentRegions, or to avoid them
	bool allowOverlap_;

	public:
	// Sets the shape of the ComponentRegion for the component
	void setShape(RegionShape r);
	// Returns the ComponentRegion defined for the component
	RegionShape shape();
	// Sets the centre of the defined ComponentRegion
	void setCentre(Vec3<double> v);
	// Sets the centre of the defined ComponentRegion in fractional coordinates
	void setCentreFrac(Vec3<double> v);
	// Returns the centre of the defined ComponentRegion (fractional or real)
	Vec3<double> centre();
	// Returns whether the centre was set in real or fractional coordinates
	bool isCentreFrac();
	// Sets the geometry of the defined ComponentRegion
	void setGeometry(Vec3<double> v);
	// Sets the geometry of the defined ComponentRegion in fractional coordinates
	void setGeometryFrac(Vec3<double> v);
	// Returns the geometry of the defined ComponentRegion
	Vec3<double> geometry();
	// Returns whether the size of the region was set in real or fractional coordinates
	bool isGeometryFrac();
	// Sets the rotations of the defined ComponentRegion
	void setRotations(Vec3<double> v);
	// Returns the rotations of the defined ComponentRegion
	Vec3<double> rotations();
	// Return whether to rotate the region
	bool rotateRegion();
	// Set wheter to rotate the region
	void setRotateRegion(bool b);
	// Sets whether to allow overlap with other ComponentRegions
	void setAllowOverlap(bool b);
	// Returns whether to allow overlap over other ComponentRegions when inserting
	bool allowOverlap();
	// Determines whether the supplied coordinates are within the ComponentRegion defined
	bool coordsInRegion(const Vec3<double> &point, Cell *cell);
	// Determines whether the supplied coordinates overlap any of the other ComponentRegions supplied
	bool pointOverlaps(const Vec3<double> &point, Cell *cell, Reflist<Model,int> &components);
	// Generate a random coordinate inside the ComponentRegion
	Vec3<double> randomCoords(Cell *cell, Reflist<Model,int> &components);
};

#endif
