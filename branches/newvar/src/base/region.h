/*
	** Monte Carlo Region
	*** src/base/region.h
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
	// Size of the ComponentRegion
	Vec3<double> size_;
	// Whether the region size is defined in fractional coordinates
	bool sizeFrac_;
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
	// Sets the size of the defined ComponentRegion
	void setSize(Vec3<double> v);
	// Sets the size of the defined ComponentRegion in fractional coordinates
	void setSizeFrac(Vec3<double> v);
	// Returns the size of the defined ComponentRegion
	Vec3<double> size();
	// Returns whether the size of the region was set in real or fractional coordinates
	bool isSizeFrac();
	// Sets the length of the ComponentRegion (for some ComponentRegion types)
	void setLength(double v);
	// Returns the ComponentRegion length
	double length();
	// Sets whether to allow overlap with other ComponentRegions
	void setAllowOverlap(bool b);
	// Returns whether to allow overlap over other ComponentRegions when inserting
	bool allowOverlap();
	// Determines whether the supplied coordinates are within the ComponentRegion defined
	bool coordsInRegion(const Vec3<double>&, Cell*);
	// Determines whether the supplied coordinates overlap any of the other ComponentRegions supplied
	bool pointOverlaps(const Vec3<double>&, Cell*, Reflist<Model,int> &components);
	// Generate a random coordinate inside the ComponentRegion
	Vec3<double> randomCoords(Cell*, Reflist<Model,int> &components);
};

#endif
