/*
	** Monte Carlo region
	*** src/classes/region.h
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

#ifndef H_REGION_H
#define H_REGION_H

#include "templates/vector3.h"
#include "templates/reflist.h"

// Region Shapes
enum region_shape { RS_CELL, RS_CUBOID, RS_SPHEROID, RS_CYLINDER, RS_NITEMS };
const char *text_from_RS(region_shape);
const char **get_RS_strings();
region_shape RS_from_text(const char*);

// Forward Declarations
class unitcell;
class component;

// Region
class region
{
	public:
	// Constructor / Destructor
	region();
	~region();
	// List pointers
	region *prev, *next;

	private:
	// Type of region the component is limited to
	region_shape shape;
	// Coordinates of the centre of any defined region
	vec3<double> centre;
	// Whether 'centre' should be set to the box centre automatically
	bool usecellcentre;
	// Size of the region
	vec3<double> size;
	// Length of cylindrical region
	double length;
	// Whether to allow overlap with other regions, or to avoid them
	bool allowoverlap;

	public:
	// Sets the shape of the region for the component
	void set_shape(region_shape r) { shape = r; }
	// Returns the region defined for the component
	region_shape get_shape() { return shape; }
	// Sets the centre of the defined region
	void set_centre(vec3<double> v) { centre = v; }
	// Returns the centre of the defined region
	vec3<double> get_centre() { return centre; }
	// Sets the size of the defined region
	void set_size(vec3<double> v) { size = v; }
	// Returns the size of the defined region
	vec3<double> get_size() { return size; }
	// Sets whether to use the cell centre as the region centre
	void set_usecellcentre(bool b) { usecellcentre = b; }
	// Returns whether to use the cell centre as the region centre
	bool get_usecellcentre() { return usecellcentre; }
	// Sets the length of the region (for some region types)
	void set_length(double v) { length = v; }
	// Returns the region length
	double get_length() {return length; }
	// Sets whether to allow overlap with other regions
	void set_allowoverlap(bool b) { allowoverlap = b; }
	// Returns whether to allow overlap over other regions when inserting
	bool get_allowoverlap() { return allowoverlap; }
	// Determines whether the supplied coordinates are within the region defined
	bool check_coords(const vec3<double>&, unitcell*);
	// Determines whether the supplied coordinates overlap any of the other regions supplied
	bool check_overlap(const vec3<double>&, unitcell*, component*);
	// Generate a random coordinate inside the region
	vec3<double> random_coords(unitcell*, component*);
};

#endif
