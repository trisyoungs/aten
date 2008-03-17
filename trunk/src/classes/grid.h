/*
	*** Grid data structure
	*** src/classes/grid.h
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

#ifndef ATEN_GRID_H
#define ATEN_GRID_H

#include "templates/vector3.h"
#include "classes/dnchar.h"
#include "classes/cell.h"
#include "base/constants.h"
#include "base/debug.h"
#ifdef IS_MAC
	#include <OpenGL/gl.h>
#else
	#include <GL/gl.h>
#endif

// Surface rendering styles
enum surface_style { SS_GRID, SS_POINTS, SS_TRIANGLES, SS_SOLID };

// 3D Grid Class
class grid
{
	public:
	// Constructor / Destructor
	grid();
	~grid();
	// List pointers
	grid *prev, *next;

	/*
	// Identity
	*/
	private:
	// Name of the grid data
	dnchar name;
	// Log for changes to grid, display style etc.
	int log;

	public:
	// Set name of grid data
	void set_name(const char *s) { name = s; }
	// Return name of grid data
	const char *get_name() { return name.get(); }

	/*
	// Gridded Data
	*/
	private:
	// Cell that determine spacing between gridpoints and their axis system
	unitcell cell;
	// Surface origin
	vec3<double> origin;
	// Number of points in each direction
	vec3<int> npoints;
	// Voxel values
	double ***data;
	// Clear voxel data
	void clear();
	// Create voxel data
	void create();
	// Cutoff for isosurface generation
	double cutoff;
	// Minimum and maximum values stored in data[]
	double min, max;
	// Update minimum and maximum values
	void set_limits(double d);
	// Order of loops when reading data point-by-point
	vec3<int> looporder;

	public:
	// Set spacing for a cubic grid
	void set_axes(double r);
	// Set spacing for an orthorhombic grid
	void set_axes(const vec3<double> v);
	// Set spacing for a parallelepiped grid
	void set_axes(const mat3<double> m);
	// Return the grid axes
	mat3<double> get_axes() { return cell.get_axes(); }
	// Return lengths of cell axiss
	vec3<double> get_lengths() { return cell.get_lengths(); }
	// Set data origin
	void set_origin(const vec3<double> v) { origin = v; log++; }
	// Return the origin of the grid data
	vec3<double> get_origin() { return origin; }
	// Set number of points in data series (creates data[])
	void set_npoints(vec3<int>);
	// Return number of points in data series
	vec3<int> get_npoints() { return npoints; }
	// Return minimum value in data[]
	double get_minimum() { return min; }
	// Return maximum value in data[]
	double get_maximum() { return max; }
	// Set isovalue cutoff for surface
	void set_cutoff(double d) { cutoff = d; log++; }
	// Return isovalue cutoff for surface
	double get_cutoff() { return cutoff; }
	// Return data array
	double ***get_data() { return data; }
	// Set loop ordering
	void set_looporder(int n, int xyz) { looporder.set(n,xyz); }

	/*
	// Data Interface
	*/
	private:
	// Count variable used by set_next_point()
	vec3<int> currentpoint;
	// Whether enough points have been added
	bool datafull;

	public:
	// Set specific point in data array
	void set_data(int x, int y, int z, double d);
	// Set 'next' point in data array
	void set_next_data(double d);
	
	/*
	// Visuals
	*/
	private:
	// GL display list of rendered surface
	GLuint displaylist;
	// Log point corresponding to the last render
	int render_point;
	// Whether the surface is currently visible
	bool visible;
	// How to render this surface
	surface_style style;
	// Colour (including alpha component)
	GLfloat colour[4];

	public:
	// Return the surface display list
	GLuint get_displaylist();
	// Return whether re-rendering is necessary
	bool should_rerender() { return (render_point == log ? FALSE : TRUE); }
	// Update the log point of the surface
	void update_renderpoint() { render_point = log; }
	// Set whether the surface is visible
	void set_visible(bool v) { visible = v; }
	// Return whether the surface is visible
	bool get_visible() { return visible; }
	// Set the rendering style of the surface
	void set_style(surface_style ss) { style = ss; log++; }
	// Return the rendering style of the surface
	surface_style get_style() { return style; }
	// Set the colour of the surface
	void set_colour(int r, int g, int b);
	void set_colour(double r, double g, double b);
	// Set transparency of the surface
	void set_transparency(GLfloat a) { colour[3] = a; log++; }
	// Return the colour of the surface
	GLfloat *get_colour() { return colour; }

	/*
	// Transformations
	*/
	public:
	void bohr_to_angstrom();
};

#endif
