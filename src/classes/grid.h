/*
	*** Grid data structure
	*** src/classes/Grid.h
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
#include <QtOpenGL/QtOpenGL>

// Surface rendering styles
enum SurfaceStyle { SS_GRID, SS_POINTS, SS_TRIANGLES, SS_SOLID };

// 3D Grid Class
class Grid
{
	public:
	// Constructor / Destructor
	Grid();
	~Grid();
	// List pointers
	Grid *prev, *next;

	/*
	// Identity
	*/
	private:
	// Name of the Grid data
	Dnchar name_;

	public:
	// Set name of Grid data
	void setName(const char *s);
	// Return name of Grid data
	const char *name();

	/*
	// Gridded Data
	*/
	private:
	// Cell that determine spacing between Gridpoints and their axis system
	Cell cell_;
	// Surface origin
	Vec3<double> origin_;
	// Number of points in each direction
	Vec3<int> nPoints_;
	// Voxel values
	double ***data_;
	// Clear voxel data
	void clear();
	// Create voxel data
	void create();
	// Cutoff for isosurface generation
	double cutoff_;
	// Minimum and maximum values stored in data[]
	double minimum_, maximum_;
	// Update minimum and maximum values
	void setLimits(double d);
	// Order of loops when reading data point-by-point
	Vec3<int> loopOrder_;

	public:
	// Set spacing for a cubic Grid
	void setAxes(double r);
	// Set spacing for an orthorhombic Grid
	void setAxes(const Vec3<double> lengths);
	// Set spacing for a parallelepiped Grid
	void setAxes(const Mat3<double> axes);
	// Return the Grid axes
	Mat3<double> axes();
	// Return lengths of cell axiss
	Vec3<double> lengths();
	// Set data origin
	void setOrigin(const Vec3<double> v);
	// Return the origin of the Grid data
	Vec3<double> origin();
	// Set number of points in data series (creates data[])
	void setNPoints(Vec3<int>);
	// Return number of points in data series
	Vec3<int> nPoints();
	// Return minimum value in data[]
	double minimum();
	// Return maximum value in data[]
	double maximum();
	// Set isovalue cutoff for surface
	void setCutoff(double d);
	// Return isovalue cutoff for surface
	double cutoff();
	// Return data array
	double ***data();
	// Set loop ordering
	void setLoopOrder(int n, int xyz);

	/*
	// Data Interface
	*/
	private:
	// Count variable used by set_next_point()
	Vec3<int> currentPoint_;
	// Whether enough points have been added
	bool dataFull_;

	public:
	// Set specific point in data array
	void setData(int x, int y, int z, double d);
	// Set 'next' point in data array
	void setNextData(double d);
	
	/*
	// Visuals
	*/
	private:
	// Log for changes to Grid, display style etc.
	int log_;
	// GL display list of rendered surface
	GLuint displayList_;
	// Log point corresponding to the last render
	int renderPoint_;
	// Whether the surface is currently visible
	bool visible_;
	// How to render this surface
	SurfaceStyle style_;
	// Colour (including alpha component)
	GLfloat colour_[4];

	public:
	// Return the surface display list
	GLuint displayList();
	// Return whether re-rendering is necessary
	bool shouldRerender();
	// Update the log point of the surface
	void updateRenderPoint();
	// Request re-rendering of the surface
	void requestRerender();
	// Set whether the surface is visible
	void setVisible(bool v);
	// Return whether the surface is visible
	bool isVisible();
	// Set the rendering style of the surface
	void setStyle(SurfaceStyle ss);
	// Return the rendering style of the surface
	SurfaceStyle style();
	// Set the colour of the surface
	void setColour(int r, int g, int b);
	void setColour(double r, double g, double b);
	// Set transparency of the surface
	void setTransparency(GLfloat a);
	// Return the transparency of the surface
	GLfloat transparency();
	// Return the colour of the surface
	GLfloat *colour();

	/*
	// Transformations
	*/
	public:
	void bohrToAngstrom();
};

#endif
