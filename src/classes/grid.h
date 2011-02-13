/*
	*** Grid data structure
	*** src/classes/grid.h
	Copyright T. Youngs 2007-2010

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
#include "base/dnchar.h"
#include "base/cell.h"
#include "base/constants.h"

// GridPoint class
class GridPoint
{
	public:
	// Constructor / Destructor
	GridPoint();
	~GridPoint();
	// List pointers
	GridPoint *prev, *next;

	// Data
	private:
	// Coordinates of point
	Vec3<double> r_;
	// Value at point
	double value_;
	// Associated flag
	int flag_;

	public:
	// Return coordinates of point
	Vec3<double> &r();
	// Return value at point
	double value() const;
	// Set value at point
	void setValue(double v);
	// Retrieve flag status
	int flag() const;
	// Set flag status
	void setFlag(int i);
};

// Grid Data Class
class Grid
{
	public:
	// Constructor / Destructor
	Grid();
	~Grid();
	// List pointers
	Grid *prev, *next;
	// Grid type
	enum GridType { NoData, RegularXYData, RegularXYZData, FreeXYZData, nGridTypes };
	static GridType gridType(const char *s, bool reporterror);
	static const char *gridType(Grid::GridType gt);
	// Surface rendering styles
	enum SurfaceStyle { PointSurface, TriangleSurface, SolidSurface, nSurfaceStyles };
	static SurfaceStyle surfaceStyle(const char *s);
	// Assignment operator
	void operator=(Grid &source);


	/*
	// Identity
	*/
	private:
	// Name of the Grid data
	Dnchar name_;
	// Type of data contained in the class
	GridType type_;

	public:
	// Set name of Grid data
	void setName(const char *s);
	// Return name of Grid data
	const char *name() const;
	// Initialise grid to specified type and size
	bool initialise(GridType type, Vec3<int> npoints);
	// Return type of Grid data
	GridType type() const;


	/*
	// Gridded Data
	*/
	private:
	// Cell that determines origin, spacing between Gridpoints, and their axis system
	Cell cell_;
	// Surface origin
	Vec3<double> origin_;
	// Number of points in each direction
	Vec3<int> nPoints_;
	// Voxel values
	double ***data3d_;
	// Surface values
	double **data2d_;
	// Free grid data
	List<GridPoint> gridPoints_;
	// Clear array data only
	void deleteArrays();
	// Clear all data
	void clear();
	// Allocate grid arrays
	bool allocateArrays();
	// Cutoffs for isosurface generation
	double lowerPrimaryCutoff_, upperPrimaryCutoff_;
	double lowerSecondaryCutoff_, upperSecondaryCutoff_;
	// Bounds (enclosing rhombohedral volume) for grid data
	Vec3<double> lowerLeftCorner_, upperRightCorner_;
	// Logpoint for last bounds calculation
	int boundsLog_;
	// Minimum and maximum values stored in data[]
	double minimum_, maximum_;
	// Update minimum and maximum values
	void setLimits(double d);
	// Order of loops when reading data point-by-point
	Vec3<int> loopOrder_;
	// Use data value for z-component of 2D surface
	bool useDataForZ_;
	// Calculate bounding lower-left and upper-right corners
	void calculateBounds();
	// Sum calculation log point
	int sumPoint_;
	// Total sums of the positive and negative gridpoints
	double totalPositiveSum_, totalNegativeSum_;
	// Partial sums of the grid surfaces, determined by cutoffs
	double partialPrimarySum_, partialSecondarySum_;
	// Calculate sums
	void calculateSums();

	public:
	// Return pointer to the underlying cell structure
	Cell *cell();
	// Set spacing for a cubic Grid
	void setAxes(double r);
	// Set spacing for an orthorhombic Grid
	void setAxes(const Vec3<double> lengths);
	// Set spacing for a parallelepiped Grid
	void setAxes(const Matrix axes);
	// Return the Grid axes
	Matrix axes() const;
	// Return lengths of cell axes
	Vec3<double> lengths() const;
	// Set data origin
	void setOrigin(const Vec3<double> v);
	// Return the origin of the Grid data
	Vec3<double> origin() const;
	// Return number of points in regular gridded data (if it exists)
	Vec3<int> nPoints() const;
	// Return minimum value in data[]
	double minimum() const;
	// Return maximum value in data[]
	double maximum() const;
	// Return LLC bounding values, calculating first if necessary
	Vec3<double> lowerLeftCorner();
	// Return URC bounding values, calculating first if necessary
	Vec3<double> upperRightCorner();
	// Set lower isovalue cutoff for primary surface
	void setLowerPrimaryCutoff(double d);
	// Return lower isovalue cutoff for primary surface
	double lowerPrimaryCutoff() const;
	// Set uppwer isovalue cutoff for primary surface
	void setUpperPrimaryCutoff(double d);
	// Return upper isovalue cutoff for primary surface
	double upperPrimaryCutoff() const;
	// Return whether supplied number is within primary cutoff range
	bool withinPrimaryCutoff(double d) const;
	// Set lower isovalue cutoff for secondary surface
	void setLowerSecondaryCutoff(double d);
	// Return lower isovalue cutoff for secondary surface
	double lowerSecondaryCutoff() const;
	// Set uppwer isovalue cutoff for secondary surface
	void setUpperSecondaryCutoff(double d);
	// Return upper isovalue cutoff for secondary surface
	double upperSecondaryCutoff() const;
	// Return whether supplied number is within secondary cutoff range
	bool withinSecondaryCutoff(double d) const;
	// Return 3D data array
	double ***data3d();
	// Return 2D data array
	double **data2d();
	// Return head of gridpoints array
	GridPoint *gridPoints();
	// Set loop ordering
	void setLoopOrder(int n, int xyz);
	// Set whether to use data2d_ values for the z-component of the 2D surface
	void setUseDataForZ(bool b);
	// Whether to use data2d_ values for z-component of 2D surface
	bool useDataForZ() const;
	// Return the total positive sum of the grid (calculated when drawn)
	double totalPositiveSum();
	// Return the total negative sum of the grid (calculated when drawn)
	double totalNegativeSum();
	// Return the partial primary sum of the grid, determined by cutoffs (calculated when drawn)
	double partialPrimarySum();
	// Return the partial secondary sum of the grid, determined by cutoffs (calculated when drawn)
	double partialSecondarySum();


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
	// Add free data point
	void addFreePoint(double x, double y, double z, double value);


	/*
	// Visuals
	*/
	private:
	// Log for changes to Grid, display style etc.
	int log_;
	// Primitive containing surface triangles
	int renderPoint_;
	// Whether the surface is currently visible
	bool visible_;
	// How to render this surface
	SurfaceStyle style_;
	// Local colours (including alpha component)
	double primaryColour_[4], secondaryColour_[4];
	// Colour scale to take colouring from (zero for internal colours)
	int colourScale_;
	// Whether to use the associated colour scale (TRUE) or the internal colour (FALSE)
	bool useColourScale_;
	// Whether to draw complementary surface with second set of cutoffs
	bool useSecondary_;
	// Whether the grid data is periodic
	bool periodic_;
	// Whether to show outline of grid volume
	bool outlineVolume_;
	// Offset (shift) when drawing grid volume
	Vec3<int> shift_;
	
	public:
	// Increase the internal log
	void logChange();
	// Return whether re-rendering is necessary
	bool shouldRerender() const;
	// Update the log point of the surface
	void updateRenderPoint();
	// Request rerendering of the surface (update log)
	void requestRerender();
	// Set whether the surface is visible
	void setVisible(bool v);
	// Return whether the surface is visible
	bool isVisible() const;
	// Set the rendering style of the surface
	void setStyle(SurfaceStyle ss);
	// Return the rendering style of the surface
	SurfaceStyle style() const;
	// Set the primary colour of the surface
	void setPrimaryColour(double r, double g, double b, double a = -1);
	// Set the secondary colour of the surface
	void setSecondaryColour(double r, double g, double b, double a = -1);
	// Set alpha value of the primary colour
	void setPrimaryAlpha(double a);
	// Return the alpha value of the primary colour
	double primaryAlpha() const;
	// Set alpha value of the secondary colour
	void setSecondaryAlpha(double a);
	// Return the alpha value of the secondary colour
	double secondaryAlpha() const;
	// Return the primary colour of the surface
	double *primaryColour();
	// Copy the primary colour of the surface
	void copyPrimaryColour(GLfloat *col);
	// Return the secondary colour of the surface
	double *secondaryColour();
	// Copy the secondary colour of the surface
	void copySecondaryColour(GLfloat *col);
	// Set the colourscale associated with the data
	void setColourScale(int id);
	// Return the colourscale associated with the data
	int colourScale() const;
	// Whether the surface uses the defined colour scale or not
	bool useColourScale() const;
	// Set whether the surface should be rendered with an associated colourscale
	void setUseColourScale(bool b);
	// Set whether to render additional data using secondary cutoff
	void setUseSecondary(bool b);
	// Returns whether to render additional data using secondary cutoff
	bool useSecondary() const;
	// Set whether the grid data is periodic
	void setPeriodic(bool b);
	// Return whether the grid data is periodic
	bool periodic() const;
	// Set whether to outline grid volume
	void setOutlineVolume(bool b);
	// Return whether to outline grid volume
	bool outlineVolume() const;
	// Set shift amounts for grid
	void setShift(int i, int j, int k);
	// Set single shift amount
	void setShift(int id, int i);
	// Return shift amount
	Vec3<int> shift();


	/*
	// Transformations
	*/
	public:
	void bohrToAngstrom();
};

#endif
