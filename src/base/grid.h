/*
	*** Grid Data
	*** src/base/grid.h
	Copyright T. Youngs 2007-2016

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
#include "base/cell.h"
#include "base/gridpoint.h"
#include "math/constants.h"
#include "base/namespace.h"
#include "render/primitive.h"
#include "templates/objectstore.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations
class FilePluginInterface;

// Grid Data
class Grid : public ListItem<Grid>, ObjectStore<Grid>
{
	public:
	// Constructor / Destructor
	Grid();
	~Grid();
	// Grid type
	enum GridType { NoData, RegularXYData, RegularXYZData, FreeXYZData, nGridTypes };
	static GridType gridType(QString s, bool reportError = false);
	static const char* gridType(Grid::GridType gt);
	// Surface rendering styles
	enum SurfaceStyle { PointSurface, MeshSurface, SolidSurface, nSurfaceStyles };
	static SurfaceStyle surfaceStyle(QString s);
	static const char* surfaceStyle(Grid::SurfaceStyle ss);
	// Assignment operator
	void operator=(Grid& source);


	/*
	 * Identity
	 */
	private:
	// Parent model
	Model* parent_;
	// Filename from which the grid data was loaded
	QString filename_;
	// Plugin used to load the grid data
	FilePluginInterface* plugin_;
	// Name of the Grid data
	QString name_;
	// Type of data contained in the class
	GridType type_;

	public:
	// Set filename of grid data
	void setFilename(QString filename);
	// Return filename of grid data
	QString filename() const;
	// Set plugin used to load the grid data
	void setPlugin(FilePluginInterface* plugin);
	// Return plugin used to load the grid data
	FilePluginInterface* plugin();
	// Set name of Grid data
	void setName(QString name);
	// Return name of Grid data
	QString name() const;
	// Initialise grid to specified type and size
	bool initialise(GridType type, Vec3<int> nXYZ);
	// Return type of Grid data
	GridType type() const;
	// Return parent model
	Model* parent();
	// Set parent model
	void setParent(Model* parent);


	/*
	 * Data
	 */
	private:
	// Cell that determines origin, spacing between Gridpoints, and their axis system
	UnitCell cell_;
	// Surface origin
	Vec3<double> origin_;
	// Data minima (values at origin)
	Vec3<double> dataMinimum_;
	// Data maxima
	Vec3<double> dataMaximum_;
	// Whether axes are visible
	bool axisVisible_[3];
	// Positions of axes
	Vec3<double> axisPosition_[3];
	// Major spacing for axis ticks
	Vec3<double> axisMajorSpacing_;
	// Number of minor ticks per major spacing
	Vec3<int> axisMinorTicks_;
	// Number of points in each direction
	Vec3<int> nXYZ_;
	// Voxel values
	double** *data3d_;
	// Surface values
	double** data2d_;
	// Free grid data
	List<GridPoint> gridPoints_;
	// Cutoffs for isosurface generation
	double lowerPrimaryCutoff_, upperPrimaryCutoff_;
	double lowerSecondaryCutoff_, upperSecondaryCutoff_;
	// Bounds (enclosing rhombohedral volume) for grid data
	Vec3<double> lowerLeftCorner_, upperRightCorner_;
	// Logpoint for last bounds calculation
	int boundsLog_;
	// Minimum and maximum values stored in data[]
	double minimum_, maximum_;
	// Order of loops when reading data point-by-point
	Vec3<int> loopOrder_;
	// Use data value for z-component of 2D surface
	bool useDataForZ_;
	// Sum calculation log point
	int sumPoint_;
	// Total sums of the positive and negative gridpoints
	double totalPositiveSum_, totalNegativeSum_;
	// Partial sums of the grid surfaces, determined by cutoffs
	double partialPrimarySum_, partialSecondarySum_;

	private:
	// Clear all data
	void clear();
	// Clear array data only
	void deleteArrays();
	// Allocate grid arrays
	bool allocateArrays();
	// Update minimum and maximum values
	void setLimits(double d);
	// Calculate sums
	void calculateSums();
	// Calculate bounding lower-left and upper-right corners
	void calculateBounds();
	// Calculate sum of points between given cutoffs
	double sum(double lowerCutoff, double upperCutoff);
	// Calculate cutoff to give requested percentage of grid encapsulated in displayed surface
	bool calculateCutoff(double percentage, bool upperCutoff, bool secondary, double& newCutoff);

	public:
	// Return pointer to the underlying cell structure
	UnitCell* cell();
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
	// Set data minima
	void setDataMinimum(const Vec3<double> v);
	// Return the axis minima
	Vec3<double> dataMinimum() const;
	// Set data maxima
	void setDataMaximum(const Vec3<double> v);
	// Return the axis maxima
	Vec3<double> dataMaximum() const;
	// Set whether axis is visible
	void setAxisVisible(int id, bool visible);
	// Return whether axis is visible
	bool isAxisVisible(int id);
	// Return axis visibility array
	bool* axisVisible();
	// Set axis position element
	void setAxisPosition(int axis, int el, double value);
	// Return axis position
	Vec3<double> axisPosition(int axis);
	// Set axis major spacing
	void setAxisMajorSpacing(int axis, double value);
	// Return axis position
	Vec3<double> axisMajorSpacing();
	// Set axis major spacing
	void setAxisMinorTicks(int axis, int value);
	// Return axis position
	Vec3<int> axisMinorTicks();
	// Return number of points in regular gridded data (if it exists)
	Vec3<int> nXYZ() const;
	// Return total number of points
	int nPoints() const;
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
	// Set upper isovalue cutoff for primary surface
	void setUpperPrimaryCutoff(double d);
	// Set primary lower/upper cutoff based on view percentage
	double setPrimaryCutoffAsViewPercentage(double d, bool upperCutoff);
	// Return upper isovalue cutoff for primary surface
	double upperPrimaryCutoff() const;
	// Return whether supplied number is within primary cutoff range
	bool withinPrimaryCutoff(double d) const;
	// Set lower isovalue cutoff for secondary surface
	void setLowerSecondaryCutoff(double d);
	// Return lower isovalue cutoff for secondary surface
	double lowerSecondaryCutoff() const;
	// Set upper isovalue cutoff for secondary surface
	void setUpperSecondaryCutoff(double d);
	// Set secondary lower/upper cutoff based on view percentage
	double setSecondaryCutoffAsViewPercentage(double d, bool upperCutoff);
	// Return upper isovalue cutoff for secondary surface
	double upperSecondaryCutoff() const;
	// Return whether supplied number is within secondary cutoff range
	bool withinSecondaryCutoff(double d) const;
	// Return 3D data array
	double*** data3d();
	// Return 2D data array
	double** data2d();
	// Return head of gridpoints array
	GridPoint* gridPoints();
	// Set loop ordering (from string)
	void setLoopOrder(QString loopOrder);
	// Set loop ordering element
	void setLoopOrder(int n, int xyz);
	// Return loop ordering 
	Vec3<int> loopOrder();
	// Set whether to use data2d_ values for the z-component of the 2D surface
	void setUseDataForZ(bool b);
	// Whether to use data2d_ values for z-component of 2D surface
	bool useDataForZ() const;
	// Return the total positive sum of the grid
	double totalPositiveSum();
	// Return the total negative sum of the grid
	double totalNegativeSum();
	// Return the total absolute sum of the grid
	double totalAbsoluteSum();
	// Return the partial primary sum of the grid, determined by cutoffs
	double partialPrimarySum();
	// Return the partial secondary sum of the grid, determined by cutoffs
	double partialSecondarySum();


	/*
	 * Data Interface
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
	 * Visuals
	 */
	private:
	// Log for changes to Grid, display style etc.
	int log_;
	// Whether the surface is currently visible
	bool visible_;
	// How to render the primary and secondary surfaces
	SurfaceStyle primaryStyle_, secondaryStyle_;
	// Local colours (including alpha component)
	double primaryColour_[4], secondaryColour_[4];
	// Colour scale to take colouring from (zero for internal colours)
	int colourScale_;
	// Whether to use the associated colour scale (true) or the internal colour (false)
	bool useColourScale_;
	// Whether to draw complementary surface with second set of cutoffs
	bool useSecondary_;
	// Whether the grid data is periodic
	bool periodic_;
	// Whether to show outline of grid volume
	bool outlineVolume_;
	// Offset (shift) when drawing grid volume
	Vec3<int> shift_;
	// Whether to fill enclosed volume
	bool fillEnclosedVolume_;
	// Axis order used in last generation
	Vec3<int> axisLoopOrder_;
	// Directions for axis order
	Vec3<int> axisLoopSigns_;

	public:
	// Increase the internal log
	void logChange();
	// Return whether re-rendering is necessary
	bool shouldRerender() const;
	// Update the log point of the surface
	void updateRenderPoint();
	// Set whether the surface is visible
	void setVisible(bool v);
	// Return whether the surface is visible
	bool isVisible() const;
	// Set the rendering style of the primary surface
	void setPrimaryStyle(SurfaceStyle ss);
	// Return the rendering style of the primary surface
	SurfaceStyle primaryStyle() const;
	// Set the rendering style of the secondarysurface
	void setSecondaryStyle(SurfaceStyle ss);
	// Return the rendering style of the secondary surface
	SurfaceStyle secondaryStyle() const;
	// Set colour of primary surface
	void setPrimaryColour(double r, double g, double b, double a = -1);
	// Return the colour of the primary surface
	double* primaryColour();
	// Copy the colour of the primary surface
	void copyPrimaryColour(Vec4<GLfloat>& col);
	// Set the colour of the secondary surface
	void setSecondaryColour(double r, double g, double b, double a = -1);
	// Return the colour of the secondary surface
	double* secondaryColour();
	// Copy the colour of the secondary surface
	void copySecondaryColour(Vec4<GLfloat>& col);
	// Set alpha value of the primary colour
	void setPrimaryAlpha(double a);
	// Return the alpha value of the primary colour
	double primaryAlpha() const;
	// Set alpha value of the secondary colour
	void setSecondaryAlpha(double a);
	// Return the alpha value of the secondary colour
	double secondaryAlpha() const;
	// Set the colourscale associated with the data
	void setColourScale(int id);
	// Return the colourscale associated with the data
	int colourScale() const;
	// Set whether the surface should be rendered with an associated colourscale
	void setUseColourScale(bool b);
	// Whether the surface uses the defined colour scale or not
	bool useColourScale() const;
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
	// Whether to fill enclosed volume
	void setFillEnclosedVolume(bool b);
	// Return whether to fill enclosed volume
	bool fillEnclosedVolume();
	// Set axis order used in last generation
	void setAxisLoopOrder(Vec3<int> order);
	// Return axis order used in last generation
	Vec3<int> axisLoopOrder();
	// Set directions for axis order
	void setAxisLoopSigns(Vec3<int> signs);
	// Return directions for axis order
	Vec3<int> axisLoopSigns();


	/*
	 * Rendering
	 */
	private:
	// Primitives representing primary surface
	Primitive primaryPrimitive_;
	// Primitives representing secondary surface
	Primitive secondaryPrimitive_;
	// Logpoints at which rendergroups were last created
	int primaryPrimitivePoint_, secondaryPrimitivePoint_;

	public:
	// Return voxel matrix, offset to grid origin
	Matrix voxelMatrix();
	// Return primary primitive
	Primitive& primaryPrimitive();
	// Send primary primitive to GL, regenerating if necessary
	void sendPrimaryPrimitive();
	// Return secondary primitive
	Primitive& secondaryPrimitive();
	// Return secondary primitive to GL, regenerating if necessary
	void sendSecondaryPrimitive();


	/*
	 * Transformations
	 */
	public:
	// Convert data from Bohr to Angstroms
	void bohrToAngstrom();
	// Modify region of data (adjacent gridpoints) (recursive)
	int modifyRegion(int startX, int startY, int startZ, double minValue, double maxValue, double newValue, bool periodic);
};

ATEN_END_NAMESPACE

#endif
