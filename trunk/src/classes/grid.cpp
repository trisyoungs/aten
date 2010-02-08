/*
	*** Grid data structure
	*** src/classes/grid.cpp
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

#include "classes/grid.h"
#include "base/messenger.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "base/constants.h"
#include <QtOpenGL/QtOpenGL>

// Surface rendering styles
const char *SurfaceStyleKeywords[] = { "grid", "points", "triangles", "solid" };
Grid::SurfaceStyle Grid::surfaceStyle(const char *s)
{
	return (Grid::SurfaceStyle) enumSearch("surface style", Grid::nSurfaceStyles, SurfaceStyleKeywords, s);
}

// Constructor
Grid::Grid()
{
	// Private variables
	data3d_ = NULL;
	data2d_ = NULL;
	type_ = Grid::NoData;
	dataFull_ = FALSE;
	minimum_ = 10000.0;
	maximum_ = -10000.0;
	cutoff_ = 0.0;
	log_ = -1;
	style_ = Grid::SolidSurface;
	displayList_ = 0;
	offScreenDisplayList_ = 0;
	renderPoint_ = -1;
	visible_ = TRUE;
	positiveColour_[0] = 0.0;
	positiveColour_[1] = 0.0;
	positiveColour_[2] = 1.0;
	positiveColour_[3] = 0.5;
	negativeColour_[0] = 1.0;
	negativeColour_[1] = 1.0;
	negativeColour_[2] = 1.0;
	negativeColour_[3] = 0.5;
	symmetric_ = FALSE;
	loopOrder_.set(0,1,2);
	colourScale_ = 0;
	//prefs.colourScale[0].addLink(this);
	useColourScale_ = FALSE;
	useDataForZ_ = TRUE;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Grid::~Grid()
{
	clear();
	if (useColourScale_ && (colourScale_ != -1)) prefs.colourScale[colourScale_].breakLink(this);
}

// Assignment operator
void Grid::operator=(Grid &source)
{
	// Copy PODs directly
	type_ = source.type_;
	dataFull_ = source.dataFull_;
	minimum_ = source.minimum_;
	maximum_ = source.maximum_;
	cutoff_ = source.cutoff_;
	log_ = 0;
	style_ = source.style_;
	displayList_ = 0;
	renderPoint_ = -1;
	visible_ = source.visible_;
	for (int i=0; i<4; i++)
	{
		positiveColour_[i] = source.positiveColour_[i];
		negativeColour_[i] = source.negativeColour_[i];
	}
	symmetric_ = source.symmetric_;
	loopOrder_ = source.loopOrder_;
	colourScale_ = source.colourScale_;
	useColourScale_ = source.useColourScale_;
	useDataForZ_ = source.useDataForZ_;
	cell_ = source.cell_;
	origin_ = source.origin_;
	nPoints_ = source.nPoints_;
	// Delete any existing 2D or 3D array in this Grid
	deleteArrays();
	// Create new data structure
	create();
	// Copy data from source structure
	int x,y,z;
	if (type_ == Grid::VolumetricData)
	{
		for (x=0; x<nPoints_.x; x++)
		{
			for (y=0; y<nPoints_.y; y++)
			{
				for (z=0; z<nPoints_.z; z++) data3d_[x][y][z] = source.data3d_[x][y][z];
			}
		}
	}
	else
	{
		for (x=0; x<nPoints_.x; x++)
		{
			for (y=0; y<nPoints_.y; y++) data2d_[x][y] = source.data2d_[x][y];
		}
	}
	name_ = source.name_;
}

/*
// Identity
*/

// Set name of Grid data
void Grid::setName(const char *s)
{
	name_ = s;
}

// Return name of Grid data
const char *Grid::name()
{
	return name_.get();
}

// Set type of Grid data
void Grid::setType(GridType gt)
{
	type_ = gt;
}

// Return type of Grid data
Grid::GridType Grid::type()
{
	return type_;
}

/*
// Gridded Data
*/

// Return the Grid axes
Mat3<double> Grid::axes()
{
	return cell_.axes();
}

// Return lengths of cell axes
Vec3<double> Grid::lengths()
{
	return cell_.lengths();
}

// Set data origin
void Grid::setOrigin(const Vec3<double> v)
{
	origin_ = v; log_++;
}

// Return the origin of the Grid data
Vec3<double> Grid::origin()
{
	return origin_;
}

// Return number of points in data series
Vec3<int> Grid::nPoints()
{
	return nPoints_;
}

// Return minimum value in data[]
double Grid::minimum()
{
	return minimum_;
}

// Return maximum value in data[]
double Grid::maximum()
{
	return maximum_;
}

// Set isovalue cutoff for surface
void Grid::setCutoff(double d)
{
	cutoff_ = d;
	log_++;
}

// Return isovalue cutoff for surface
double Grid::cutoff()
{
	return cutoff_;
}

// Return whether supplied number is within cutoff range
bool Grid::withinCutoff(double d)
{
	// TEST
	if (d < cutoff_) return TRUE;
	return FALSE;
}

// Return 3D data array
double ***Grid::data3d()
{
	return data3d_;
}

// Return 2D data array
double **Grid::data2d()
{
	return data2d_;
}

// Set loop ordering
void Grid::setLoopOrder(int n, int xyz)
{
	loopOrder_.set(n,xyz);
}

// Return whether re-rendering is necessary
bool Grid::shouldRerender()
{
	return (renderPoint_ == log_ ? FALSE : TRUE);
}

// Update the log point of the surface
void Grid::updateRenderPoint()
{
	renderPoint_ = log_;
}

// Request re-rendering of the surface inside a new display list
void Grid::requestRerender()
{
	log_ ++;
}

// Set whether the surface is visible
void Grid::setVisible(bool v)
{
	visible_ = v;
}

// Return whether the surface is visible
bool Grid::isVisible()
{
	return visible_;
}

// Set the rendering style of the surface
void Grid::setStyle(Grid::SurfaceStyle ss)
{
	style_ = ss;
	log_++;
}

// Return the rendering style of the surface
Grid::SurfaceStyle Grid::style()
{
	return style_;
}

// Set alpha value of the surface
void Grid::setAlpha(double a)
{
	positiveColour_[3] = a;
	negativeColour_[3] = a;
	log_++;
}

// Return alpha value of the grid's surface
double Grid::alpha()
{
	return positiveColour_[3];
}

// Return the (positive) colour of the grid's surface
double *Grid::positiveColour()
{
	return positiveColour_;
}

// Copy the positive colour of the surface
void Grid::copyPositiveColour(GLfloat *col)
{
	col[0] = (GLfloat) positiveColour_[0];
	col[1] = (GLfloat) positiveColour_[1];
	col[2] = (GLfloat) positiveColour_[2];
	col[3] = (GLfloat) positiveColour_[3];
}

// Return the (negative) colour of the grid's surface
double *Grid::negativeColour()
{
	return negativeColour_;
}

// Copy the negative colour of the surface
void Grid::copyNegativeColour(GLfloat *col)
{
	col[0] = (GLfloat) negativeColour_[0];
	col[1] = (GLfloat) negativeColour_[1];
	col[2] = (GLfloat) negativeColour_[2];
	col[3] = (GLfloat) negativeColour_[3];
}

// Log changes
void Grid::logChange()
{
	log_ ++;
}

// Set the colourscale associated with the data
void Grid::setColourScale(int id)
{
	// Check range of supplied id
	if ((id < 0) || (id > 9))
	{
		// Remove link in old colourscale if necessary
		if (useColourScale_) prefs.colourScale[colourScale_].breakLink(this);
		useColourScale_ = FALSE;
		log_ ++;
		return;
	}
	// Remove old colourscale link (if one existed)
	if (useColourScale_) prefs.colourScale[colourScale_].breakLink(this);
	colourScale_ = id;
	log_ ++;
	prefs.colourScale[colourScale_].addLink(this);
	useColourScale_ = TRUE;
	int i, j, k;
	double **data2, *data1;
	// Adjust the colour scale to encompass all grid values...
	if (type_ == Grid::VolumetricData)
	{
		for (i = 0; i < nPoints_.x; i++)
		{
			data2 = data3d_[i];
			for (j = 0; j<nPoints_.y; j++)
			{
				data1 = data2[j];
				for (k = 0; k<nPoints_.z; k++) prefs.colourScale[colourScale_].adjustRange(data1[k]);
			}
		}
	}
	else if (type_ == Grid::SurfaceData)
	{
		for (i = 0; i < nPoints_.x; i++)
		{
			data1 = data2d_[i];
			for (j = 0; j<nPoints_.y; j++) prefs.colourScale[colourScale_].adjustRange(data1[j]);
		}
	}
}

// Return the colourscale associated with the data
int Grid::colourScale()
{
	return colourScale_;
}

// Set whether the surface uses the defined colour scale or not
void Grid::setUseColourScale(bool b)
{
	useColourScale_ = b;
	log_ ++;
}

// Whether the surface uses the defined colour scale or not
bool Grid::useColourScale()
{
	return useColourScale_;
}

// Set whether to use data2d_ values for the z-component of the 2D surface
void Grid::setUseDataForZ(bool b)
{
	useDataForZ_ = b;
	log_ ++;
}

// Whether to use data2d_ value sfor z-component of 2D surface
bool Grid::useDataForZ()
{
	return useDataForZ_;
}

// Create data array (from npoints vector)
void Grid::create()
{
	msg.enter("Grid::create");
	int i, j;
	if (type_ == Grid::VolumetricData)
	{
		if (data3d_ != NULL) clear();
		data3d_ = new double**[nPoints_.x];
		for (i = 0; i<nPoints_.x; i++)
		{
			data3d_[i] = new double*[nPoints_.y];
			for (j = 0; j<nPoints_.y; j++) data3d_[i][j] = new double[nPoints_.z];
		}
	}
	else if (type_ == Grid::SurfaceData)
	{
		if (data2d_ != NULL) clear();
		data2d_ = new double*[nPoints_.x];
		for (i = 0; i<nPoints_.x; i++) data2d_[i] = new double[nPoints_.y];
	}
	msg.exit("Grid::create");
}

// Clear array data only
void Grid::deleteArrays()
{
	msg.enter("Grid::deleteArrays");
	int i, j;
	if (data3d_ != NULL)
	{
		for (i = 0; i<nPoints_.x; i++)
		{
			for (j = 0; j<nPoints_.y; j++) delete[] data3d_[i][j];
			delete[] data3d_[i];
		}
		delete[] data3d_;
		data3d_ = NULL;
	}
	if (data2d_ != NULL)
	{
		for (i = 0; i<nPoints_.x; i++)
		{
			delete[] data2d_[i];
		}
		delete[] data2d_;
		data2d_ = NULL;
	}
	msg.exit("Grid::deleteArrays");
}

// Clear all data
void Grid::clear()
{
	msg.enter("Grid::clear");
	dataFull_ = FALSE;
	minimum_ = 10000.0;
	maximum_ = -10000.0;
	cutoff_ = 0.0;
	currentPoint_.zero();
	visible_ = TRUE;
	deleteArrays();
	msg.exit("Grid::clear");
}

// Return pointer to the underlying cell structure
Cell *Grid::cell()
{
	return &cell_;
}

// Set spacing for a cubic grid
void Grid::setAxes(double r)
{
	cell_.set( Vec3<double>(r,r,r), Vec3<double>(90.0, 90.0, 90.0) );
	log_++;
}

// Set spacing for an orthorhombic grid
void Grid::setAxes(const Vec3<double> v)
{
	cell_.set( v, Vec3<double>(90.0, 90.0, 90.0) );
	log_++;
}

// Set spacing for a parallelepiped grid
void Grid::setAxes(const Mat3<double> m)
{
	cell_.set(m);
	log_++;
}

// Get cell axes in suitable GL format
double *Grid::axesForGl()
{
	return cell_.axesForGL();
}

// Set grid extent (and data[])
void Grid::setNPoints(Vec3<int> v)
{
	msg.enter("Grid::setNPoints");
	nPoints_ = v;
	// If nPoints_.z is zero, its a 2D array
	if (nPoints_.z == 0) type_ = Grid::SurfaceData;
	else type_ = Grid::VolumetricData;
	log_ ++;
	create();
	msg.exit("Grid::setNPoints");
}

// Update minimum / maximum based on supplied value
void Grid::setLimits(double d)
{
	if (d < minimum_) minimum_ = d;
	else if (d > maximum_) maximum_ = d;
	cutoff_ = (maximum_ - minimum_) * 0.5 + minimum_;
}

// Set specific point in data array
void Grid::setData(int x, int y, int z, double d)
{
	// Check limits against npoints vector
	if ((x < 0) || (x >= nPoints_.x))
	{
		msg.print("X index %i is outside array bounds (0 to %i) for grid data.\n", x, nPoints_.x-1);
		return;
	}
	else if ((y < 0) || (y >= nPoints_.y))
	{
		msg.print("Y index %i is outside array bounds (0 to %i) for grid data.\n", y, nPoints_.y-1);
		return;
	}
	else if ((type_ == Grid::SurfaceData) && ((z < 0) || (z >= nPoints_.z)))
	{
		msg.print("Z index %i is outside array bounds (0 to %i) for grid data.\n", z, nPoints_.z-1);
		return;
	}
	// Okay, so store data
	if (type_ == Grid::VolumetricData) data3d_[x][y][z] = d;
	else data2d_[x][y] = d;
	// Set new minimum / maximum
	setLimits(d);
}

// Set 'next' point in data array
void Grid::setNextData(double d)
{
	// Check limit
	if (dataFull_ == TRUE)
	{
		msg.print("Grid::setNextData - Array already full.\n");
		return;
	}
	// Set current point referenced by currentpoint and increase it
	if (type_ == Grid::VolumetricData)
	{
		currentPoint_.print();
		data3d_[currentPoint_.x][currentPoint_.y][currentPoint_.z] = d;
		currentPoint_.set(loopOrder_.x, currentPoint_.get(loopOrder_.x) + 1);
		if (currentPoint_.get(loopOrder_.x) == nPoints_.get(loopOrder_.x))
		{
			currentPoint_.set(loopOrder_.x, 0);
			currentPoint_.set(loopOrder_.y, currentPoint_.get(loopOrder_.y) + 1);
			if (currentPoint_.get(loopOrder_.y) == nPoints_.get(loopOrder_.y))
			{
				currentPoint_.set(loopOrder_.y, 0);
				currentPoint_.set(loopOrder_.z, currentPoint_.get(loopOrder_.z) + 1);
				if (currentPoint_.get(loopOrder_.z) == nPoints_.get(loopOrder_.z)) dataFull_ = TRUE;
			}
		}
	}
	else
	{
		data2d_[currentPoint_.x][currentPoint_.y] = d;
		currentPoint_.set(loopOrder_.x, currentPoint_.get(loopOrder_.x) + 1);
		if (currentPoint_.get(loopOrder_.x) == nPoints_.get(loopOrder_.x))
		{
			currentPoint_.set(loopOrder_.x, 0);
			currentPoint_.set(loopOrder_.y, currentPoint_.get(loopOrder_.y) + 1);
			if (currentPoint_.get(loopOrder_.y) == nPoints_.get(loopOrder_.y)) dataFull_ = TRUE;
		}
	}
	// Set new minimum / maximum
	setLimits(d);
}

void Grid::setPositiveColour(double r, double g, double b, double a)
{
	positiveColour_[0] = r;
	positiveColour_[1] = g;
	positiveColour_[2] = b;
	if (a >= 0.0) positiveColour_[3] = a;
	log_ ++;
}

void Grid::setNegativeColour(double r, double g, double b, double a)
{
	negativeColour_[0] = r;
	negativeColour_[1] = g;
	negativeColour_[2] = b;
	if (a >= 0.0) negativeColour_[3] = a;
	log_ ++;
}

// Convert Bohr to Angstrom
void Grid::bohrToAngstrom()
{
	// Only the axes and origin need to be modified...
	Vec3<double> lengths = cell_.lengths();
	Vec3<double> angles = cell_.angles();
	lengths *= ANGBOHR;
	cell_.set(lengths, angles);
	origin_ *= ANGBOHR;
}

// Return displaylist (create first if necessary)
GLuint Grid::displayList(bool offscreenlist)
{
	// If an offscreen list is requested, we shouldn't have to (and its probably dangerous to try) and delete any previous off-screen list
	// since it will have been destroyed along with the off-screen temporary context.
	if (offscreenlist)
	{
		offScreenDisplayList_ = glGenLists(1);
		return offScreenDisplayList_;
	}
	else if (displayList_ == 0)
	{
		displayList_ = glGenLists(1);
		if (displayList_ == 0) printf("Critical - couldn't generate display list for grid data.\n");
	}
	return displayList_;
}

// Set whether to use both signs of a symmetric isovalue distribution
void Grid::setSymmetric(bool b)
{
	symmetric_ = b;
	log_ ++;
}

// Returns whether to use both signs of a symmetric isovalue distribution
bool Grid::isSymmetric()
{
	return symmetric_;
}
