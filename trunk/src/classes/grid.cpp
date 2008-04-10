/*
	*** Grid data structure
	*** src/classes/grid.cpp
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

#include "classes/grid.h"
#include "base/debug.h"
#include "base/prefs.h"
#include "base/constants.h"
#include <QtOpenGL/QtOpenGL>

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
	renderPoint_ = -1;
	visible_ = TRUE;
	colour_[0] = 1.0f;
	colour_[1] = 0.0f;
	colour_[2] = 0.0f;
	colour_[3] = 0.5f;
	loopOrder_.set(0,1,2);
	colourScale_ = 0;
	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
Grid::~Grid()
{
	clear();
}

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

// Return the Grid axes
Mat3<double> Grid::axes()
{
	return cell_.axes();
}

// Return lengths of cell axiss
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

// Request re-rendering of the surface
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

// Set transparency of the surface
void Grid::setTransparency(GLfloat a)
{
	colour_[3] = a;
	log_++;
}

// Return transparency of the grid's surface
GLfloat Grid::transparency()
{
	return colour_[3];
}

// Return the colour of the grid's surface
GLfloat *Grid::colour()
{
	return colour_;
}

// Set the colourscale associated with the data
void Grid::setColourScale(int id)
{
	colourScale_ = id;
	log_ ++;
	if (colourScale_ == 0) return;
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

// Create data array (from npoints vector)
void Grid::create()
{
	dbgBegin(Debug::Calls,"Grid::create");
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
	dbgEnd(Debug::Calls,"Grid::create");
}

// Clear data array
void Grid::clear()
{
	dbgBegin(Debug::Calls,"Grid::clear");
	dataFull_ = FALSE;
	minimum_ = 10000.0;
	maximum_ = -10000.0;
	cutoff_ = 0.0;
	currentPoint_.zero();
	visible_ = TRUE;
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

	dbgEnd(Debug::Calls,"Grid::clear");
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

// Get cell axes in suitaqble GL format
void Grid::axesForGl(double *glmat)
{
	cell_.axesForGl(glmat);
}

// Set grid extent (and data[])
void Grid::setNPoints(Vec3<int> v)
{
	dbgBegin(Debug::Calls,"Grid::setNPoints");
	nPoints_ = v;
	// If nPoints_.z is zero, its a 2D array
	if (nPoints_.z == 0) type_ = Grid::SurfaceData;
	else type_ = Grid::VolumetricData;
	log_ ++;
	create();
	dbgEnd(Debug::Calls,"Grid::setNPoints");
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
		msg(Debug::None,"X index %i is outside array bounds (0--%i) for grid data.\n", x, nPoints_.x-1);
		return;
	}
	else if ((y < 0) || (y >= nPoints_.y))
	{
		msg(Debug::None,"Y index %i is outside array bounds (0--%i) for grid data.\n", y, nPoints_.y-1);
		return;
	}
	else if ((type_ == Grid::SurfaceData) && ((z < 0) || (z >= nPoints_.z)))
	{
		msg(Debug::None,"Z index %i is outside array bounds (0--%i) for grid data.\n", z, nPoints_.z-1);
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
		msg(Debug::None,"Grid::setNextData - Array already full.\n");
		return;
	}
	// Set current point referenced by currentpoint and increase it
	if (type_ == Grid::VolumetricData)
	{
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

// Set surface colour
void Grid::setColour(int r, int g, int b)
{
	colour_[0] = r;
	colour_[1] = g;
	colour_[2] = b;
	log_ ++;
}

void Grid::setColour(double r, double g, double b)
{
	colour_[0] = (GLfloat) r;
	colour_[1] = (GLfloat) g;
	colour_[2] = (GLfloat) b;
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
GLuint Grid::displayList()
{
	if (displayList_ == 0)
	{
		displayList_ = glGenLists(1);
		if (displayList_ == 0) printf("Critical - couldn't generate display list for grid data.\n");
	}
	return displayList_;
}
