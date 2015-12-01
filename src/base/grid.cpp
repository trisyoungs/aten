/*
	*** Grid Data
	*** src/base/grid.cpp
	Copyright T. Youngs 2007-2015

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

#include "base/grid.h"
#include "base/messenger.h"
#include "base/prefs.h"
#include "base/sysfunc.h"
#include "math/constants.h"
#include "model/model.h"
#include "render/primitiveset.h"
#ifdef _MAC
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include <QOpenGLContext>

ATEN_USING_NAMESPACE

// Static Members
template<class Grid> RefList<Grid,int> ObjectStore<Grid>::objects_;
template<class Grid> int ObjectStore<Grid>::objectCount_ = 0;

// Grid data types
const char* GridTypeKeywords[Grid::nGridTypes] = { "none", "regularxy", "regularxyz", "freexyz" };
Grid::GridType Grid::gridType(QString s, bool reportError)
{
	Grid::GridType gt = (Grid::GridType) enumSearch("grid type", Grid::nGridTypes, GridTypeKeywords, s, reportError);
	if ((gt == Grid::nGridTypes) && reportError) enumPrintValid(Grid::nGridTypes,GridTypeKeywords);
	return gt;
}
const char* Grid::gridType(Grid::GridType gt)
{
	return GridTypeKeywords[gt];
}

// Surface rendering styles
const char* SurfaceStyleKeywords[] = { "points", "mesh", "solid" };
Grid::SurfaceStyle Grid::surfaceStyle(QString s)
{
	return (Grid::SurfaceStyle) enumSearch("surface style", Grid::nSurfaceStyles, SurfaceStyleKeywords, s);
}
const char* Grid::surfaceStyle(Grid::SurfaceStyle ss)
{
	return SurfaceStyleKeywords[ss];
}

// Constructor
Grid::Grid() : ListItem<Grid>(), ObjectStore<Grid>(this, ObjectTypes::GridObject)
{
	// Private variables
	data3d_ = NULL;
	data2d_ = NULL;
	type_ = Grid::NoData;
	dataFull_ = false;
	minimum_ = 10000.0;
	maximum_ = -10000.0;
	lowerPrimaryCutoff_ = 0.0;
	upperPrimaryCutoff_ = 0.0;
	lowerSecondaryCutoff_ = 0.0;
	upperSecondaryCutoff_ = 0.0;
	log_ = -1;
	boundsLog_ = -1;
// 	primaryPrimitive_ = NULL;
// 	secondaryPrimitive_ = NULL;
	primaryStyle_ = Grid::SolidSurface;
	secondaryStyle_ = Grid::SolidSurface;
	visible_ = true;
	primaryColour_[0] = 0.0;
	primaryColour_[1] = 0.0;
	primaryColour_[2] = 1.0;
	primaryColour_[3] = 0.5;
	secondaryColour_[0] = 1.0;
	secondaryColour_[1] = 1.0;
	secondaryColour_[2] = 1.0;
	secondaryColour_[3] = 0.5;
	useSecondary_ = false;
	outlineVolume_ = false;
	fillEnclosedVolume_ = false;
	periodic_ = false;
	loopOrder_.set(0,1,2);
	colourScale_ = 0;
	parent_ = NULL;
	useColourScale_ = false;
	useDataForZ_ = true;
	totalPositiveSum_ = 0.0;
	totalNegativeSum_ = 0.0;
	partialPrimarySum_ = 0.0;
	partialSecondarySum_ = 0.0;
	sumPoint_ = -1;
	axisVisible_[0] = false;
	axisVisible_[1] = false;
	axisVisible_[2] = false;
	axisPosition_[0].set(0.0,0.0,0.0);
	axisPosition_[1].set(0.0,0.0,0.0);
	axisPosition_[2].set(0.0,0.0,0.0);
	axisMajorSpacing_.set(1.0, 1.0, 1.0);
	axisMinorTicks_.set(3, 3, 3);
}

// Destructor
Grid::~Grid()
{
	clear();
	if (useColourScale_ && (colourScale_ != -1)) prefs.colourScale[colourScale_].breakLink(this);

	if (primaryPrimitive_.registeredAsDynamic()) PrimitiveSet::releaseDynamicPrimitive(&primaryPrimitive_);
	if (secondaryPrimitive_.registeredAsDynamic()) PrimitiveSet::releaseDynamicPrimitive(&secondaryPrimitive_);
}

// Assignment operator
void Grid::operator=(Grid& source)
{
	// Copy PODs directly
	type_ = source.type_;
	dataFull_ = source.dataFull_;
	minimum_ = source.minimum_;
	maximum_ = source.maximum_;
	lowerPrimaryCutoff_ = source.lowerPrimaryCutoff_;
	upperPrimaryCutoff_ = source.upperPrimaryCutoff_;
	lowerSecondaryCutoff_ = source.lowerSecondaryCutoff_;
	upperSecondaryCutoff_ = source.upperSecondaryCutoff_;
	log_ = 0;
	primaryStyle_ = source.primaryStyle_;
	secondaryStyle_ = source.secondaryStyle_;
	visible_ = source.visible_;
	totalPositiveSum_ = source.totalPositiveSum_;
	partialPrimarySum_ = source.partialPrimarySum_;
	sumPoint_ = -1;
	totalNegativeSum_ = source.totalNegativeSum_;
	partialSecondarySum_ = source.partialSecondarySum_;
	for (int i=0; i<4; i++)
	{
		primaryColour_[i] = source.primaryColour_[i];
		secondaryColour_[i] = source.secondaryColour_[i];
	}
	useSecondary_ = source.useSecondary_;
	loopOrder_ = source.loopOrder_;
	colourScale_ = source.colourScale_;
	useColourScale_ = source.useColourScale_;
	useDataForZ_ = source.useDataForZ_;
	cell_ = source.cell_;
	origin_ = source.origin_;
	nXYZ_ = source.nXYZ_;

	// Delete any existing 2D or 3D array in this Grid
	deleteArrays();

	// Create new data structure
	allocateArrays();

	// Copy data from source structure
	int x,y,z;
	if (type_ == Grid::RegularXYZData)
	{
		for (x=0; x<nXYZ_.x; x++)
		{
			for (y=0; y<nXYZ_.y; y++)
			{
				for (z=0; z<nXYZ_.z; z++) data3d_[x][y][z] = source.data3d_[x][y][z];
			}
		}
	}
	else
	{
		for (x=0; x<nXYZ_.x; x++)
		{
			for (y=0; y<nXYZ_.y; y++) data2d_[x][y] = source.data2d_[x][y];
		}
	}
	name_ = source.name_;
}

/*
 * Identity
 */

// Set name of Grid data
void Grid::setName(QString s)
{
	name_ = s;
}

// Return name of Grid data
QString Grid::name() const
{
	return name_;
}

// Initialise grid of specified type and size (if relevant)
bool Grid::initialise(Grid::GridType type, Vec3<int> nXYZ)
{
	Messenger::enter("Grid::initialise");
	type_ = type;
	bool result = true;
	clear();
	switch (type_)
	{
		case (Grid::RegularXYData):
			nXYZ_ = nXYZ;
			result = allocateArrays();
			if (result) Messenger::print("Initialised grid structure for regular 2D XY data, %i points total.", nXYZ_.x*nXYZ_.y);
			break;
		case (Grid::RegularXYZData):
			nXYZ_ = nXYZ;
			result = allocateArrays();
			if (result) Messenger::print("Initialised grid structure for regular 3D XY data, %i points total.", nXYZ_.x*nXYZ_.y*nXYZ_.z);
			break;
		case (Grid::FreeXYZData):
			Messenger::print("Initialised grid structure for free 3D XYZ data.");
			break;
		default:
			break;
	}
	logChange();
	Messenger::exit("Grid::initialise");
	return result;
}

// Return type of Grid data
Grid::GridType Grid::type() const
{
	return type_;
}

// Return parent model
Model* Grid::parent()
{
	return parent_;
}

// Set parent model
void Grid::setParent(Model* parent)
{
	parent_ = parent;
}

/*
 * Data
 */


// Clear all data
void Grid::clear()
{
	Messenger::enter("Grid::clear");
	dataFull_ = false;
	minimum_ = 10000.0;
	maximum_ = -10000.0;
	lowerPrimaryCutoff_ = 0.0;
	upperPrimaryCutoff_ = 0.0;
	lowerSecondaryCutoff_ = 0.0;
	upperSecondaryCutoff_ = 0.0;
	currentPoint_.zero();
	visible_ = true;
	deleteArrays();
	Messenger::exit("Grid::clear");
}

// Clear array data only
void Grid::deleteArrays()
{
	Messenger::enter("Grid::deleteArrays");
	int i, j;
	if (data3d_ != NULL)
	{
		for (i = 0; i<nXYZ_.x; i++)
		{
			for (j = 0; j<nXYZ_.y; j++) delete[] data3d_[i][j];
			delete[] data3d_[i];
		}
		delete[] data3d_;
		data3d_ = NULL;
	}
	if (data2d_ != NULL)
	{
		for (i = 0; i<nXYZ_.x; i++)
		{
			delete[] data2d_[i];
		}
		delete[] data2d_;
		data2d_ = NULL;
	}
	gridPoints_.clear();
	Messenger::exit("Grid::deleteArrays");
}


// Create data array (from npoints vector)
bool Grid::allocateArrays()
{
	Messenger::enter("Grid::allocateArrays");
	int i, j;
	switch (type_)
	{
		case (Grid::RegularXYZData):
			if (data3d_ != NULL) clear();
			// Check point limits (secondary only)
			if (nXYZ_.min() < 1)
			{
				Messenger::print("Can't allocate 3D grid array - One or more grid limits are secondary (%i,%i,%i).", nXYZ_.x, nXYZ_.y, nXYZ_.z);
				Messenger::exit("Grid::allocateArrays");
				return false;
			}
			data3d_ = new double**[nXYZ_.x];
			for (i = 0; i<nXYZ_.x; i++)
			{
				data3d_[i] = new double*[nXYZ_.y];
				for (j = 0; j<nXYZ_.y; j++) data3d_[i][j] = new double[nXYZ_.z];
			}
			break;
		case (Grid::RegularXYData):
			if (data2d_ != NULL) clear();
			// Check point limits (secondary only)
			if ((nXYZ_.x < 1) || (nXYZ_.y < 1))
			{
				Messenger::print("Can't allocate 2D grid array - One or more grid limits are secondary (%i,%i).", nXYZ_.x, nXYZ_.y);
				Messenger::exit("Grid::allocateArrays");
				return false;
			}
			data2d_ = new double*[nXYZ_.x];
			for (i = 0; i<nXYZ_.x; i++) data2d_[i] = new double[nXYZ_.y];
		case (Grid::FreeXYZData):
			break;
		default:
			printf("Internal Error: Don't know how to allocate arrays for grid type '%s'\n", Grid::gridType(type_));
			break;
	}
	Messenger::exit("Grid::allocateArrays");
	return true;
}

// Update minimum / maximum based on supplied value
void Grid::setLimits(double d)
{
	if (d < minimum_) minimum_ = d;
	else if (d > maximum_) maximum_ = d;
	lowerPrimaryCutoff_ = 0.5 * maximum_;
	upperPrimaryCutoff_ = maximum_;
	lowerSecondaryCutoff_ = 0.5 * maximum_;
	upperSecondaryCutoff_ = maximum_;
}

// Calculate sums
void Grid::calculateSums()
{
	Messenger::enter("Grid::calculateSums");
	int i,j,k;
	double** data2, *data1;
	totalPositiveSum_ = 0.0;
	totalNegativeSum_ = 0.0;
	partialPrimarySum_ = 0.0;
	partialSecondarySum_ = 0.0;
	if (type_ == Grid::RegularXYZData)
	{
		for (i = 0; i < nXYZ_.x; i++)
		{
			data2 = data3d_[i];
			for (j = 0; j<nXYZ_.y; j++)
			{
				data1 = data2[j];
				for (k = 0; k<nXYZ_.z; k++)
				{
					if (data1[k] > 0) totalPositiveSum_ += data1[k];
					else totalNegativeSum_ += data1[k];
					// Calculate cutoff sums
					if (withinPrimaryCutoff(data1[k])) partialPrimarySum_ += fabs(data1[k]);
					if (withinSecondaryCutoff(data1[k])) partialSecondarySum_ += fabs(data1[k]);
				}
			}
		}
	}
	else if (type_ == Grid::RegularXYData)
	{
		for (i = 0; i < nXYZ_.x; i++)
		{
			data1 = data2d_[i];
			for (j = 0; j<nXYZ_.y; j++)
			{
				if (data1[j] > 0) totalPositiveSum_ += data1[j];
				else totalNegativeSum_ += data1[j];
				if (withinPrimaryCutoff(data1[j])) partialPrimarySum_ += fabs(data1[j]);
				if (withinSecondaryCutoff(data1[j])) partialSecondarySum_ += fabs(data1[j]);
			}
		}
	}
	Messenger::exit("Grid::calculateSums");
}

// Calculate bounding lower-left and upper-right corners
void Grid::calculateBounds()
{
	Messenger::enter("Grid::calculateBounds");
	// How we calculate bounds depends on grid type
	Vec3<double> v;
	switch (type_)
	{
		case (Grid::RegularXYData):
		case (Grid::RegularXYZData):
			// Determine resulting coordinates for points at each corner of unit cube
			lowerLeftCorner_.zero();
			upperRightCorner_.zero();
			for (int n=0; n<8; ++n)
			{
				v = cell_.axes().transform(n&1, n&2, n&4);
				for (int m=0; m<3; ++m)
				{
					if (v.get(m) < lowerLeftCorner_.get(m)) lowerLeftCorner_.set(m,v.get(m));
					if (v.get(m) > upperRightCorner_.get(m)) upperRightCorner_.set(m,v.get(m));
				}
			}
			break;
		case (Grid::FreeXYZData):
			// Search through all gridpoint data to find limits
			lowerLeftCorner_.zero();
			upperRightCorner_.zero();
			for (GridPoint* gp = gridPoints_.first(); gp != NULL; gp = gp->next)
			{
				v = gp->r();
				for (int m=0; m<3; ++m)
				{
					if (v.get(m) < lowerLeftCorner_.get(m)) lowerLeftCorner_.set(m,v.get(m));
					if (v.get(m) > upperRightCorner_.get(m)) upperRightCorner_.set(m,v.get(m));
				}
			}
			break;
		default:
			printf("Internal Error : Don't know how to calculate bounds for grid data type %i\n", type_);
			break;
	}
	boundsLog_ = log_;
	Messenger::exit("Grid::calculateBounds");
}

// Calculate sum of points between given cutoffs
double Grid::sum(double lowerCutoff, double upperCutoff)
{
	// Get sum between specified points
	int x, y, z;
	double sum = 0.0;
	if (type_ == Grid::RegularXYZData)
	{
		for (x=0; x<nXYZ_.x; x++)
		{
			for (y=0; y<nXYZ_.y; y++)
			{
				for (z=0; z<nXYZ_.z; z++)
				{
					if (data3d_[x][y][z] < lowerCutoff) continue;
					if (data3d_[x][y][z] > upperCutoff) continue;
					sum += data3d_[x][y][z];
				}
			}
		}
	}
	else
	{
		for (x=0; x<nXYZ_.x; x++)
		{
			for (y=0; y<nXYZ_.y; y++)
			{
				if (data2d_[x][y] < lowerCutoff) continue;
				if (data2d_[x][y] > upperCutoff) continue;
				sum += data2d_[x][y];
			}
		}
	}
	return sum;
}


// Calculate lower (or upper) cutoff to give requested percentage of grid encapsulated in displayed surface
bool Grid::calculateCutoff(double percentage, bool upperCutoff, bool secondary, double& newCutoff)
{
	const int nBins = 100, maxPasses = 10;
	double binDelta, histogramStart;
	Array<double> histogram(nBins), fractions(nBins);
	double point;
	int bin, x, y, z, n, m, lowBin, highBin;

	// Set initial histogram limits
	histogramStart = minimum_;
	if (secondary)
	{
		if (upperCutoff) binDelta = (maximum_ - lowerSecondaryCutoff_) / nBins;
		else binDelta = (upperSecondaryCutoff_ - minimum_) / nBins;
		newCutoff = (upperCutoff ? upperSecondaryCutoff_ : lowerSecondaryCutoff_);
	}
	else
	{
		if (upperCutoff) binDelta = (maximum_ - lowerPrimaryCutoff_) / nBins;
		else binDelta = (upperPrimaryCutoff_ - minimum_) / nBins;
		newCutoff = (upperCutoff ? upperPrimaryCutoff_ : lowerPrimaryCutoff_);
	}

	// Loop over passes
	for (int pass = 0; pass < maxPasses; ++pass)
	{
		// Zero histogram
		histogram = 0.0;

		// Bin grid data
		if (type_ == Grid::RegularXYZData)
		{
			for (x=0; x<nXYZ_.x; ++x)
			{
				for (y=0; y<nXYZ_.y; ++y)
				{
					for (z=0; z<nXYZ_.z; ++z)
					{
						point = data3d_[x][y][z];
						// Ignore points above / below relevant cutoffs
						if (upperCutoff)
						{
							if (point < (secondary ? lowerSecondaryCutoff_ : lowerPrimaryCutoff_)) continue;
						}
						else if (point > (secondary ? upperSecondaryCutoff_ : upperPrimaryCutoff_)) continue;
						bin = (point - histogramStart) / binDelta;
						if (bin < 0) bin = 0;
						else if (bin >= nBins) bin = nBins-1;
						histogram[bin] += fabs(point);
					}
				}
			}
		}
		else if (type_ == Grid::RegularXYData)
		{
			for (x=0; x<nXYZ_.x; ++x)
			{
				for (y=0; y<nXYZ_.y; ++y)
				{
					point = data2d_[x][y];
					// Ignore points above / below relevant cutoffs
					if (upperCutoff)
					{
						if (point < (secondary ? lowerSecondaryCutoff_ : lowerPrimaryCutoff_)) continue;
					}
					else if (point > (secondary ? upperSecondaryCutoff_ : upperPrimaryCutoff_)) continue;
					bin = (point - histogramStart) / binDelta;
					if (bin < 0) bin = 0;
					else if (bin >= nBins) bin = nBins-1;
					histogram[bin] += fabs(point);
				}
			}
		}
		else if (type_ == Grid::FreeXYZData)
		{
			printf("TODO - Not implemented yet!\n");
		}

		// Calculate fractions of data represented by each bin (cutoff)
		fractions = 0.0;
		for (n=0; n<nBins; ++n)
		{
			for (m=0; m<=n; ++m) fractions[m] += histogram[n];
		}
		fractions /= totalAbsoluteSum();

		// TEST print out bin values and sums
// 		for (n=0; n<nBins; ++n) printf("HistoBin %i (x=%f) grid% = %f\n", n, histogramStart+binDelta*n, fractions[n]*100.0);

		// Find new limits for next pass
		for (highBin=0; highBin<nBins; ++highBin) if ((fractions[highBin]*100.0) < percentage) break;
		if (highBin >= nBins)
		{
			// Target cutoff is beyond the current maximum limit of the histogram?
			Messenger::print("Unable to find suitable cutoff for requested percentage (%f)", percentage);
			return false;
		}
		for (lowBin=highBin-1; lowBin>=0; --lowBin) if ((fractions[lowBin]*100.0) > percentage) break;
		if (lowBin < 0)
		{
			// Target cutoff is beyond the current minimum limit of the histogram?
			Messenger::print("Unable to find suitable cutoff for requested percentage (%f)", percentage);
			return false;
		}

		// Set new cutoff limits for next pass
// 		printf("Low/Hi bins are %i and %i\n", lowBin, highBin);
		histogramStart = histogramStart+binDelta*lowBin;
		binDelta = (binDelta*(highBin-lowBin)) / nBins;
// 		printf("New start and delta are %f and %f\n", histogramStart, binDelta);

		// Done?
		if (binDelta < (maximum_-minimum_)*0.00001) break;
	}

	newCutoff = histogramStart;

	return true;
}

// Return pointer to the underlying cell structure
UnitCell* Grid::cell()
{
	return &cell_;
}

// Set spacing for a cubic grid
void Grid::setAxes(double r)
{
	cell_.set( Vec3<double>(r,r,r), Vec3<double>(90.0, 90.0, 90.0) );
	logChange();
}

// Set spacing for an orthorhombic grid
void Grid::setAxes(const Vec3<double> v)
{
	cell_.set( v, Vec3<double>(90.0, 90.0, 90.0) );
	logChange();
}

// Set spacing for a parallelepiped grid
void Grid::setAxes(const Matrix axes)
{
	cell_.set(axes);
	logChange();
}

// Return the Grid axes
Matrix Grid::axes() const
{
	return cell_.axes();
}

// Return lengths of cell axes
Vec3<double> Grid::lengths() const
{
	return cell_.lengths();
}

// Set data origin
void Grid::setOrigin(const Vec3<double> v)
{
	origin_ = v;
	logChange();
}

// Return the origin of the Grid data
Vec3<double> Grid::origin() const
{
	return origin_;
}

// Set data minima
void Grid::setDataMinimum(const Vec3<double> v)
{
	dataMinimum_ = v;
	logChange();
}

// Return the data minimum
Vec3<double> Grid::dataMinimum() const
{
	return dataMinimum_;
}

// Set data maxima
void Grid::setDataMaximum(const Vec3<double> v)
{
	dataMaximum_ = v;
	logChange();
}

// Return the data maximum
Vec3<double> Grid::dataMaximum() const
{
	return dataMaximum_;
}

// Set whether axis is visible
void Grid::setAxisVisible(int id, bool visible)
{
	axisVisible_[id] = visible;
}

// Return whether axis is visible
bool Grid::isAxisVisible(int id)
{
	return axisVisible_[id];
}

// Return axis visibility array
bool *Grid::axisVisible()
{
	return axisVisible_;
}

// Set axis position element
void Grid::setAxisPosition(int axis, int el, double value)
{
	axisPosition_[axis].set(el, value);
}

// Return axis position
Vec3<double> Grid::axisPosition(int axis)
{
	return axisPosition_[axis];
}

// Set axis major spacing
void Grid::setAxisMajorSpacing(int axis, double value)
{
	axisMajorSpacing_.set(axis, value);
}

// Return axis position
Vec3<double> Grid::axisMajorSpacing()
{
	return axisMajorSpacing_;
}

// Set axis major spacing
void Grid::setAxisMinorTicks(int axis, int value)
{
	axisMinorTicks_.set(axis, value);
}

// Return axis position
Vec3<int> Grid::axisMinorTicks()
{
	return axisMinorTicks_;
}

// Return number of points in data series
Vec3<int> Grid::nXYZ() const
{
	return nXYZ_;
}

// Return number of points in data series
int Grid::nPoints() const
{
	switch (type_)
	{
		case (Grid::RegularXYData):
			return nXYZ_.x*nXYZ_.y;
			break;
		case (Grid::RegularXYZData):
			return nXYZ_.x*nXYZ_.y*nXYZ_.z;
			break;
		case (Grid::FreeXYZData):
			return gridPoints_.nItems();
			break;
		default:
			break;
	}

	return 0;
}

// Return minimum value in data[]
double Grid::minimum() const
{
	return minimum_;
}

// Return maximum value in data[]
double Grid::maximum() const
{
	return maximum_;
}

// Return LLC bounding values, calculating first if necessary
Vec3<double> Grid::lowerLeftCorner()
{
	if (boundsLog_ != log_) calculateBounds();
	return lowerLeftCorner_;
}

// Return URC bounding values, calculating first if necessary
Vec3<double> Grid::upperRightCorner()
{
	if (boundsLog_ != log_) calculateBounds();
	return upperRightCorner_;
}

// Set lower isovalue cutoff for primary surface
void Grid::setLowerPrimaryCutoff(double d)
{
	lowerPrimaryCutoff_ = d;
	logChange();
}

// Return lower isovalue cutoff for primary surface
double Grid::lowerPrimaryCutoff() const
{
	return lowerPrimaryCutoff_;
}

// Set upper isovalue cutoff for primary surface
void Grid::setUpperPrimaryCutoff(double d)
{
	upperPrimaryCutoff_ = d;
	logChange();
}

// Set primary cutoff as view percentage
double Grid::setPrimaryCutoffAsViewPercentage(double d, bool upperCutoff)
{
	// First, check limits of percentage
	if ((d < 0.0) || (d > 100.0))
	{
		Messenger::print("Percentage value is not a real percentage (%f).\n", d);
		return 100.0*partialPrimarySum()/totalAbsoluteSum();
	}

	// Calculate new cutoff
	double newCutoff;
	if (calculateCutoff(d, upperCutoff, false, newCutoff))
	{
		if (upperCutoff) setUpperPrimaryCutoff(newCutoff);
		else setLowerPrimaryCutoff(newCutoff);
	}

	return 100.0*partialPrimarySum()/totalAbsoluteSum();
}

// Return upper isovalue cutoff for primary surface
double Grid::upperPrimaryCutoff() const
{
	return upperPrimaryCutoff_;
}

// Return whether supplied number is within primary cutoff range
bool Grid::withinPrimaryCutoff(double d) const
{
	if (upperPrimaryCutoff_ > lowerPrimaryCutoff_)
	{
		if ((d >= lowerPrimaryCutoff_) && (d <= upperPrimaryCutoff_)) return true;
	}
	else if ((d >= upperPrimaryCutoff_) && (d <= lowerPrimaryCutoff_)) return true;
	return false;
}

// Set lower isovalue cutoff for secondary surface
void Grid::setLowerSecondaryCutoff(double d)
{
	lowerSecondaryCutoff_ = d;
	logChange();
}

// Return lower isovalue cutoff for secondary surface
double Grid::lowerSecondaryCutoff() const
{
	return lowerSecondaryCutoff_;
}

// Set upper isovalue cutoff for secondary surface
void Grid::setUpperSecondaryCutoff(double d)
{
	upperSecondaryCutoff_ = d;
	logChange();
}

// Set secondary lower/upper cutoff based on view percentage
double Grid::setSecondaryCutoffAsViewPercentage(double d, bool upperCutoff)
{
	// First, check limits of percentage
	if ((d < 0.0) || (d > 100.0))
	{
		Messenger::print("Percentage value is not a real percentage (%f).\n", d);
		return 100.0*partialSecondarySum()/totalAbsoluteSum();
	}

	// Calculate new cutoff
	double newCutoff;
	if (calculateCutoff(d, upperCutoff, true, newCutoff))
	{
		if (upperCutoff) setUpperSecondaryCutoff(newCutoff);
		else setLowerSecondaryCutoff(newCutoff);
	}

	return 100.0*partialSecondarySum()/totalAbsoluteSum();
}

// Return upper isovalue cutoff for secondary surface
double Grid::upperSecondaryCutoff() const
{
	return upperSecondaryCutoff_;
}

// Return whether supplied number is within secondary cutoff range
bool Grid::withinSecondaryCutoff(double d) const
{
	if (upperSecondaryCutoff_ > lowerSecondaryCutoff_)
	{
		if ((d >= lowerSecondaryCutoff_) && (d <= upperSecondaryCutoff_)) return true;
	}
	else if ((d >= upperSecondaryCutoff_) && (d <= lowerSecondaryCutoff_)) return true;
	return false;
}

// Return 3D data array
double** *Grid::data3d()
{
	return data3d_;
}

// Return 2D data array
double** Grid::data2d()
{
	return data2d_;
}

// Return first gridpoint in list
GridPoint* Grid::gridPoints()
{
	return gridPoints_.first();
}

// Set loop ordering
void Grid::setLoopOrder(int n, int xyz)
{
	loopOrder_.set(n,xyz);
}

// Return loop ordering 
Vec3<int> Grid::loopOrder()
{
	return loopOrder_;
}

// Set whether to use data2d_ values for the z-component of the 2D surface
void Grid::setUseDataForZ(bool b)
{
	useDataForZ_ = b;
	logChange();
}

// Whether to use data2d_ value sfor z-component of 2D surface
bool Grid::useDataForZ() const
{
	return useDataForZ_;
}

// Return the total positive sum of the grid (calculated when drawn)
double Grid::totalPositiveSum()
{
	if (log_ != sumPoint_)
	{
		calculateSums();
		sumPoint_ = log_;
	}
	return totalPositiveSum_;
}

// Return the partial positive sum of the grid, determined by cutoffs (calculated when drawn)
double Grid::partialPrimarySum()
{
	if (log_ != sumPoint_)
	{
		calculateSums();
		sumPoint_ = log_;
	}
	return partialPrimarySum_;
}

// Return the total negative sum of the grid
double Grid::totalNegativeSum()
{
	if (log_ != sumPoint_)
	{
		calculateSums();
		sumPoint_ = log_;
	}
	return totalNegativeSum_;
}

// Return the partial negative sum of the grid, determined by cutoffs
double Grid::partialSecondarySum()
{
	if (log_ != sumPoint_)
	{
		calculateSums();
		sumPoint_ = log_;
	}
	return partialSecondarySum_;
}

// Return the total absolute sum of the grid
double Grid::totalAbsoluteSum()
{
	if (log_ != sumPoint_)
	{
		calculateSums();
		sumPoint_ = log_;
	}
	return totalPositiveSum_ + fabs(totalNegativeSum_);
}

/*
 * Style
 */

// Log changes
void Grid::logChange()
{
	++log_;
	if (parent_ != NULL) parent_->logChange(Log::Grids);
}

// Set whether the surface is visible
void Grid::setVisible(bool v)
{
	visible_ = v;
	logChange();
}

// Return whether the surface is visible
bool Grid::isVisible() const
{
	return visible_;
}

// Set the rendering style of the primary surface
void Grid::setPrimaryStyle(Grid::SurfaceStyle ss)
{
	primaryStyle_ = ss;
	logChange();
}

// Return the rendering style of the primary surface
Grid::SurfaceStyle Grid::primaryStyle() const
{
	return primaryStyle_;
}

// Set the rendering style of the secondary surface
void Grid::setSecondaryStyle(Grid::SurfaceStyle ss)
{
	secondaryStyle_ = ss;
	logChange();
}

// Return the rendering style of the primary surface
Grid::SurfaceStyle Grid::secondaryStyle() const
{
	return secondaryStyle_;
}

// Set colour of primary surface
void Grid::setPrimaryColour(double r, double g, double b, double a)
{
	primaryColour_[0] = r;
	primaryColour_[1] = g;
	primaryColour_[2] = b;
	if (a >= 0.0) primaryColour_[3] = a;
	logChange();
}

// Return the primary grid colour
double* Grid::primaryColour()
{
	return primaryColour_;
}

// Copy the colour of the primary surface
void Grid::copyPrimaryColour(Vec4<GLfloat>& col)
{
	col.x = (GLfloat) primaryColour_[0];
	col.y = (GLfloat) primaryColour_[1];
	col.z = (GLfloat) primaryColour_[2];
	col.w = (GLfloat) primaryColour_[3];
}

// Set the colour of the secondary surface
void Grid::setSecondaryColour(double r, double g, double b, double a)
{
	secondaryColour_[0] = r;
	secondaryColour_[1] = g;
	secondaryColour_[2] = b;
	if (a >= 0.0) secondaryColour_[3] = a;
	logChange();
}

// Return the colour of the secondary surface
double* Grid::secondaryColour()
{
	return secondaryColour_;
}

// Copy the colour of the secondary surface
void Grid::copySecondaryColour(Vec4<GLfloat>& col)
{
	col.x = (GLfloat) secondaryColour_[0];
	col.y = (GLfloat) secondaryColour_[1];
	col.z = (GLfloat) secondaryColour_[2];
	col.w = (GLfloat) secondaryColour_[3];
}

// Set alpha value of the primary colour
void Grid::setPrimaryAlpha(double a)
{
	primaryColour_[3] = a;
	logChange();
}

// Return alpha value of the primary colour
double Grid::primaryAlpha() const
{
	return primaryColour_[3];
}

// Set alpha value of the secondary colour
void Grid::setSecondaryAlpha(double a)
{
	secondaryColour_[3] = a;
	logChange();
}

// Return alpha value of the secondary colour
double Grid::secondaryAlpha() const
{
	return secondaryColour_[3];
}

// Set the colourscale associated with the data
void Grid::setColourScale(int id)
{
	// Check range of supplied id
	if ((id < 0) || (id > 9))
	{
		// Remove link in old colourscale if necessary
		if (useColourScale_) prefs.colourScale[colourScale_].breakLink(this);
		useColourScale_ = false;
		logChange();
		return;
	}
	// Remove old colourscale link (if one existed)
	if (useColourScale_) prefs.colourScale[colourScale_].breakLink(this);
	colourScale_ = id;
	logChange();
	prefs.colourScale[colourScale_].addLink(this);
	useColourScale_ = true;
	int i, j, k;
	double** data2, *data1;
	// Adjust the colour scale to encompass all grid values...
	if (type_ == Grid::RegularXYZData)
	{
		for (i = 0; i < nXYZ_.x; i++)
		{
			data2 = data3d_[i];
			for (j = 0; j<nXYZ_.y; j++)
			{
				data1 = data2[j];
				for (k = 0; k<nXYZ_.z; k++) prefs.colourScale[colourScale_].adjustRange(data1[k]);
			}
		}
	}
	else if (type_ == Grid::RegularXYData)
	{
		for (i = 0; i < nXYZ_.x; i++)
		{
			data1 = data2d_[i];
			for (j = 0; j<nXYZ_.y; j++) prefs.colourScale[colourScale_].adjustRange(data1[j]);
		}
	}
}

// Return the colourscale associated with the data
int Grid::colourScale() const
{
	return colourScale_;
}

// Set whether the surface uses the defined colour scale or not
void Grid::setUseColourScale(bool b)
{
	useColourScale_ = b;
	logChange();
}

// Whether the surface uses the defined colour scale or not
bool Grid::useColourScale() const
{
	return useColourScale_;
}

// Set whether to use both signs of a symmetric isovalue distribution
void Grid::setUseSecondary(bool b)
{
	useSecondary_ = b;
	logChange();
}

// Returns whether to use both signs of a symmetric isovalue distribution
bool Grid::useSecondary() const
{
	return useSecondary_;
}

// Set whether the grid data is periodic
void Grid::setPeriodic(bool b)
{
	periodic_ = b;
	logChange();
}

// Return whether the grid data is periodic
bool Grid::periodic() const
{
	return periodic_;
}

// Set whether to outline grid volume
void Grid::setOutlineVolume(bool b)
{
	outlineVolume_ = b;
	logChange();
}

// Return whether to outline grid volume
bool Grid::outlineVolume() const
{
	return outlineVolume_;
}

// Set shift amount for grid
void Grid::setShift(int i, int j, int k)
{
	shift_.set(i,j,k);
	logChange();
}

// Set single shift amount
void Grid::setShift(int id, int i)
{
	shift_.set(id, i);
	logChange();
}

// Return shift amount
Vec3<int> Grid::shift()
{
	return shift_;
}

// Whether to fill enclosed volume
void Grid::setFillEnclosedVolume(bool b)
{
	fillEnclosedVolume_ = b;
	logChange();
}

// Return whether to fill enclosed volume
bool Grid::fillEnclosedVolume()
{
	return fillEnclosedVolume_;
}

/*
 * Rendering
 */

// Return voxel matrix, offset to grid origin
Matrix Grid::voxelMatrix()
{
	Matrix A;
	A.createTranslation(origin_);
	A.multiplyRotation(cell_.axes());
	return A;
}

// Return primiary primitive
Primitive& Grid::primaryPrimitive()
{
	return primaryPrimitive_;
}

// Send primary primitive to GL, regenerating if necessary
void Grid::sendPrimaryPrimitive()
{
	if (primaryPrimitivePoint_ != log_)
	{
		// Create primitive if we don't already have one
		if (!primaryPrimitive_.registeredAsDynamic()) PrimitiveSet::registerDynamicPrimitive(&primaryPrimitive_);

		Vec4<GLfloat> colour(primaryColour_[0], primaryColour_[1], primaryColour_[2], primaryColour_[3]);
		if (type_ == Grid::RegularXYData) primaryPrimitive_.createSurface(this, colour, useColourScale_ ? colourScale_ : -1);
		else if (type_ == Grid::RegularXYZData) primaryPrimitive_.marchingCubes(this, lowerPrimaryCutoff_, upperPrimaryCutoff_, useColourScale_ ? colourScale_ : -1);
		else printf("Don't know how to create a primitive based on this type of Grid data (%s)\n", Grid::gridType(type_));
		primaryPrimitivePoint_ = log_;
	}

	// Set colour
	if (!useColourScale_)
	{
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		glEnable(GL_COLOR_MATERIAL);
		glColor4f(primaryColour_[0], primaryColour_[1], primaryColour_[2], primaryColour_[3]);
	}

	// Render it
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	if (primaryStyle_ == Grid::SolidSurface) primaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_FILL, true, !useColourScale_, primaryColour_);
	else if (primaryStyle_ == Grid::MeshSurface) primaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_LINE, false, !useColourScale_, primaryColour_);
	else if (primaryStyle_ == Grid::PointSurface)
	{
		glPointSize(3.0f);
		primaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_POINT, false, !useColourScale_, primaryColour_);
	}
}

// Return primiary primitive
Primitive& Grid::secondaryPrimitive()
{
	return secondaryPrimitive_;
}

// Send primary primitive to GL, regenerating if necessary
void Grid::sendSecondaryPrimitive()
{
	if (secondaryPrimitivePoint_ != log_)
	{
		// Create primitive if we don't already have one
		if (!secondaryPrimitive_.registeredAsDynamic()) PrimitiveSet::registerDynamicPrimitive(&secondaryPrimitive_);

		Vec4<GLfloat> colour(secondaryColour_[0], secondaryColour_[1], secondaryColour_[2], secondaryColour_[3]);
		secondaryPrimitive_.marchingCubes(this, lowerSecondaryCutoff_, upperSecondaryCutoff_, useColourScale_ ? colourScale_ : -1);
		if (type_ == Grid::RegularXYData) secondaryPrimitive_.createSurface(this, colour, useColourScale_ ? colourScale_ : -1);
		else if (type_ == Grid::RegularXYZData) secondaryPrimitive_.marchingCubes(this, lowerSecondaryCutoff_, upperSecondaryCutoff_, useColourScale_ ? colourScale_ : -1);
		else printf("Don't know how to create a primitive based on this type of Grid data (%s)\n", Grid::gridType(type_));
		secondaryPrimitivePoint_ = log_;
	}

	// Set colour
	if (!useColourScale_)
	{
		glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
		glEnable(GL_COLOR_MATERIAL);
		glColor4f(secondaryColour_[0], secondaryColour_[1], secondaryColour_[2], secondaryColour_[3]);
	}

	// Render it
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	if (secondaryStyle_ == Grid::SolidSurface) secondaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_FILL, true, !useColourScale_, secondaryColour_);
	else if (secondaryStyle_ == Grid::MeshSurface) secondaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_LINES, false, !useColourScale_, secondaryColour_);
	else if (secondaryStyle_ == Grid::PointSurface)
	{
		glPointSize(3.0f);
		secondaryPrimitive_.sendToGL(QOpenGLContext::currentContext(), GL_POINTS, false, !useColourScale_, secondaryColour_);
	}
}

/*
 * Data Interace
 */

// Set specific point in data array
void Grid::setData(int x, int y, int z, double d)
{
	// Check limits against npoints vector
	if ((x < 0) || (x >= nXYZ_.x))
	{
		Messenger::print("X index %i is outside array bounds (0 to %i) for grid data.", x, nXYZ_.x-1);
		return;
	}
	else if ((y < 0) || (y >= nXYZ_.y))
	{
		Messenger::print("Y index %i is outside array bounds (0 to %i) for grid data.", y, nXYZ_.y-1);
		return;
	}
	else if ((type_ == Grid::RegularXYData) && ((z < 0) || (z >= nXYZ_.z)))
	{
		Messenger::print("Z index %i is outside array bounds (0 to %i) for grid data.", z, nXYZ_.z-1);
		return;
	}
	// Okay, so store data
	if (type_ == Grid::RegularXYZData) data3d_[x][y][z] = d;
	else data2d_[x][y] = d;
	// Set new minimum / maximum
	setLimits(d);
}

// Set 'next' point in data array
void Grid::setNextData(double d)
{
	// Check limit
	if (dataFull_ == true)
	{
		Messenger::print("Grid::setNextData - Array already full.");
		return;
	}
	// Set current point referenced by currentpoint and increase it
	if (type_ == Grid::RegularXYZData)
	{
		data3d_[currentPoint_.x][currentPoint_.y][currentPoint_.z] = d;
		currentPoint_.set(loopOrder_.x, currentPoint_.get(loopOrder_.x) + 1);
		if (currentPoint_.get(loopOrder_.x) == nXYZ_.get(loopOrder_.x))
		{
			currentPoint_.set(loopOrder_.x, 0);
			currentPoint_.set(loopOrder_.y, currentPoint_.get(loopOrder_.y) + 1);
			if (currentPoint_.get(loopOrder_.y) == nXYZ_.get(loopOrder_.y))
			{
				currentPoint_.set(loopOrder_.y, 0);
				currentPoint_.set(loopOrder_.z, currentPoint_.get(loopOrder_.z) + 1);
				if (currentPoint_.get(loopOrder_.z) == nXYZ_.get(loopOrder_.z)) dataFull_ = true;
			}
		}
	}
	else
	{
		data2d_[currentPoint_.x][currentPoint_.y] = d;
		currentPoint_.set(loopOrder_.x, currentPoint_.get(loopOrder_.x) + 1);
		if (currentPoint_.get(loopOrder_.x) == nXYZ_.get(loopOrder_.x))
		{
			currentPoint_.set(loopOrder_.x, 0);
			currentPoint_.set(loopOrder_.y, currentPoint_.get(loopOrder_.y) + 1);
			if (currentPoint_.get(loopOrder_.y) == nXYZ_.get(loopOrder_.y)) dataFull_ = true;
		}
	}
	// Set new minimum / maximum
	setLimits(d);
}

// Add free data point
void Grid::addFreePoint(double x, double y, double z, double value)
{
	GridPoint* gp = gridPoints_.add();
	gp->r().set(x, y, z);
	gp->setValue(value);
	logChange();
	type_ = Grid::FreeXYZData;
}

/*
 * Transformations
 */

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

// Modify region of data (adjacent gridpoints) (recursive)
int Grid::modifyRegion(int startX, int startY, int startZ, double minValue, double maxValue, double newValue, bool periodic)
{
	int i, j, k, x, y, z, nCells = 0;
	double value;
	QList< Vec3<int> > queue;
	Vec3<int> v;

	if (type_ == Grid::RegularXYZData)
	{
		// Seed queue with the target cell...
		queue.append(Vec3<int>(startX, startY, startZ));

		// Loop over queued points
		while (queue.count() > 0)
		{
			// Grab last queued point
			Vec3<int>& vec = queue.last();

			// If this cell value is not in the range required, remove point from queue and continue
			value = data3d_[vec.x][vec.y][vec.z];
			if ((value < minValue) || (value > maxValue))
			{
				queue.removeLast();
				continue;
			}

			// It is in the range required, so change its value...
			setData(vec.x, vec.y, vec.z, newValue);
			// ...increase nCells...
			++nCells;
			// ...add valid adjacent cells to the queue...
			for (x=0; x<3; ++x)
			{
				for (y=-1; y < 2; y+=2)
				{
					v = vec;
					v.add(x, y);
					// Fold position if necessary
					if (v[x] < 0) { if (periodic) v.set(x,nXYZ_[x]-1); else continue; }
					else if (v[x] >= nXYZ_[x]) { if (periodic) v.set(x,0); else continue; }
					value = data3d_[v.x][v.y][v.z];
					if ((value < minValue) || (value > maxValue)) continue;
					else
					{
						// Insert a new item at the beginning of the list, so we can still removeLast on the original item later...
						queue.prepend(Vec3<int>(v.x,v.y,v.z));
// 						printf("Queueing adjacent cell %i %i %i \n", x, y, z);
					}
				}
			}
			// ...and finally remove it from the list
			queue.removeLast();
		}
	}
	else
	{
		// Seed queue with the target cell...
		queue.append(Vec3<int>(startX, startY, 0));

		// Loop over queued points
		while (queue.count() > 0)
		{
			// Grab last queued point
			Vec3<int>& vec = queue.last();

			// If this cell value is not in the range required, remove point from queue and continue
			value = data2d_[vec.x][vec.y];
			if ((value < minValue) || (value > maxValue))
			{
				queue.removeLast();
				continue;
			}

			// It is in the range required, so change its value...
			setData(vec.x, vec.y, 0, newValue);
			// ...increase nCells...
			++nCells;
			// ...add valid adjacent cells to the queue...
			for (x=0; x<2; ++x)
			{
				for (y=-1; y < 2; y+=2)
				{
					v = vec;
					v.add(x, y);
					// Fold position if necessary
					if (v[x] < 0) { if (periodic) v.set(x,nXYZ_[x]-1); else continue; }
					else if (v[x] >= nXYZ_[x]) { if (periodic) v.set(x,0); else continue; }
					value = data2d_[v.x][v.y];
					if ((value < minValue) || (value > maxValue)) continue;
					else
					{
						// Insert a new item at the beginning of the list, so we can still removeLast on the original item later...
						queue.prepend(Vec3<int>(v.x,v.y,0));
// 						printf("Queueing adjacent cell %i %i %i \n", x, y, z);
					}
				}
			}
			// ...and finally remove it from the list
			queue.removeLast();
		}
	}
	return nCells;
}
