/*
	*** Molecule geometry calculation
	*** src/methods/geometry.h
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

#ifndef ATEN_GEOMETRY_H
#define ATEN_GEOMETRY_H

#include "methods/calculable.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Site;

// Geometry Class
class Geometry : public Calculable
{
	public:
	// Constructor / Destructor
	Geometry();
	~Geometry();

	/*
	// Sites
	*/
	private:
	// Centres involved in geometry measurement
	Site* sites_[4];
	// Number of continuous defined sites
	int nSites_;
	
	public:
	// Set site involved in geometry measurement
	void setSite(int, Site*);
	// Get site involved in geometry measurement
	Site* site(int);

	/*
	// Methods
	*/
	public:
	// Initialise structure
	bool initialise();
	// Accumulate quantity data from supplied config
	void accumulate(Model*);
	// Finalise data
	void finalise(Model*);
	// Save data
	bool save();

	/*
	// Data Description
	*/
	private:
	// Number of bins in histogram
	int nBins_;
	// Minimum / maximum coordinates and range of data
	double lower_, upper_, range_;
	// Step size between bins
	double binWidth_;

	public:
	// Set histogram range
	void setRange(double, double, int);
	// Get lower limit
	double lower() { return lower_; }
	// Get bin width
	double binWidth() { return binWidth_; }
	// Get number of bins
	int nBins() { return nBins_; }

	/*
	// Data
	*/
	private:
	// Histogram function
	double* data_;
	// Count for number of added data (i.e. nframes)
	int nAdded_;
};

ATEN_END_NAMESPACE

#endif
