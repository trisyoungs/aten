/*
	*** Measurement calculation
	*** src/methods/measure.h
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

#ifndef H_GEOMETRY_H
#define H_GEOMETRY_H

#include "methods/quantity.h"

// Forward Declarations
class site;

// Geometry Class
class geometry : public calculable
{
	public:
	// Constructor / Destructor
	geometry();
	~geometry();

	/*
	// Sites
	*/
	private:
	// Centres involved in geometry measurement
	site *sites[4];
	
	public:
	// Set site involved in geometry measurement
	void set_site(int, site*);
	// Get site involved in geometry measurement
	site *get_site(int);

	/*
	// Methods
	*/
	public:
	// Initialise structure
	bool initialise();
	// Accumulate quantity data from supplied config
	void accumulate(model*);
	// Finalise data
	void finalise(model*);
	// Save data
	bool save();

	/*
	// Data Description
	*/
	private:
	// Number of bins in histogram
	int nbins;
	// Minimum / maximum coordinates and range of data
	double lower, upper, range;
	// Step size between bins
	double binwidth;

	public:
	// Set histogram range
	void set_range(double, double, int);
	// Get lower limit
	double get_lower() { return lower; }
	// Get bin width
	double get_binwidth() { return binwidth; }
	// Get number of bins
	int get_nbins() { return nbins; }

	/*
	// Data
	*/
	private:
	// Histogram function
	double *data;
	// Count for number of added data (i.e. nframes)
	int acc;
};

#endif
