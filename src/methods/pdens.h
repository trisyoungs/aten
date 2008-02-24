/*
	*** Probability density calculation
	*** src/methods/pdens.h
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

#ifndef H_PDENS_H
#define H_PDENS_H

#include "methods/quantity.h"
#include "templates/vector3.h"

// Forward Declarations
class site;

// Radial Distribution Function Class
class pdens : public calculable
{
	public:
	// Constructor / Destructor
	pdens();
	~pdens();

	/*
	// Sites
	*/
	private:
	// Centres involved in distribution
	site *sites[2];
	
	public:
	// Set site involved in distribution
	void set_site(int, site*);
	// Get site involved in distribution
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
	// Number of gridpoints in each +ve OR -ve direction
	int nsteps;
	// Total number of gridpoints along each cartesian axis
	int totalsteps;
	// Step size between gridpoints
	double stepsize;

	public:
	// Set distribution data
	void set_range(double, int);
	// Get stepsize
	double get_stepsize() { return stepsize; }
	// Get number of bins
	int get_nsteps() { return nsteps; }

	/*
	// Data
	*/
	private:
	// Distribution
	double ***data;
	// Add point to data array
	void add_point(vec3<int>&);
	// Count for number of added data (i.e. nframes)
	int acc;
};

#endif
