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

#ifndef ATEN_PDENS_H
#define ATEN_PDENS_H

#include "methods/calculable.h"
#include "templates/vector3.h"

// Forward Declarations
class Site;

// Radial Distribution Function Class
class Pdens : public Calculable
{
	public:
	// Constructor / Destructor
	Pdens();
	~Pdens();

	/*
	// Sites
	*/
	private:
	// Centres involved in distribution
	Site *sites_[2];
	
	public:
	// Set site involved in distribution
	void setSite(int, Site*);
	// Get site involved in distribution
	Site *site(int);

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
	// Number of gridpoints in each +ve OR -ve direction
	int nSteps_;
	// Total number of gridpoints along each cartesian axis
	int totalSteps_;
	// Step size between gridpoints
	double stepSize_;

	public:
	// Set distribution data
	void setRange(double, int);
	// Get stepsize
	double stepSize();
	// Get number of bins
	int nSteps();

	/*
	// Data
	*/
	private:
	// Distribution
	double ***data_;
	// Add point to data array
	void addPoint(Vec3<int>&);
	// Count for number of added data (i.e. nframes)
	int nAdded_;
};

#endif
