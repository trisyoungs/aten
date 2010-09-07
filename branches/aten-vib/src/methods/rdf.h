/*
	*** Radial distribution function calculation
	*** src/methods/rdf.h
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

#ifndef ATEN_RDF_H
#define ATEN_RDF_H

#include "methods/calculable.h"

// Forward Declarations
class Site;

// Radial Distribution Function Class
class Rdf : public Calculable
{
	public:
	// Constructor / Destructor
	Rdf();
	~Rdf();

	/*
	// Sites
	*/
	private:
	// Centres involved in RDF
	Site *sites_[2];
	
	public:
	// Set site involved in RDF
	void setSite(int, Site*);
	// Get site involved in RDF
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
	// Number of bins in each dimension
	int nBins_;
	// Minimum / maximum coordinates and range of data
	double lower_, upper_, range_;
	// Step size between bins
	double binWidth_;

	public:
	// Set rdf range
	void setRange(double, double, int);
	// Get lower limit
	double lower();
	// Get stepsize
	double binWidth();
	// Get number of bins
	int nBins();

	/*
	// Data
	*/
	private:
	// Distribution function
	double *data_;
	// Count for number of added data (i.e. nframes)
	int nAdded_;
};

#endif
