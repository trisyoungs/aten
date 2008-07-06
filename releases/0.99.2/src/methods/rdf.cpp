/*
	*** Radial distribution function calculation
	*** src/methods/rdf.cpp
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

#include "methods/rdf.h"
#include "classes/site.h"
#include "classes/pattern.h"
#include "model/model.h"

// Constructor
Rdf::Rdf()
{
	sites_[0] = NULL;
	sites_[1] = NULL;
	lower_ = 0.0;
	upper_ = 15.0;
	binWidth_ = 0.1;
	nBins_ = 150;
	range_ = 15.0;
	nAdded_ = 0;
	data_ = NULL;
}

// Destructor
Rdf::~Rdf()
{
	if (sites_[0] != NULL) delete sites_[0];
	if (sites_[1] != NULL) delete sites_[1];
	if (data_ != NULL) delete[] data_;
}

// Get lower limit
double Rdf::lower()
{
	return lower_;
}

// Get stepsize
double Rdf::binWidth()
{
	return binWidth_;
}

// Get number of bins
int Rdf::nBins()
{
	return nBins_;
}

// Set site
void Rdf::setSite(int i, Site *s)
{
	if (i < 2) sites_[i] = s;
	else printf("OUTOFRANGE:Rdf::set_site\n");
}

// Get site
Site *Rdf::site(int i)
{
	if (i < 2) return sites_[i];
	else printf("OUTOFRANGE:Rdf::set_site\n");
	return NULL;
}

// Set RDF range_
void Rdf::setRange(double d, double w, int n)
{
	lower_ = d;
	binWidth_ = w;
	nBins_ = n;
	upper_ = d + w*n;
	range_ = upper_ - lower_;
}

// Initialise structure
bool Rdf::initialise()
{
	msg.enter("Rdf::initialise");
	// Check site definitions....
	if ((sites_[0] == NULL) || (sites_[1] == NULL))
	{
		msg.print("Rdf::initialise - At least one site has NULL value.\n");
		msg.exit("Rdf::initialise");
		return FALSE;
	}
	// Create the data_ arrays
	data_ = new double[nBins_];
	for (int n=0; n<nBins_; n++) data_[n] = 0.0;
	msg.print("There are %i bins in rdf '%s', beginning at r = %f.\n", nBins_, name_.get(), lower_);
	nAdded_ = 0;
	msg.exit("Rdf::initialise");
	return TRUE;
}

// Accumulate quantity data_ from supplied model
void Rdf::accumulate(Model *sourcemodel)
{
	msg.enter("Rdf::accumulate");
	int m1, m2, bin;
	static Vec3<double> centre1, centre2, mimd;
	Cell *cell = sourcemodel->cell();
	// Loop over molecules for site1
	for (m1=0; m1 < sites_[0]->pattern()->nMols(); m1++)
	{
		// Get first centre
		centre1 = sites_[0]->calculateCentre(sourcemodel,m1);
		// Loop over molecules for site2
		for (m2 = 0; m2 < sites_[1]->pattern()->nMols(); m2++)
		{
			centre2 = sites_[1]->calculateCentre(sourcemodel,m2);
			// Calculate minimum image distance and bin
			mimd = cell->mimd(centre2,centre1);
			// Add distance to data_ array
			bin = int(mimd.magnitude() / binWidth_);
	//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
			if (bin < nBins_) data_[bin] += 1.0;
		}
	}
	// Increase nAdded_umulation counter
	nAdded_ ++;
	msg.exit("Rdf::accumulate");
}

// Finalise
void Rdf::finalise(Model *sourcemodel)
{
	msg.enter("Rdf::finalise");
	int n;
	double factor, r1, r2, numDensity;
	// Normalise the rdf w.r.t. number of frames and number of central molecules
	for (n=0; n<nBins_; n++) data_[n] /= double(nAdded_) * sites_[0]->pattern()->nMols() ;
	// Normalise nAdded_ording to number density of sites_ in RDF shells
	numDensity = sites_[1]->pattern()->nMols() / sourcemodel->cell()->volume();
	for (n=0; n<nBins_; n++)
	{
		r1 = lower_ + double(n) * binWidth_;
		r2 = r1 + binWidth_;
		factor = (4.0 / 3.0) * PI * (r2*r2*r2 - r1*r1*r1) * numDensity;
		data_[n] /= factor;

	}
	msg.exit("Rdf::finalise");
}

// Save RDF data_
bool Rdf::save()
{
	int n;
	for (n=0; n<nBins_; n++) printf(" %f  %f\n",binWidth_ * (n + 0.5), data_[n]);
	return TRUE;
}
