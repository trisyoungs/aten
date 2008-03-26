/*
	*** Geometryy measurement calculation
	*** src/methods/geometry.cpp
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

#include "methods/geometry.h"
#include "classes/site.h"
#include "classes/pattern.h"
#include "model/model.h"

// Constructor
Geometry::Geometry()
{
	sites_[0] = NULL;
	sites_[1] = NULL;
	sites_[2] = NULL;
	sites_[3] = NULL;
	lower_ = 0.0;
	upper_ = 15.0;
	binWidth_ = 0.1;
	nBins_ = 150;
	range_ = 15.0;
	nAdded_ = 0;
	data_ = NULL;
}

// Destructor
Geometry::~Geometry()
{
	if (sites_[0] != NULL) delete sites_[0];
	if (sites_[1] != NULL) delete sites_[1];
	if (sites_[2] != NULL) delete sites_[2];
	if (sites_[3] != NULL) delete sites_[3];
	if (data_ != NULL) delete[] data_;
}

// Set site
void Geometry::setSite(int i, Site *s)
{
	if (i < 4) sites_[i] = s;
	else printf("OUTOFRANGE:Geometry::setSite\n");
}

// Get site
Site *Geometry::site(int i)
{
	if (i < 4) return sites_[i];
	else printf("OUTOFRANGE:Geometry::site\n");
	return NULL;
}

// Set histogram range_
void Geometry::setRange(double d, double w, int n)
{
	lower_ = d;
	binWidth_ = w;
	nBins_ = n;
	upper_ = d + w*n;
	range_ = upper_ - lower_;
}

// Initialise structure
bool Geometry::initialise()
{
	dbgBegin(Debug::Calls,"Geometry::initialise");
	// Check site definitions....
	for (nSites_ = 0; nSites_ < 4; nSites_++) if (sites_[nSites_] == NULL) break;
	if (nSites_ == 0)
	{
		msg(Debug::None,"Geometry::initialise - At least two sites_ must be defined.\n");
		dbgEnd(Debug::Calls,"Geometry::initialise");
		return FALSE;
	}
	// Create the data arrays
	data_ = new double[nBins_];
	for (int n=0; n<nBins_; n++) data_[n] = 0.0;
	msg(Debug::None,"There are %i bins in geometry '%s', beginning at r = %f.\n", nBins_, name_.get(), lower_);
	nAdded_ = 0;
	dbgEnd(Debug::Calls,"Geometry::initialise");
	return TRUE;
}

// Accumulate quantity data from supplied model
void Geometry::accumulate(Model *sourcemodel)
{
	dbgBegin(Debug::Calls,"Geometry::accumulate");
	int m1, m2, m3, m4, bin;
	static Vec3<double> centre1, centre2, centre3, centre4;
	Cell *cell = sourcemodel->cell();
	double geom;
	if (nSites_ == 2)
	{
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
				geom = cell->distance(centre1,centre2);
				// Add distance to data array
				bin = int(geom / binWidth_);
				//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
				if (bin < nBins_) data_[bin] += 1.0;
			}
		}
	}
	else if (nSites_ == 3)
	{
		// Loop over molecules for site1
		for (m1=0; m1 < sites_[0]->pattern()->nMols(); m1++)
		{
			// Get first centre
			centre1 = sites_[0]->calculateCentre(sourcemodel,m1);
			// Loop over molecules for site2
			for (m2 = 0; m2 < sites_[1]->pattern()->nMols(); m2++)
			{
				centre2 = sites_[1]->calculateCentre(sourcemodel,m2);
				// Loop over molecules for site3
				for (m3 = 0; m3 < sites_[2]->pattern()->nMols(); m3++)
				{
					centre3 = sites_[2]->calculateCentre(sourcemodel,m3);


					// Calculate minimum image distance and bin
					geom = cell->angle(centre1, centre2, centre3);
					// Add distance to data array
					bin = int(geom / binWidth_);
					//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
					if (bin < nBins_) data_[bin] += 1.0;
				}
			}
		}
	}
	else if (nSites_ == 4)
	{
		// Loop over molecules for site1
		for (m1=0; m1 < sites_[0]->pattern()->nMols(); m1++)
		{
			// Get first centre
			centre1 = sites_[0]->calculateCentre(sourcemodel,m1);
			// Loop over molecules for site2
			for (m2 = 0; m2 < sites_[1]->pattern()->nMols(); m2++)
			{
				centre2 = sites_[1]->calculateCentre(sourcemodel,m2);
				// Loop over molecules for site3
				for (m3 = 0; m3 < sites_[2]->pattern()->nMols(); m3++)
				{
					centre3 = sites_[2]->calculateCentre(sourcemodel,m3);


					// Calculate minimum image distance and bin
					geom = cell->angle(centre1, centre2, centre3);
					// Add distance to data array
					bin = int(geom / binWidth_);
					//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
					if (bin < nBins_) data_[bin] += 1.0;
				}
			}
		}
	}

	// Increase accumulation counter
	nAdded_ ++;
	dbgEnd(Debug::Calls,"Geometry::accumulate");
}

// Finalise
void Geometry::finalise(Model *sourcemodel)
{
	dbgBegin(Debug::Calls,"Geometry::finalise");
	dbgEnd(Debug::Calls,"Geometry::finalise");
}

// Save measurement data
bool Geometry::save()
{
	int n;
	for (n=0; n<nBins_; n++) printf(" %f  %f\n",binWidth_ * (n + 0.5), data_[n]);
	return TRUE;
}
