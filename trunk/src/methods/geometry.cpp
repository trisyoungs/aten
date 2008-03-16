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

// Constructor
geometry::geometry()
{
	sites[0] = NULL;
	sites[1] = NULL;
	sites[2] = NULL;
	sites[3] = NULL;
	lower = 0.0;
	upper = 15.0;
	binwidth = 0.1;
	nbins = 150;
	range = 15.0;
	acc = 0;
	data = NULL;
}

// Destructor
geometry::~geometry()
{
}

// Set site
void geometry::set_site(int i, site *s)
{
	if (i < 4) sites[i] = s;
	else printf("OUTOFRANGE:geometry::set_site\n");
}

// Get site
site *geometry::get_site(int i)
{
	if (i < 4) return sites[i];
	else printf("OUTOFRANGE:geometry::get_site\n");
	return NULL;
}

// Set histogram range
void geometry::set_range(double d, double w, int n)
{
	lower = d;
	binwidth = w;
	nbins = n;
	upper = d + w*n;
	range = upper - lower;
}

// Initialise structure
bool geometry::initialise()
{
	dbg_begin(DM_CALLS,"geometry::initialise");
	// Check site definitions....
	for (nsites = 0; nsites < 4; nsites++) if (sites[nsites] == NULL) break;
	if (nsites == 0)
	{
		msg(DM_NONE,"geometry::initialise - At least two sites must be defined.\n");
		dbg_end(DM_CALLS,"geometry::initialise");
		return FALSE;
	}
	// Create the data arrays
	data = new double[nbins];
	for (int n=0; n<nbins; n++) data[n] = 0.0;
	msg(DM_NONE,"There are %i bins in geometry '%s', beginning at r = %f.\n", nbins, name.get(), lower);
	acc = 0;
	dbg_end(DM_CALLS,"geometry::initialise");
	return TRUE;
}

// Accumulate quantity data from supplied model
void geometry::accumulate(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"geometry::accumulate");
	int m1, m2, m3, m4, bin;
	static vec3<double> centre1, centre2, centre3, centre4;
	unitcell *cell = sourcemodel->get_cell();
	double geom;
	if (nsites == 2)
	{
		// Loop over molecules for site1
		for (m1=0; m1 < sites[0]->get_pattern()->get_nmols(); m1++)
		{
			// Get first centre
			centre1 = sites[0]->calculate_centre(sourcemodel,m1);
			// Loop over molecules for site2
			for (m2 = 0; m2 < sites[1]->get_pattern()->get_nmols(); m2++)
			{
				centre2 = sites[1]->calculate_centre(sourcemodel,m2);
				// Calculate minimum image distance and bin
				geom = cell->distance(centre1,centre2);
				// Add distance to data array
				bin = int(geom / binwidth);
				//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
				if (bin < nbins) data[bin] += 1.0;
			}
		}
	}
	else if (nsites == 3)
	{
		// Loop over molecules for site1
		for (m1=0; m1 < sites[0]->get_pattern()->get_nmols(); m1++)
		{
			// Get first centre
			centre1 = sites[0]->calculate_centre(sourcemodel,m1);
			// Loop over molecules for site2
			for (m2 = 0; m2 < sites[1]->get_pattern()->get_nmols(); m2++)
			{
				centre2 = sites[1]->calculate_centre(sourcemodel,m2);
				// Loop over molecules for site3
				for (m3 = 0; m3 < sites[2]->get_pattern()->get_nmols(); m3++)
				{
					centre3 = sites[2]->calculate_centre(sourcemodel,m3);


					// Calculate minimum image distance and bin
					geom = cell->angle(centre1, centre2, centre3);
					// Add distance to data array
					bin = int(geom / binwidth);
					//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
					if (bin < nbins) data[bin] += 1.0;
				}
			}
		}
	}
	else if (nsites == 4)
	{
		// Loop over molecules for site1
		for (m1=0; m1 < sites[0]->get_pattern()->get_nmols(); m1++)
		{
			// Get first centre
			centre1 = sites[0]->calculate_centre(sourcemodel,m1);
			// Loop over molecules for site2
			for (m2 = 0; m2 < sites[1]->get_pattern()->get_nmols(); m2++)
			{
				centre2 = sites[1]->calculate_centre(sourcemodel,m2);
				// Loop over molecules for site3
				for (m3 = 0; m3 < sites[2]->get_pattern()->get_nmols(); m3++)
				{
					centre3 = sites[2]->calculate_centre(sourcemodel,m3);


					// Calculate minimum image distance and bin
					geom = cell->angle(centre1, centre2, centre3);
					// Add distance to data array
					bin = int(geom / binwidth);
					//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
					if (bin < nbins) data[bin] += 1.0;
				}
			}
		}
	}

	// Increase accumulation counter
	acc ++;
	dbg_end(DM_CALLS,"geometry::accumulate");
}

// Finalise
void geometry::finalise(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"geometry::finalise");
	dbg_end(DM_CALLS,"geometry::finalise");
}

// Save measurement data
bool geometry::save()
{
	int n;
	for (n=0; n<nbins; n++) printf(" %f  %f\n",binwidth * (n + 0.5), data[n]);
	return TRUE;
}
