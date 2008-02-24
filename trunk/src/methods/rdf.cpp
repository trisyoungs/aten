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

// Constructor
rdf::rdf()
{
	sites[0] = NULL;
	sites[1] = NULL;
	lower = 0.0;
	upper = 15.0;
	binwidth = 0.1;
	nbins = 150;
	range = 15.0;
	acc = 0;
	data = NULL;
}

// Destructor
rdf::~rdf()
{
	if (sites[0] != NULL) delete sites[0];
	if (sites[1] != NULL) delete sites[1];
	if (data != NULL) delete[] data;
}

// Set site
void rdf::set_site(int i, site *s)
{
	if (i < 2) sites[i] = s;
	else printf("OUTOFRANGE:rdf::set_site\n");
}

// Get site
site *rdf::get_site(int i)
{
	if (i < 2) return sites[i];
	else printf("OUTOFRANGE:rdf::set_site\n");
	return NULL;
}

// Set RDF range
void rdf::set_range(double d, double w, int n)
{
	lower = d;
	binwidth = w;
	nbins = n;
	upper = d + w*n;
	range = upper - lower;
}

// Initialise structure
bool rdf::initialise()
{
	dbg_begin(DM_CALLS,"rdf::initialise");
	// Check site definitions....
	if ((sites[0] == NULL) || (sites[1] == NULL))
	{
		msg(DM_NONE,"rdf::initialise - At least one site has NULL value.\n");
		dbg_end(DM_CALLS,"rdf::initialise");
		return FALSE;
	}
	// Create the data arrays
	data = new double[nbins];
	for (int n=0; n<nbins; n++) data[n] = 0.0;
	msg(DM_NONE,"There are %i bins in rdf '%s', beginning at r = %f.\n", nbins, name.get(), lower);
	acc = 0;
	dbg_end(DM_CALLS,"rdf::initialise");
	return TRUE;
}

// Accumulate quantity data from supplied model
void rdf::accumulate(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"rdf::accumulate");
	int m1, m2, bin;
	static vec3<double> centre1, centre2, mimd;
	unitcell *cell = sourcemodel->get_cell();
	double dist;
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
			mimd = cell->mimd(centre2,centre1);
			// Add distance to data array
			bin = int(mimd.magnitude() / binwidth);
	//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
			if (bin < nbins) data[bin] += 1.0;
		}
	}
	// Increase accumulation counter
	acc ++;
	dbg_end(DM_CALLS,"rdf::accumulate");
}

// Finalise
void rdf::finalise(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"rdf::finalise");
	int n;
	double factor, r1, r2, numdensity;
	// Normalise the rdf w.r.t. number of frames and number of central molecules
	for (n=0; n<nbins; n++) data[n] /= double(acc) * sites[0]->get_pattern()->get_nmols() ;
	// Normalise according to number density of sites in RDF shells
	numdensity = sites[1]->get_pattern()->get_nmols() / sourcemodel->get_volume();
	for (n=0; n<nbins; n++)
	{
		r1 = lower + double(n) * binwidth;
		r2 = r1 + binwidth;
		factor = (4.0 / 3.0) * PI * (r2*r2*r2 - r1*r1*r1) * numdensity;
		data[n] /= factor;

	}
	dbg_end(DM_CALLS,"rdf::finalise");
}

// Save RDF data
bool rdf::save()
{
	int n;
	for (n=0; n<nbins; n++) printf(" %f  %f\n",binwidth * (n + 0.5), data[n]);
	return TRUE;
}
