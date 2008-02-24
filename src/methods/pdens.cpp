/*
	*** Probability density calculation
	*** src/methods/pdens.cpp
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

#include "methods/pdens.h"
#include "classes/site.h"
#include "classes/pattern.h"
#include <fstream>

// Constructor
pdens::pdens()
{
	sites[0] = NULL;
	sites[1] = NULL;
	stepsize = 0.5;
	nsteps = 30;
	acc = 0;
	data = NULL;
}

// Destructor
pdens::~pdens()
{
	if (sites[0] != NULL) delete sites[0];
	if (sites[1] != NULL) delete sites[1];
	if (data != NULL) delete[] data;
}

// Set site
void pdens::set_site(int i, site *s)
{
	if (i < 2) sites[i] = s;
	else printf("OUTOFRANGE:pdenssetsite\n");
}

// Get site
site *pdens::get_site(int i)
{
	if (i < 2) return sites[i];
	else printf("OUTOFRANGE:pdensgetsite\n");
	return NULL;
}

// Set distribution range
void pdens::set_range(double ss, int n)
{
	stepsize = ss;
	nsteps = n;
	totalsteps = n + n + 1;
}

// Initialise structure
bool pdens::initialise()
{
	dbg_begin(DM_CALLS,"pdens::initialise");
	// Check site definitions....
	if ((sites[0] == NULL) || (sites[1] == NULL))
	{
		msg(DM_NONE,"pdens::initialise - At least one site has NULL value.\n");
		dbg_end(DM_CALLS,"calculable::initialise");
		return FALSE;
	}
	// Create the data array
	int n, m, o;
	data = new double**[totalsteps];
	for (n=0; n<totalsteps; n++)
	{
		data[n] = new double*[totalsteps];
		for (m=0; m<totalsteps; m++)
		{
			data[n][m] = new double[totalsteps];
			for (o=0; o<totalsteps; o++) data[n][m][o] = 0.0;
		}
	}
	msg(DM_NONE,"There are %i gridpoints of %f Angstrom along each cartesian axis in pdens '%s'.\n", totalsteps, stepsize, name.get());
	acc = 0;
	dbg_end(DM_CALLS,"pdens::initialise");
	return TRUE;
}

// Accumulate quantity data from supplied model
void pdens::accumulate(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"pdens::accumulate");
	int m1, m2, bin;
	static vec3<double> centre1, centre2, mimd;
	static vec3<int> gridpoint;
	static mat3<double> axes;
	unitcell *cell = sourcemodel->get_cell();
	double n, m, o;
	// Loop over molecules for site1
	for (m1=0; m1 < sites[0]->get_pattern()->get_nmols(); m1++)
	{
		// Get central position and local coordinate system
		centre1 = sites[0]->calculate_centre(sourcemodel,m1);
		axes = sites[0]->calculate_axes(sourcemodel,m1);
		// Loop over molecules for site2
		for (m2 = 0; m2 < sites[1]->get_pattern()->get_nmols(); m2++)
		{
			centre2 = sites[1]->calculate_centre(sourcemodel,m2);
			// Calculate minimum image vector...
			mimd = cell->mimd(centre2,centre1);
			// ...translate into local coordinate system...
			mimd *= axes;
			// ...and work out the gridpoint (convert to 0..totalsteps from -nsteps..0..+nsteps)
			gridpoint.x = int(mimd.x / stepsize);
			gridpoint.y = int(mimd.y / stepsize);
			gridpoint.z = int(mimd.z / stepsize);
			gridpoint += nsteps;
	//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
			add_point(gridpoint);
		}
	}
	// Increase accumulation counter
	acc ++;
	dbg_end(DM_CALLS,"pdens::accumulate");
}

// Add point to data array
void pdens::add_point(vec3<int> &coords)
{
	// Check coordinates of gridpoint given
	if ((coords.x < 0) || (coords.x >= totalsteps)) return;
	if ((coords.y < 0) || (coords.y >= totalsteps)) return;
	if ((coords.z < 0) || (coords.z >= totalsteps)) return;
	if (data == NULL) printf("pdens::add_point <<<< Data array not initialised! >>>>\n");
	else data[coords.x][coords.y][coords.z] += 1.0;
}

// Finalise
void pdens::finalise(model *sourcemodel)
{
	dbg_begin(DM_CALLS,"pdens::finalise");
	int n, m, o;
	double factor, numdensity;
	// Normalise the pdens w.r.t. number of frames, number of central molecules, and number density of system
	numdensity = sites[1]->get_pattern()->get_nmols() / sourcemodel->get_volume() * (stepsize * stepsize * stepsize);
	factor = double(acc) * sites[0]->get_pattern()->get_nmols() * numdensity;
	for (n=0; n<totalsteps; n++)
		for (m=0; m<totalsteps; m++)
			for (o=0; o<totalsteps; o++) data[n][m][o] /= factor;
	dbg_end(DM_CALLS,"pdens::finalise");
}

// Save RDF data
bool pdens::save()
{
	int n, m, o;
	ofstream output(filename.get(), ios::out);
	for (n=0; n<totalsteps; n++)
		for (m=0; m<totalsteps; m++)
			for (o=0; o<totalsteps; o++) output << data[n][m][o] << "\n";
	output.close();
	return TRUE;
}
