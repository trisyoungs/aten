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
#include "model/model.h"
#include <fstream>

// Constructor
Pdens::Pdens()
{
	sites_[0] = NULL;
	sites_[1] = NULL;
	stepSize_ = 0.5;
	nSteps_ = 30;
	nAdded_ = 0;
	data_ = NULL;
}

// Destructor
Pdens::~Pdens()
{
	if (sites_[0] != NULL) delete sites_[0];
	if (sites_[1] != NULL) delete sites_[1];
	if (data_ != NULL) delete[] data_;
}

// Get stepsize
double Pdens::stepSize()
{
	return stepSize_;
}

// Get number of bins
int Pdens::nSteps()
{
	return nSteps_;
}

// Set site
void Pdens::setSite(int i, Site *s)
{
	if (i < 2) sites_[i] = s;
	else printf("OUTOFRANGE:pdenssetsite\n");
}

// Get site
Site *Pdens::site(int i)
{
	if (i < 2) return sites_[i];
	else printf("OUTOFRANGE:pdensgetsite\n");
	return NULL;
}

// Set distribution range
void Pdens::setRange(double ss, int n)
{
	stepSize_ = ss;
	nSteps_ = n;
	totalSteps_ = n + n + 1;
}

// Initialise structure
bool Pdens::initialise()
{
	dbgBegin(Debug::Calls,"Pdens::initialise");
	// Check site definitions....
	if ((sites_[0] == NULL) || (sites_[1] == NULL))
	{
		msg(Debug::None,"Pdens::initialise - At least one site has NULL value.\n");
		dbgEnd(Debug::Calls,"calculable::initialise");
		return FALSE;
	}
	// Create the data_ array
	int n, m, o;
	data_ = new double**[totalSteps_];
	for (n=0; n<totalSteps_; n++)
	{
		data_[n] = new double*[totalSteps_];
		for (m=0; m<totalSteps_; m++)
		{
			data_[n][m] = new double[totalSteps_];
			for (o=0; o<totalSteps_; o++) data_[n][m][o] = 0.0;
		}
	}
	msg(Debug::None,"There are %i gridpoints of %f Angstrom along each cartesian axis in pdens '%s'.\n", totalSteps_, stepSize_, name_.get());
	nAdded_ = 0;
	dbgEnd(Debug::Calls,"Pdens::initialise");
	return TRUE;
}

// Accumulate quantity data_ from supplied model
void Pdens::accumulate(Model *sourcemodel)
{
	dbgBegin(Debug::Calls,"Pdens::accumulate");
	int m1, m2, bin;
	static Vec3<double> centre1, centre2, mimd;
	static Vec3<int> gridPoint;
	static Mat3<double> axes;
	Cell *cell = sourcemodel->cell();
	double n, m, o;
	// Loop over molecules for site1
	for (m1=0; m1 < sites_[0]->pattern()->nMols(); m1++)
	{
		// Get central position and local coordinate system
		centre1 = sites_[0]->calculateCentre(sourcemodel,m1);
		axes = sites_[0]->calculateAxes(sourcemodel,m1);
		// Loop over molecules for site2
		for (m2 = 0; m2 < sites_[1]->pattern()->nMols(); m2++)
		{
			centre2 = sites_[1]->calculateCentre(sourcemodel,m2);
			// Calculate minimum image vector...
			mimd = cell->mimd(centre2,centre1);
			// ...translate into local coordinate system...
			mimd *= axes;
			// ...and work out the gridpoint (convert to 0..totalSteps_ from -nsteps..0..+nsteps)
			gridPoint.x = int(mimd.x / stepSize_);
			gridPoint.y = int(mimd.y / stepSize_);
			gridPoint.z = int(mimd.z / stepSize_);
			gridPoint += nSteps_;
	//printf("Adding distance %f to bin %i\n",mimd.magnitude(),bin);
			addPoint(gridPoint);
		}
	}
	// Increase accumulation counter
	nAdded_ ++;
	dbgEnd(Debug::Calls,"Pdens::accumulate");
}

// Add point to data_ array
void Pdens::addPoint(Vec3<int> &coords)
{
	// Check coordinates of gridpoint given
	if ((coords.x < 0) || (coords.x >= totalSteps_)) return;
	if ((coords.y < 0) || (coords.y >= totalSteps_)) return;
	if ((coords.z < 0) || (coords.z >= totalSteps_)) return;
	if (data_ == NULL) printf("Pdens::add_point <<<< Data array not initialised! >>>>\n");
	else data_[coords.x][coords.y][coords.z] += 1.0;
}

// Finalise
void Pdens::finalise(Model *sourcemodel)
{
	dbgBegin(Debug::Calls,"Pdens::finalise");
	int n, m, o;
	double factor, numberDensity;
	// Normalise the pdens w.r.t. number of frames, number of central molecules, and number density of system
	numberDensity = sites_[1]->pattern()->nMols() / sourcemodel->cell()->volume() * (stepSize_ * stepSize_ * stepSize_);
	factor = double(nAdded_) * sites_[0]->pattern()->nMols() * numberDensity;
	for (n=0; n<totalSteps_; n++)
		for (m=0; m<totalSteps_; m++)
			for (o=0; o<totalSteps_; o++) data_[n][m][o] /= factor;
	dbgEnd(Debug::Calls,"Pdens::finalise");
}

// Save RDF data_
bool Pdens::save()
{
	int n, m, o;
	ofstream output(filename_.get(), ios::out);
	for (n=0; n<totalSteps_; n++)
		for (m=0; m<totalSteps_; m++)
			for (o=0; o<totalSteps_; o++) output << data_[n][m][o] << "\n";
	output.close();
	return TRUE;
}
