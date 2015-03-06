/*
	*** Model site functions
	*** src/model/site.cpp
	Copyright T. Youngs 2007-2015

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

#include "model/model.h"
#include "base/site.h"
#include "base/pattern.h"

ATEN_USING_NAMESPACE

// Find site by name
Site* Model::findSite(const char* s)
{
	Messenger::enter("Model::findSite");
	Site* result = NULL;
	for (result = sites.first(); result != NULL; result = result->next) if (strcmp(result->name(),s) == 0) break;
	Messenger::exit("Model::findSite");
	return result;
}

// Calculate site centre
Vec3<double> Model::siteCentre(Site* s, int mol)
{
	Messenger::enter("Model::calculateCentre");
	int offset, n, ii;
	Atom** modelatoms = atomArray();
	static Vec3<double> firstid, mim, centre;
	Pattern* sitep = s->pattern();
	offset = sitep->startAtom();
	offset += sitep->nAtoms() * mol;
	// If no atoms are in the list, use all atoms in the molecule
	if (s->atoms.count() != 0)
	{
		ii = s->atoms.first();
		centre = modelatoms[offset + ii]->r();
		firstid = centre;
		for (n=1; n<s->atoms.count(); ++n)
		{
			centre += cell_.mim(modelatoms[offset + n]->r(), firstid);
		}
		// Take average
		centre /= s->atoms.count();
	}
	else
	{
		// Use all atoms for centre. Grab first as the MIM point
		centre = modelatoms[offset]->r();
		firstid = centre;
		for (n=1; n<sitep->nAtoms(); ++n)
		{
			centre += cell_.mim(modelatoms[offset + n]->r(), firstid);
		}
		// Take average
		centre /= sitep->nAtoms();
	}
	s->setCentre(centre);
	Messenger::exit("Model::calculateCentre");
	return centre;
}

// Calculate site local axis system
Matrix Model::siteAxes(Site* s, int mol)
{
	Messenger::enter("Model::calculateAxes");
	int offset, n;
	Atom** modelatoms = atomArray();
	static Vec3<double> mim, v1, v2, centre;
	Matrix axes;
	ListItem<int>* li;
	Pattern* sitep = s->pattern();
	offset = sitep->startAtom();
	offset += sitep->nAtoms() * mol;
	// Calculate 'position' of x-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v1.zero();
	for (n=0; n<s->xAxisAtoms.count(); ++n)
	{
		mim = cell_.mim(modelatoms[offset + s->xAxisAtoms.at(n)]->r(), centre);
		v1 += mim;
	}
	// Take average and subtract site centre to get vector
	v1 /= s->xAxisAtoms.count();
	v1 -= centre;
	// Calculate 'position' of y-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v2.zero();
	for (n=0; n<s->yAxisAtoms.count(); ++n)
	{
		mim = cell_.mim(modelatoms[offset + s->yAxisAtoms.at(n)]->r(), centre);
		v2 += mim;
	}
	// Take average and subtract site centre to get vector
	v2 /= s->yAxisAtoms.count();
	v2 -= centre;
	// Orthogonalise, normalise, and generate corresponding z-axis
	v2.orthogonalise(v1);
	v1.normalise();
	v2.normalise();
	axes.setColumn(0,v1,0.0);
	axes.setColumn(1,v2,0.0);
	axes.setColumn(2,v1 * v2,0.0);
	//axes.print();
	s->setAxes(axes);
	Messenger::enter("Model::calculateAxes");
	return axes;
}
