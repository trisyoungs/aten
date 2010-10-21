/*
	*** Model site functions
	*** src/model/site.cpp
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

#include "model/model.h"
#include "classes/site.h"
#include "base/pattern.h"

// Find site by name
Site *Model::findSite(const char *s)
{
	msg.enter("Model::findSite");
	Site *result = NULL;
	for (result = sites.first(); result != NULL; result = result->next)
		if (strcmp(result->name(),s) == 0) break;
	msg.exit("Model::findSite");
	return result;
}

// Calculate site centre
Vec3<double> Model::siteCentre(Site *s, int mol)
{
	msg.enter("Model::calculateCentre");
	int offset, n;
	Atom **modelatoms = atomArray();
	static Vec3<double> firstid, mim, centre;
	ListItem<int> *li;
	Pattern *sitep = s->pattern();
	offset = sitep->startAtom();
	offset += sitep->nAtoms() * mol;
	// If no atoms are in the list, use all atoms in the molecule
	if (s->atoms.nItems() != 0)
	{
		li = s->atoms.first();
		centre = modelatoms[offset + li->value()]->r();
		firstid = centre;
		for (li = li->next; li != NULL; li = li->next)
		{
			mim = cell_.mim(modelatoms[offset + li->value()]->r(), firstid);
			centre += mim;
		}
		// Take average
		centre /= s->atoms.nItems();
	}
	else
	{
		// Use all atoms for centre. Grab first as the MIM point
		centre = modelatoms[offset]->r();
		firstid = centre;
		for (n=1; n<sitep->nAtoms(); n++)
		{
			mim = cell_.mim(modelatoms[offset + n]->r(), firstid);
			centre += mim;
		}
		// Take average
		centre /= sitep->nAtoms();
	}
	s->setCentre(centre);
	msg.exit("Model::calculateCentre");
	return centre;
}

// Calculate site local axis system
Mat3<double> Model::siteAxes(Site *s, int mol)
{
	msg.enter("Model::calculateAxes");
	int offset;
	Atom **modelatoms = atomArray();
	static Vec3<double> mim, v1, v2, centre;
	Mat3<double> axes;
	ListItem<int> *li;
	Pattern *sitep = s->pattern();
	offset = sitep->startAtom();
	offset += sitep->nAtoms() * mol;
	// Calculate 'position' of x-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v1.zero();
	for (li = s->xAxisAtoms.first(); li != NULL; li = li->next)
	{
		mim = cell_.mim(modelatoms[offset + li->value()]->r(), centre);
		v1 += mim;
	}
	// Take average and subtract site centre to get vector
	v1 /= s->xAxisAtoms.nItems();
	v1 -= centre;
	// Calculate 'position' of y-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v2.zero();
	for (li = s->yAxisAtoms.first(); li != NULL; li = li->next)
	{
		mim = cell_.mim(modelatoms[offset + li->value()]->r(), centre);
		v2 += mim;
	}
	// Take average and subtract site centre to get vector
	v2 /= s->yAxisAtoms.nItems();
	v2 -= centre;
	// Orthogonalise, normalise, and generate corresponding z-axis
	v2.orthogonalise(v1);
	v1.normalise();
	v2.normalise();
	axes.set(0,v1);
	axes.set(1,v2);
	axes.set(2,v1 * v2);
	//axes.print();
	s->setAxes(axes);
	msg.enter("Model::calculateAxes");
	return axes;
}
