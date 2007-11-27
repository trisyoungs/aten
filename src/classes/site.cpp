/*
	*** Molecule site
	*** src/classes/site.h
	Copyright T. Youngs 2007

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

#include "classes/site.h"
#include "classes/pattern.h"
#include "base/sysfunc.h"

// Site types
const char *ST_strings[ST_NITEMS] = { "Molecule COM", "Molecule COG", "Atom(s) COM", "Atom(s) COG" };
const char *ST_keywords[ST_NITEMS] = { "molcom", "molcog", "atomcom", "atomcog" };
const char *text_from_ST(site_type i)
	{ return ST_strings[i]; }
site_type ST_from_text(const char *s)
	{ return (site_type) enum_search("site type",ST_NITEMS,ST_keywords,s); }

// Constructor
site::site()
{
	centretype = ST_MOLCOM;
	centre.zero();
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_SITE] ++;
	#endif
}

// Destructor
site::~site()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_SITE] ++;
	#endif
}

// Calculate site centre
vec3<double> site::calculate_centre(model *srcmodel, int mol)
{
	dbg_begin(DM_CALLS,"site::calculate_centre");
	int offset, n;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = srcmodel->get_cell();
	static vec3<double> firstid, mim;
	listitem<int> *li;
	offset = sourcepattern->get_startatom();
	offset += sourcepattern->get_natoms() * mol;
	// If no atoms are in the list, use all atoms in the molecule
	if (atoms.size() != 0)
	{
		li = atoms.first();
		centre = modelatoms[offset + li->data]->r;
		firstid = centre;
		for (li = li->next; li != NULL; li = li->next)
		{
			mim = cell->mim(modelatoms[offset + li->data]->r, firstid);
			centre += mim;
		}
		// Take average
		centre /= atoms.size();
	}
	else
	{
		// Use all atoms for centre. Grab first as the MIM point
		centre = modelatoms[offset]->r;
		firstid = centre;
		for (n=1; n<sourcepattern->get_natoms(); n++)
		{
			mim = cell->mim(modelatoms[offset + n]->r, firstid);
			centre += mim;
		}
		// Take average
		centre /= sourcepattern->get_natoms();
	}
	dbg_end(DM_CALLS,"site::calculate_centre");
	return centre;
}

// Calculate site local axis system
mat3<double> site::calculate_axes(model *srcmodel, int mol)
{
	dbg_begin(DM_CALLS,"site::calculate_axes");
	int offset, n;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = srcmodel->get_cell();
	static vec3<double> mim, v1, v2;
	listitem<int> *li;
	offset = sourcepattern->get_startatom();
	offset += sourcepattern->get_natoms() * mol;
	// Calculate 'position' of x-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v1.zero();
	for (li = xaxisatoms.first(); li != NULL; li = li->next)
	{
		mim = cell->mim(modelatoms[offset + li->data]->r, centre);
		v1 += mim;
	}
	// Take average and subtract site centre to get vector
	v1 /= xaxisatoms.size();
	v1 -= centre;
	// Calculate 'position' of y-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v2.zero();
	for (li = yaxisatoms.first(); li != NULL; li = li->next)
	{
		mim = cell->mim(modelatoms[offset + li->data]->r, centre);
		v2 += mim;
	}
	// Take average and subtract site centre to get vector
	v2 /= yaxisatoms.size();
	v2 -= centre;
	// Orthogonalise, normalise, and generate corresponding z-axis
	v2.orthogonalise(v1);
	v1.normalise();
	v2.normalise();
	axes.set(0,v1);
	axes.set(1,v2);
	axes.set(2,v1 * v2);
	//axes.print();
	dbg_begin(DM_CALLS,"site::calculate_axes");
	return axes;
}
