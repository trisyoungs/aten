/*
	*** Molecule site
	*** src/classes/site.h
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

#include "classes/site.h"
#include "classes/pattern.h"
#include "base/sysfunc.h"
#include "model/model.h"

// Site types
const char *ST_strings[ST_NITEMS] = { "Molecule COM", "Molecule COG", "Atom(s) COM", "Atom(s) COG" };
const char *ST_keywords[ST_NITEMS] = { "molcom", "molcog", "atomcom", "atomcog" };
const char *text_from_ST(SiteType i)
	{ return ST_strings[i]; }
SiteType ST_from_text(const char *s)
	{ return (SiteType) enumSearch("site type",ST_NITEMS,ST_keywords,s); }

// Constructor
Site::Site()
{
	// Private variables
	centre_ = ST_MOLCOM;
	pattern_ = NULL;
	centre_.zero();
	// Public variables
	next = NULL;
	prev = NULL;
}

// Set the pattern pointer for the atom
void Site::setPattern(Pattern *p)
{
	pattern_ = p;
}

// Returns the current pattern for the atom
Pattern *Site::pattern()
{
	return pattern_;
}

// Set name of site
void Site::setName(const char *s)
{
	name_ = s;
}

// Get name of site
const char *Site::name()
{
	return name_.get();
}

// Calculate site centre
Vec3<double> Site::calculateCentre(Model *srcmodel, int mol)
{
	dbgBegin(Debug::Calls,"Site::calculateCentre");
	int offset, n;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	static Vec3<double> firstid, mim;
	Listitem<int> *li;
	offset = pattern_->startAtom();
	offset += pattern_->nAtoms() * mol;
	// If no atoms are in the list, use all atoms in the molecule
	if (atoms.nItems() != 0)
	{
		li = atoms.first();
		centre_ = modelatoms[offset + li->data]->r();
		firstid = centre_;
		for (li = li->next; li != NULL; li = li->next)
		{
			mim = cell->mim(modelatoms[offset + li->data]->r(), firstid);
			centre_ += mim;
		}
		// Take average
		centre_ /= atoms.nItems();
	}
	else
	{
		// Use all atoms for centre. Grab first as the MIM point
		centre_ = modelatoms[offset]->r();
		firstid = centre_;
		for (n=1; n<pattern_->nAtoms(); n++)
		{
			mim = cell->mim(modelatoms[offset + n]->r(), firstid);
			centre_ += mim;
		}
		// Take average
		centre_ /= pattern_->nAtoms();
	}
	dbgEnd(Debug::Calls,"Site::calculateCentre");
	return centre_;
}

// Calculate site local axis system
Mat3<double> Site::calculateAxes(Model *srcmodel, int mol)
{
	dbgBegin(Debug::Calls,"Site::calculateAxes");
	int offset, n;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	static Vec3<double> mim, v1, v2;
	Listitem<int> *li;
	offset = pattern_->startAtom();
	offset += pattern_->nAtoms() * mol;
	// Calculate 'position' of x-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v1.zero();
	for (li = xAxisAtoms.first(); li != NULL; li = li->next)
	{
		mim = cell->mim(modelatoms[offset + li->data]->r(), centre_);
		v1 += mim;
	}
	// Take average and subtract site centre to get vector
	v1 /= xAxisAtoms.nItems();
	v1 -= centre_;
	// Calculate 'position' of y-axis (defining vector COG->xpos)
	// Get mim coordinates relative to (already-calculated) site centre
	v2.zero();
	for (li = yAxisAtoms.first(); li != NULL; li = li->next)
	{
		mim = cell->mim(modelatoms[offset + li->data]->r(), centre_);
		v2 += mim;
	}
	// Take average and subtract site centre to get vector
	v2 /= yAxisAtoms.nItems();
	v2 -= centre_;
	// Orthogonalise, normalise, and generate corresponding z-axis
	v2.orthogonalise(v1);
	v1.normalise();
	v2.normalise();
	axes_.set(0,v1);
	axes_.set(1,v2);
	axes_.set(2,v1 * v2);
	//axes.print();
	dbgBegin(Debug::Calls,"Site::calculateAxes");
	return axes_;
}
