/*
	*** Model glyph functions
	*** src/model/glyph.cpp
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

#include "classes/glyph.h"
#include "model/model.h"

// Create new glyph in this model
Glyph *Model::addGlyph(Glyph::GlyphType gt)
{
	logChange(Change::VisualLog);
	Glyph *newglyph = glyphs_.add();
	newglyph->setParent(this);
	newglyph->setType(gt);
	return newglyph;
}

// Return list of glyphs
Glyph *Model::glyphs()
{
	return glyphs_.first();
}

// Automatically add polyhedra to current atom selection 
void Model::addPolyhedraGlyphs(bool centresonly, bool linkatoms, double rcut)
{
	msg.enter("Model::addPolyhedraGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	Reflist<Atom,int> atoms;
	Atom *i, *j;
	Refitem<Atom,int> *ri, *rj, *rk;
	Glyph *g;
	while (nSelected_ > 0)
	{
		// If 'centresonly' is true, for each selected atom use its bonds to determine the atom list
		// If false, find individual fragments from the selection
		i = firstSelected();
		atoms.clear();
		if (centresonly)
		{
			deselectAtom(i);
			for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
				{
					j = bref->item->partner(i);
					if (j->nBonds() == 1) atoms.add(j);
				}
		}
		else
		{
			fragmentFromSelection(i, atoms);
			// Prune reflist to contain only those atoms with 1 bond
			ri = atoms.first();
			while (ri != NULL)
			{
				rj = ri->next;
				if (ri->item->nBonds() != 1) atoms.remove(ri);
				ri = rj;
			}
		}
		// Now we have the atom list, construct the glyphs
		// Lazy approach - add all possible triangles between atoms
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			for (rj = ri->next; rj != NULL; rj = rj->next)
			{
				// Check distance between atoms
				if (cell_.distance(ri->item, rj->item) > rcut) continue;
				for (rk = rj->next; rk != NULL; rk = rk->next)
				{
					// Check 
					g = addGlyph(Glyph::TriangleGlyph);
					if (linkatoms)
					{
						g->setAtom(0, ri->item->id(), GlyphData::PositionData);
						g->setAtom(1, rj->item->id(), GlyphData::PositionData);
						g->setAtom(2, rk->item->id(), GlyphData::PositionData);
					}
					else
					{
						g->setVector(0, ri->item->r());
						g->setVector(1, rj->item->r());
						g->setVector(2, rk->item->r());
					}
				}
			}
		}
	}
	msg.exit("Model::addPolyhedraGlyphs");
}

// Automatically add ellipsoids to current atom selection 
void Model::addEllipsoidGlyphs()
{
	msg.enter("Model::addEllipsoidGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	Reflist<Atom,int> atoms;
	Atom *i, *xi, *xj, *yi;
	Vec3<double> centroid, x, y, z, v;
	double mag, best = 0.0, angle;
	Refitem<Atom,int> *ri, *rj;
	Glyph *g;
	while (nSelected_ > 0)
	{
		// If false, find individual fragments from the selection
		i = firstSelected();
		atoms.clear();
		fragmentFromSelection(i, atoms);
		// Check that we have enough atoms
		if (atoms.nItems() < 3) continue;
		// Find the pair of atoms with the largest separation - this will be the x-axis
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			for (rj = ri->next; rj != NULL; rj = rj->next)
			{
				v = ri->item->r() - rj->item->r();
				mag = v.magnitude();
				if (mag > best)
				{
					best = mag;
					xi = ri->item;
					xj = rj->item;
					x = v * 0.5;
					centroid = x + xj->r();
				}
			}
		}
		//printf("Atom ids %i and %i determine X and centroid\n", xi->id(), xj->id());
		g = addGlyph(Glyph::LineGlyph);
		g->setVector(0, centroid);
		g->setVector(1, centroid + x);
		g = addGlyph(Glyph::SphereGlyph);
		g->setVector(0, centroid);
		g->setVector(1, 0.1, 0.1, 0.1);

		// Now find the atom in the selection that makes the angle closest to 90deg with xi-centroid
		best = PI;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			if ((ri->item == xi) || (ri->item == xj)) continue;
			angle = fabs(cell_.angle(xi->r(), centroid, ri->item->r()) - PI);
			if (angle < best)
			{
				yi = ri->item;
				best = angle;
			}
		}
		//printf("Best Y atom is id %i, forming angle of %f Deg\n", yi->id(), cell_.angle(xi->r(), centroid, yi->r()) * DEGRAD);
		// Orthogonalise Y axis w.r.t. X axis, and create z-axis
		y = yi->r() - centroid;
		y.orthogonalise(x);
		g = addGlyph(Glyph::ArrowGlyph);
		g->setVector(0, centroid);
		g->setVector(1, centroid + y);
		z = x * y;
		g = addGlyph(Glyph::ArrowGlyph);
		g->setVector(0, centroid);
		g->setVector(1, centroid + z);
		g = addGlyph(Glyph::EllipsoidGlyph);
		g->setVector(0, centroid);
		g->setVector(1,x);
		g->setVector(2,y);
	}
	msg.exit("Model::addEllipsoidGlyphs");
}
