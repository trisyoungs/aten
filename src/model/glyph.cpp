/*
	*** Model glyph functions
	*** src/model/glyph.cpp
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
#include "model/undoevent.h"
#include "model/undostate.h"

ATEN_USING_NAMESPACE

// Create new glyph in this model
Glyph* Model::addGlyph(Glyph::GlyphType gt)
{
	logChange(Log::Glyphs);
	Glyph* newglyph = glyphs_.add();
	newglyph->setParent(this);
	newglyph->setType(gt);
	if ((gt == Glyph::TextGlyph) || (gt == Glyph::Text3DGlyph)) textGlyphs_.add(newglyph);

	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
// 		GlyphEvent* newchange = new GlyphEvent;	// TODO
// 		newchange->set(true, newglyph);
// 		recordingState_->addEvent(newchange);
	}

	return newglyph;
}

// Remove specified glyph from model
void Model::removeGlyph(Glyph* g)
{
	if ((g->type() == Glyph::TextGlyph) || (g->type() == Glyph::Text3DGlyph)) textGlyphs_.remove(g);
	glyphs_.remove(g);
	logChange(Log::Glyphs);
}

// Return number of defined glyphs
int Model::nGlyphs() const
{
	return glyphs_.nItems();
}

// Return list of glyphs
Glyph* Model::glyphs() const
{
	return glyphs_.first();
}

// Return first text glyph in list (if any)
RefListItem<Glyph,int>* Model::textGlyphs() const
{
	return textGlyphs_.first();
}

// Return specific glyph
Glyph* Model::glyph(int n)
{
	if ((n < 0) || (n >= glyphs_.nItems()))
	{
		Messenger::print("Glyph index %i is out of range for list.", n);
		return NULL;
	}
	return glyphs_[n];
}

// Return vector data for glyph
Vec3<double> Model::glyphVector(Glyph* g, int dataid) const
{
	Atom* i;
	if ((dataid < 0) || (dataid >= g->nData())) Messenger::print("Tried to get vector %i from glyph when it has only %i in total.", dataid+1, g->nData());
	else
	{
		if (g->data(dataid)->atomSetLast())
		{
			i = g->data(dataid)->atom();
			if (i == NULL)
			{
				Messenger::print("Atom was apparently set last in glyph, but stored pointer is NULL.");
				return Vec3<double>();
			}
// 			// Check range of stored atom id
// 			if (id >= atoms_.nItems())
// 			{
// 				Messenger::print("Atom ID set in glyph (%i) is outside range for model.", id);
// 				return Vec3<double>();
// 			}
// 			Atom* i = atoms_[id];
			switch (g->data(dataid)->atomData())
			{
				case (GlyphData::PositionData):
					return i->r();
				case (GlyphData::ForceData):
					return i->f();
				case (GlyphData::VelocityData):
					return i->v();
			}
		}
		// Default return value is vector data
		return g->data(dataid)->vector();
	}
	return Vec3<double>();
}

// Automatically add polyhedra to current atom selection 
void Model::addPolyhedraGlyphs(bool centresonly, bool linkatoms, double rcut)
{
	Messenger::enter("Model::addPolyhedraGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	RefList<Atom,int> atoms;
	Atom* i, *j;
	RefListItem<Atom,int>* ri, *rj, *rk;
	Glyph* g;
	while (selection_.nItems() > 0)
	{
		// If 'centresonly' is true, for each selected atom use its bonds to determine the atom list
		// If false, find individual fragments from the selection
		i = selection_.first()->item;
		atoms.clear();
		if (centresonly)
		{
			deselectAtom(i);
			for (RefListItem<Bond,int>* bref = i->bonds(); bref != NULL; bref = bref->next)
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
						g->data(0)->setAtom(ri->item);
						g->data(1)->setAtom(rj->item);
						g->data(2)->setAtom(rk->item);
						g->data(0)->setAtomData(GlyphData::PositionData);
						g->data(1)->setAtomData(GlyphData::PositionData);
						g->data(2)->setAtomData(GlyphData::PositionData);
					}
					else
					{
						g->data(0)->setVector(ri->item->r());
						g->data(1)->setVector(rj->item->r());
						g->data(2)->setVector(rk->item->r());
					}
				}
			}
		}
	}
	Messenger::exit("Model::addPolyhedraGlyphs");
}

// Automatically add ellipsoids to current atom selection 
void Model::addEllipsoidGlyphs()
{
	Messenger::enter("Model::addEllipsoidGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	RefList<Atom,int> atoms;
	Vec3<double> centroid, v, u, vecx, vecy, vecz;
	double mag, best, r, phi;
	RefListItem<Atom,int>* ri, *rj;
	Atom* i, *j, *k, *l;
	Matrix A;
	RefList<Atom, Vec3<double> > xaxisatoms;
	RefListItem<Atom, Vec3<double> >* rid;
	Glyph* g;
	while (selection_.nItems() > 0)
	{
		// If false, find individual fragments from the selection
		i = selection_.first()->item;
		atoms.clear();
		fragmentFromSelection(i, atoms);
		
		// If only one atom selected, draw small sphere
		if (atoms.nItems() == 1)
		{
			// Create glyph
			v = atoms.first()->item->r();
			g = addGlyph(Glyph::EllipsoidXYZGlyph);
			g->data(0)->setVector(v);
			g->data(1)->setVector(Vec3<double>(v.x+0.1,v.y,v.z));
			g->data(2)->setVector(Vec3<double>(v.x,v.y+0.1,v.z));
			g->data(3)->setVector(Vec3<double>(v.x,v.y,v.z+0.1));
			Messenger::exit("Model::addEllipsoidGlyphs");
			return;
		}
		
		// Find most distant two points - this will be the x-axis of the ellipsoid
		best = 0.0;
		i = NULL;
		j = NULL;
		for (ri = atoms.first(); ri->next != NULL; ri = ri->next)
		{
			for (rj = ri->next; rj != NULL; rj = rj->next)
			{
				mag = cell_.distance(ri->item, rj->item);
				if (mag > best)
				{
					best = mag;
					i = ri->item;
					j = rj->item;
				}
			}
		}
// 		printf("Atom pointers are %p and %p\n", i, j);
// 		printf("Distance is greatest (%f) between atoms %i and %i\n", best, i->id(), j->id());
		centroid = i->r() + cell_.mimVector(i,j) * 0.5;
// 		centroid.print();
		mag = best * 0.5;
		vecx = i->r() - centroid;
// 		vecx.print();
		
		// Determine Y-vector from atom with greatest distance (on unit sphere) from centroid
		k = NULL;
		best = 0.0;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			if ((ri->item == i) || (ri->item == j)) continue;
			v = ri->item->r();
			// Determine angle with current x-axis and 2D polar r-coordinate (squared)
			phi = cell_.angle(i->r(), centroid, v) / DEGRAD;
			v -= centroid;
			r = (cos(phi)*mag)*(cos(phi)*mag) + (sin(phi)*v.magnitude())*(sin(phi)*v.magnitude());
			if (r > best)
			{
				k = ri->item;
				vecy = v;
				vecy.orthogonalise(vecx);
				vecy.normalise();
				vecy *= v.magnitude();
				best = r;
			}
		}
		// If no Y-pointer found, set to orthogonal and 10% of X magnitude
		if (k == NULL) vecy = vecx.orthogonal() * vecx.magnitude()*0.1;
// 		printf("Atom Y pointer is %p\n", k);
// 		vecy.print();
		
		// Determine z-vector from atom with greatest angle out of xy plane
		vecz = vecx*vecy;
		vecz.normalise();
		l = NULL;
		best = 0.0;
		A.setColumn(0, vecx, 0.0);
		A.setColumn(1, vecy, 0.0);
		A.setColumn(2, vecz, 0.0);
		A.invert();
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			if ((ri->item == i) || (ri->item == j) || (ri->item == k)) continue;
			v = ri->item->r() - centroid;
			phi = acos(v.dp(vecz)) * DEGRAD;
			if (fabs(phi-90) < 1.0) continue;
			// Calculate unit coordinate
			u = A * v;
			mag = u.magnitude();
			if (mag > best)
			{
				best = mag;
				l = ri->item;
			}
		}
// 		printf("Atom Z pointer is %p\n", l);
		// If 'l' pointer is NULL (no suitable atom found) make z-axis magnitude 10% of x-axis 
		if (l == 0) vecz *= vecx.magnitude()*0.1;

		// Create glyph
		g = addGlyph(Glyph::EllipsoidXYZGlyph);
		g->data(0)->setVector(centroid);
		g->data(1)->setVector(vecx+centroid);
		g->data(2)->setVector(vecy+centroid);
		g->data(3)->setVector(vecz+centroid);
	}
	
	Messenger::exit("Model::addEllipsoidGlyphs");
	return;
}
