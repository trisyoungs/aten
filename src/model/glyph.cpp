/*
	*** Model glyph functions
	*** src/model/glyph.cpp
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

#include "base/glyph.h"
#include "model/model.h"

// Create new glyph in this model
Glyph *Model::addGlyph(Glyph::GlyphType gt)
{
	changeLog.add(Log::Glyphs);
	changeLog.add(Log::Visual);
	Glyph *newglyph = glyphs_.add();
	newglyph->setParent(this);
	newglyph->setType(gt);
	return newglyph;
}

// Remove specified glyph from model
void Model::removeGlyph(Glyph *g)
{
	glyphs_.remove(g);
	changeLog.add(Log::Glyphs);
	changeLog.add(Log::Visual);	
}

// Return number of defined glyphs
int Model::nGlyphs()
{
	return glyphs_.nItems();
}

// Return list of glyphs
Glyph *Model::glyphs()
{
	return glyphs_.first();
}

// Return specific glyph
Glyph *Model::glyph(int n)
{
	if ((n < 0) || (n >= glyphs_.nItems()))
	{
		msg.print("Glyph index %i is out of range for list.\n", n);
		return NULL;
	}
	return glyphs_[n];
}

// Return vector data for glyph
Vec3<double> Model::glyphVector(Glyph *g, int dataid)
{
	Atom *i;
	if ((dataid < 0) || (dataid >= g->nData())) msg.print( "Tried to get vector %i from glyph when it has only %i in total.\n", dataid+1, g->nData());
	else
	{
		if (g->data(dataid)->atomSetLast())
		{
			i = g->data(dataid)->atom();
			if (i == NULL)
			{
				msg.print( "Atom was apparently set last in glyph, but stored pointer is NULL.\n");
				return Vec3<double>();
			}
// 			// Check range of stored atom id
// 			if (id >= atoms_.nItems())
// 			{
// 				msg.print( "Atom ID set in glyph (%i) is outside range for model.\n", id);
// 				return Vec3<double>();
// 			}
// 			Atom *i = atoms_[id];
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
	msg.enter("Model::addPolyhedraGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	Reflist<Atom,int> atoms;
	Atom *i, *j;
	Refitem<Atom,int> *ri, *rj, *rk;
	Glyph *g;
	while (selection_.nItems() > 0)
	{
		// If 'centresonly' is true, for each selected atom use its bonds to determine the atom list
		// If false, find individual fragments from the selection
		i = selection_.first()->item;
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
	msg.exit("Model::addPolyhedraGlyphs");
}

// Automatically add ellipsoids to current atom selection 
void Model::addEllipsoidGlyphs()
{
	msg.enter("Model::addEllipsoidGlyphs");
	// From the current selection of atoms, add polyhedra to/around them.
	Reflist<Atom,int> atoms;
	Vec3<double> centroid, v, extents;
	Mat3<double> axes;
	double mag, best = 0.0, angle, tolerance = 0.3, angletol = PI/4.0, minz, maxz;
	Refitem<Atom,int> *ri;
	Atom *i;
	Reflist<Atom, Vec3<double> > xaxisatoms;
	Refitem<Atom, Vec3<double> > *rid;
	Glyph *g;
	while (selection_.nItems() > 0)
	{
		// If false, find individual fragments from the selection
		i = selection_.first()->item;
		atoms.clear();
		fragmentFromSelection(i, atoms);
		// Check that we have enough atoms
		if (atoms.nItems() < 3) continue;
		// Get centre of current selection
		centroid.zero();
		for (ri = atoms.first(); ri != NULL; ri = ri->next) centroid += cell_.mim(ri->item, atoms.first()->item);
		centroid /= atoms.nItems();

		/*
		// 1) Determine the X-axis (choose this to be defined from the (set of) atom(s) farthest from the centroid)
		// From the centroid, measure the distance of each atom. If it is less than a tolerance amount of the current
		// maximum, ignore it. If it is within this tolerance, add it to the list of defining atoms (after considering
		// the angle it makes with the current x-axis point). If it is greater than tolerance of the current maximum, 
		// clear the list and start again with this atom.
		*/
		xaxisatoms.clear();
		extents.x = 0.0;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			v = cell_.mimd(ri->item->r(), centroid);
			mag = v.magnitude();
			//printf("Atom ID %i is %f from centroid (current max = %f)\n", ri->item->id(), mag, extents.x);
			if (mag < (extents.x - tolerance)) continue;
			if (fabs(extents.x - mag) < tolerance)
			{
				// This atom is at a distance close to the current maximal point, so check the angle it makes with the current axis
				angle = cell_.angle(ri->item->r(), centroid, centroid + axes.x());
				if (angle < angletol)
				{
					// Reconstruct xaxis and add to list of atoms
					axes.x() = v;
					for (rid = xaxisatoms.first(); rid != NULL; rid = rid->next) axes.x() += rid->data;
					axes.x() /= xaxisatoms.nItems() + 1;
					xaxisatoms.add(i, v);
					//printf("There are now %i atoms defining the x-axis, which is ", xaxisatoms.nItems());
					//axes.x().print();
				}
			}
			else
			{
				// This atom is further than the tolerance away from the current maximum, so clear list and start again
				xaxisatoms.clear();
				xaxisatoms.add(ri->item, v);
				axes.x() = v;
				extents.x = mag;
			}
		}
// 		g = addGlyph(Glyph::ArrowGlyph);
// 		g->data(0)->setVector(centroid);
// 		g->data(1)->setVector(centroid+axes.x());
		axes.x().normalise();

		/*
		// 2) Determine Y-axis.
		// Take the atom that makes the angle closest to 90deg with the x-axis, weighted by its distance
		*/
		// Now find the atom in the selection that makes the angle closest to 90deg with xi-centroid and is furthest away
		best = TWOPI;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			v = cell_.mimd(centroid, ri->item->r());
			mag = v.magnitude();
			//angle = fabs(cell_.angle(xi->r(), centroid, ri->item->r()));
			angle = cell_.angle(axes.x() + centroid, centroid, ri->item->r());
			if (angle < HALFPI) angle = HALFPI + (HALFPI - angle);
			// Magnitudinalise (!) w.r.t. distance as well.
			angle /= mag;
			//printf("Anglemag for atom %i is %f\n", ri->item->id(), angle);
			if (angle < best)
			{
				axes.y() = v;
				best = angle;
				extents.y = mag;
			}
		}
		// Must orthogonalise y-axis w.r.t. x
		axes.y().orthogonalise(axes.x());
// 		g = addGlyph(Glyph::ArrowGlyph);
// 		g->data(0)->setVector(centroid);
// 		g->data(1)->setVector(centroid+axes.y());
		axes.y().normalise();

		/*
		// 3) Construct Z-axis from X and Y, and determine extent
		*/
		axes.z() = axes.x() * axes.y();
		minz = 1000.0;
		maxz = -1000.0;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			v = (ri->item->r() - centroid) * axes;
			if (v.z < minz) minz = v.z;
			else if (v.z > maxz) maxz = v.z;
		}
		if (fabs(minz) > maxz) maxz = fabs(minz);
		extents.z = maxz;
		if (extents.z < 0.5) extents.z = 0.5;
		axes.rowMultiply(extents);
		g = addGlyph(Glyph::EllipsoidXYZGlyph);
		g->data(0)->setVector(centroid);
		g->data(1)->setVector(axes.x());
		g->data(2)->setVector(axes.y());
		g->data(3)->setVector(axes.z());

/*
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
					axes.x() = v * 0.5;
					centroid = axes.x() + xj->r();
					extents.x = axes.x().magAndNormalise();
				}
			}
		}
		//printf("Atom ids %i and %i determine X and centroid\n", xi->id(), xj->id());

		// Now find the atom in the selection that makes the angle closest to 90deg with xi-centroid and is furthest away
		best = TWOPI;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			if ((ri->item == xi) || (ri->item == xj)) continue;
			mag = cell_.distance(centroid, ri->item->r());
			//angle = fabs(cell_.angle(xi->r(), centroid, ri->item->r()));
			angle = cell_.angle(xi->r(), centroid, ri->item->r());
			if (angle < HALFPI) angle = HALFPI + (HALFPI - angle);
			// Magnitudinalise (!) w.r.t. distance as well.
			angle /= mag;
			//printf("Anglemag for atom %i is %f\n", ri->item->id(), angle);
			if (angle < best)
			{
				yi = ri->item;
				best = angle;
				extents.y = mag;
			}
		}
		//printf("Best Y atom is id %i, forming anglemag of %f Deg\n", yi->id(), cell_.angle(xi->r(), centroid, yi->r()));
		// Orthogonalise Y axis w.r.t. X axis, and create z-axis
		axes.y() = cell_.mimd(yi->r(), centroid);
		axes.y().orthogonalise(axes.x());
		axes.y().normalise();
		axes.z() = axes.x() * axes.y();
		// With the three axes so obtained, determine the extent of the ellipsoid in the Z direction
		minz = 1000.0;
		maxz = -1000.0;
		for (ri = atoms.first(); ri != NULL; ri = ri->next)
		{
			v = (ri->item->r() - centroid) * axes;
			if (v.z < minz) minz = v.z;
			else if (v.z > maxz) maxz = v.z;
		}
		if (fabs(minz) > maxz) maxz = fabs(minz);
		extents.z = maxz;
		if (extents.z < 0.5) extents.z = 0.5;
		axes.rowMultiply(extents);
		g = addGlyph(Glyph::EllipsoidXYZGlyph);
		g->data(0)->setVector(centroid);
		g->data(1)->setVector(axes.x());
		g->data(2)->setVector(axes.y());
		g->data(3)->setVector(axes.z());*/
	}
	msg.exit("Model::addEllipsoidGlyphs");
}
