/*
	*** Model bond functions
	*** src/model/bond.cpp
	Copyright T. Youngs 2007-2009

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
#include "model/undostate.h"
#include "model/undoevent.h"
#include "base/bond.h"
#include "base/pattern.h"
#include "classes/prefs.h"

// Return first bond in the model
Bond *Model::bonds()
{
	return bonds_.first();
}

// Return number of bonds in the model
int Model::nBonds()
{
	return bonds_.nItems();
}

// Return the nth bond in the model
Bond *Model::bond(int n)
{
	return bonds_[n];
}

// Add Bond (pointers)
void Model::bondAtoms(Atom *i, Atom *j, Bond::BondType bt)
{
        // Create a new bond each atom and add them to the atom's own lists.
	msg.enter("Model::bondAtoms");
	if (i == j) msg.print("Cannot bond an atom to itself!\n");
	else
	{
		// Search for old bond between atoms
		Bond *b = i->findBond(j);
		// If we found one, just set the new bond order
		if (b != NULL)
		{
			// Check order of existing bond
			if (b->order() != bt)
			{
				Bond::BondType oldtype = b->type();
				b->setType(bt);
				changeLog.add(Log::Structure);
				// Add the change to the undo state (if there is one)
				if (recordingState_ != NULL)
				{
					BondTypeEvent *newchange = new BondTypeEvent;
					newchange->set(i->id(), j->id(), oldtype, bt);
					recordingState_->addEvent(newchange);
				}
			}
		}
		else
		{
			b = bonds_.add();
			b->setType(bt);
			b->setAtoms(i,j);
			i->acceptBond(b);
			j->acceptBond(b);
			changeLog.add(Log::Structure);
			// Add the change to the undo state (if there is one)
			if (recordingState_ != NULL)
			{
				BondEvent *newchange = new BondEvent;
				newchange->set(TRUE, i->id(), j->id(), bt);
				recordingState_->addEvent(newchange);
			}
		}
	}
	msg.exit("Model::bondAtoms");
}

// Add Bond (id's)
void Model::bondAtoms(int ii, int jj, Bond::BondType bt)
{
        // Create a new bond for each atom and add them to the atom's own lists.
	msg.enter("Model::bondAtoms[int]");
	//printf("Atom ids given to Model::bondAtoms() are %i and %i (natoms=%i)\n",ii,jj,atoms_.nItems());
	if (ii == jj) msg.print("Cannot bond an atom to itself!\n");
	else
	{
		// First, locate the two atoms with the specified id's
		Atom *i = atom(ii);
		Atom *j = atom(jj);
		if (i == NULL || j == NULL)
		{
			printf("Couldn't locate one or both atoms in bond with specified ids %i and %i\n",ii,jj);
			msg.exit("Model::bondAtoms[int]");
			return;
		}
		bondAtoms(i,j,bt);
	}
	msg.exit("Model::bondAtoms[int]");
}

// Delete Bond
void Model::unbondAtoms(Atom *i, Atom *j, Bond *bij)
{
        // Delete info from bond lists for atoms i and j.
	msg.enter("Model::unbondAtoms");
	// Find bond between atoms (unless already supplied)
	Bond *b;
	if (bij != NULL) b = bij;
	else
	{
		b = i->findBond(j);
		if (b == NULL)
		{
			printf("Couldn't locate bond to unbond!\n");
			msg.exit("Model::unbondAtoms");
			return;
		}
	}
	// Store type for use later
	Bond::BondType bt = b->type();
	i->detachBond(b);
	j->detachBond(b);
	bonds_.remove(b);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		BondEvent *newchange = new BondEvent;
		newchange->set(FALSE, i->id(), j->id(), bt);
		recordingState_->addEvent(newchange);
	}
	msg.exit("Model::unbondAtoms");
}

// Delete All Bonding
void Model::clearBonding()
{
	msg.enter("Model::clearBonding");
        // Clear the bond list.
	for (Atom *i = atoms_.first(); i != NULL; i = i->next)
	{
		Refitem<Bond,int> *bref = i->bonds();
		while (bref != NULL)
		{
			// Need to detach the bond from both atoms involved
			Bond *b = bref->item;
			Atom *j = b->partner(i);
			unbondAtoms(i,j,b);
			bref = i->bonds();
		}
	}
	changeLog.add(Log::Structure);
	msg.exit("Model::clearBonding");
}

// Initialise reflists based on current extent of model
void Model::initialiseBondingCuboids()
{
	int x,y,z,box,x2,y2,z2,n,m;
	Vec3<double> r;
	extentMin_ = 1e6;
	extentMax_ = -1e6;
	cuboidSize_.set(5.0,5.0,5.0);
	// Determine the number of cuboids to partition our space into
	if (cell_.type() == Cell::NoCell)
	{
		for (Atom *i = atoms_.first(); i != NULL; i = i->next)
		{
			r = i->r();
			if (r.x < extentMin_.x) extentMin_.x = r.x;
			else if (r.x > extentMax_.x) extentMax_.x = r.x;
			if (r.y < extentMin_.y) extentMin_.y = r.y;
			else if (r.y > extentMax_.y) extentMax_.y = r.y;
			if (r.z < extentMin_.z) extentMin_.z = r.z;
			else if (r.z > extentMax_.z) extentMax_.z = r.z;
		}
		extentRange_ = extentMax_ - extentMin_;
		cuboidBoxes_.x = (extentRange_.x / cuboidSize_.x) + 1;
		cuboidBoxes_.y = (extentRange_.y / cuboidSize_.y) + 1;
		cuboidBoxes_.z = (extentRange_.z / cuboidSize_.z) + 1;
	}
	else
	{
		// For periodic systems, we will use fractional coordinates to partition the atoms
		foldAllAtoms();
		extentMin_ = 0.0;
		extentMax_ = 1.0;
		extentRange_ = 1.0;
		// Work out the box sizes necessary in each direction to get a suitable box length and no remainder of 'overlap'
		Vec3<double> lengths = cell_.lengths();
		lengths /= cuboidSize_;
		Vec3<int> ilengths;
		ilengths.set(lengths.x, lengths.y, lengths.z);
		cuboidSize_.set(1.0 / ilengths.x, 1.0 / ilengths.y, 1.0 / ilengths.z);
		cuboidBoxes_.set(ilengths.x, ilengths.y, ilengths.z);
	}
	// Create reference arrays
	nCuboids_ = cuboidBoxes_.x * cuboidBoxes_.y * cuboidBoxes_.z;
	cuboidYZ_ = cuboidBoxes_.y * cuboidBoxes_.z;
	freeBondingCuboids();
	bondingCuboids_ = new Reflist<Atom,double>[nCuboids_];
	bondingOverlays_ = new Reflist<Atom,double>[nCuboids_];
// 	printf("Box counts: x = %i, y = %i, z = %i, cube = %i\n", cuboidBoxes_.x, cuboidBoxes_.y, cuboidBoxes_.z, nCuboids_);
// 	printf("CuboidSize = "); cuboidSize_.print();
// 	printf("ExtentMin = "); extentMin_.print(); 
// 	printf("ExtentMax = "); extentMax_.print();
// 	printf("ExtentRange = "); extentRange_.print();
}

// Free any created reflists
void Model::freeBondingCuboids()
{
	if (bondingCuboids_ != NULL) delete[] bondingCuboids_;
	if (bondingOverlays_ != NULL) delete[] bondingOverlays_;
	bondingCuboids_ = NULL;
	bondingOverlays_ = NULL;
}

// Add atom to cuboid reflists
void Model::addAtomToCuboid(Atom *i)
{
	int x,y,z;
	Vec3<double> r = (cell_.type() == Cell::NoCell ? i->r() : cell_.realToFrac(i->r()));
	r -= extentMin_;
	double radius = elements().atomicRadius(i->element());
	x = int(r.x / cuboidSize_.x);
	y = int(r.y / cuboidSize_.y);
	z = int(r.z / cuboidSize_.z);
	if (x == cuboidBoxes_.x) x = 0;
	if (y == cuboidBoxes_.y) y = 0;
	if (z == cuboidBoxes_.z) z = 0;
// 	printf("Atom %i is in box %i-%i-%i=%i\n ", i->id(), x,y,z,x*cuboidYZ_+y*cuboidBoxes_.z+z);
	bondingCuboids_[x*cuboidYZ_+y*cuboidBoxes_.z+z].add(i, radius);
	// Add to overlay box
// 	printf("--> Original position"); r.print();
	r += cuboidSize_*0.5;
	if (r.x > extentRange_.x) r.x -= extentRange_.x;
	if (r.y > extentRange_.y) r.y -= extentRange_.y;
	if (r.z > extentRange_.z) r.z -= extentRange_.z;
// 	printf("--> Overlay position"); r.print();
	x = int(r.x / cuboidSize_.x);
	y = int(r.y / cuboidSize_.y);
	z = int(r.z / cuboidSize_.z);
	if (x == cuboidBoxes_.x) x = 0;
	if (y == cuboidBoxes_.y) y = 0;
	if (z == cuboidBoxes_.z) z = 0;
// 	printf("and overlay %i-%i-%i (%i)\n",x,y,z,x*cuboidBoxes_.y*cuboidBoxes_.z+y*cuboidBoxes_.z+z);
	bondingOverlays_[x*cuboidYZ_+y*cuboidBoxes_.z+z].add(i, radius);
	// We also add atoms that are on the very edges of the overlays to the ones on the other side (to account for MIM)
// 	printf("If x == 0, overlay is %i\n",(cuboidBoxes_.x-1)*cuboidBoxes_.y*cuboidBoxes_.z+y*cuboidBoxes_.z+z);
	if (x == 0)
	{
		bondingOverlays_[(cuboidBoxes_.x-1)*cuboidYZ_+y*cuboidBoxes_.z+z].add(i, radius);	// Xyz
		if (y == 0)
		{
			bondingOverlays_[(cuboidBoxes_.x-1)*cuboidYZ_+(cuboidBoxes_.y-1)*cuboidBoxes_.z+z].add(i, radius);	//XYz
			if (z == 0) bondingOverlays_[(cuboidBoxes_.x-1)*cuboidYZ_+(cuboidBoxes_.y-1)*cuboidBoxes_.z+cuboidBoxes_.z-1].add(i, radius); // XYZ
		}
		else if (z == 0) bondingOverlays_[(cuboidBoxes_.x-1)*cuboidYZ_+y*cuboidBoxes_.z+cuboidBoxes_.z-1].add(i, radius); // XyZ
	}
	if (y == 0)
	{
		bondingOverlays_[x*cuboidYZ_+(cuboidBoxes_.y-1)*cuboidBoxes_.z+z].add(i, radius);	// xYz
		if (z == 0) bondingOverlays_[x*cuboidYZ_+(cuboidBoxes_.y-1)*cuboidBoxes_.z+cuboidBoxes_.z-1].add(i, radius); // xYZ
	}
	if (z == 0) bondingOverlays_[x*cuboidYZ_+y*cuboidBoxes_.z+cuboidBoxes_.z-1].add(i, radius);	// xyZ

// 	bondingOverlays_[(cuboidBoxes_.x-1)*cuboidYZ_+y*cuboidBoxes_.z+z].add(i, radius);	// xyz
// 	printf("If x == y, overlay is %i\n",x*cuboidBoxes_.y*cuboidBoxes_.z+(cuboidBoxes_.y-1)*cuboidBoxes_.z+z);
// 	if (y == 0) bondingOverlays_[x*cuboidYZ_+(cuboidBoxes_.y-1)*cuboidBoxes_.z+z].add(i, radius);	// xYz
// 	printf("If x == z, overlay is %i\n",x*cuboidBoxes_.y*cuboidBoxes_.z+y*cuboidBoxes_.z+cuboidBoxes_.z-1);
// 	if (z == 0) bondingOverlays_[x*cuboidYZ_+y*cuboidBoxes_.z+cuboidBoxes_.z-1].add(i, radius);	// xyZ
}

// Calculate bonding from stored cuboid lists
void Model::rebond()
{
	int n, m, x, y, z, x2, y2, z2, checklist[8];
	double dist, radsum;
	double tolerance = prefs.bondTolerance();
	Atom *i, *j;
	// Loop over cuboids, checking distances with atoms in adjacent boxes
	Refitem<Atom,double> *ri, *rj;
	x = 0;
	y = 0;
	z = 0;
	for (n = 0; n<nCuboids_; n++)
	{
		if (bondingCuboids_[n].nItems() != 0)
		{
	// 		if (cuboids[n].nItems() > 0) printf("On cuboid %i (%ix%ix%i) which contains %i atoms\n", n, x, y, z, cuboids[n].nItems());
			// For each of the atoms in the cuboid, check distance with each atom in eight of the closest
			// overlay boxes. 
			checklist[0] = n;
			x2 = (x == (cuboidBoxes_.x-1) ? 0 : x+1);
			y2 = (y == (cuboidBoxes_.y-1) ? 0 : y+1);
			z2 = (z == (cuboidBoxes_.z-1) ? 0 : z+1);
	// 		if (cuboids[n].nItems() > 0) printf("....xyz = %i,%i,%i, xyz2 = %i,%i,%i\n", x, y, z, x2, y2, z2);
			checklist[1] = x2*cuboidYZ_+y*cuboidBoxes_.z+z;
			checklist[2] = x2*cuboidYZ_+y2*cuboidBoxes_.z+z;
			checklist[3] = x2*cuboidYZ_+y*cuboidBoxes_.z+z2;
			checklist[4] = x2*cuboidYZ_+y2*cuboidBoxes_.z+z2;
			checklist[5] = x*cuboidYZ_+y2*cuboidBoxes_.z+z;
			checklist[6] = x*cuboidYZ_+y*cuboidBoxes_.z+z2;
			checklist[7] = x*cuboidYZ_+y2*cuboidBoxes_.z+z2;
			for (ri = bondingCuboids_[n].first(); ri != NULL; ri = ri->next)
			{
				i = ri->item;
				// Check for excluded elements
				if (i->element() == 0) continue;
				for (m=0; m<8; m++)
				{
					for (rj = bondingOverlays_[checklist[m]].first(); rj != NULL; rj = rj->next)
					{
						j = rj->item;
						if (i == j) continue;
						if (j->element() == 0) continue;
						dist = cell_.distance(i,j);
						radsum = ri->data + rj->data;
						if (dist < radsum*tolerance) bondAtoms(i,j,Bond::Single);
					}
				}
			}
		}
		// Increase x,y,z indices for box lookup
		z ++;
		if (z == cuboidBoxes_.z)
		{
			z = 0;
			y ++;
			if (y == cuboidBoxes_.y)
			{
				y = 0;
				x ++;
			}
		}
	}
}

// Calculate Bonding
void Model::calculateBonding()
{
	msg.enter("Model::calculateBonding");
	Atom *i, *j;
	double dist, radsum;
	double tolerance = prefs.bondTolerance();
	msg.print(Messenger::Verbose, "Calculating bonds in model (tolerance = %5.2f)...",tolerance);
	clearBonding();
	// Create cuboid lists
	initialiseBondingCuboids();
	// Add all atoms to cuboid list
	for (i = atoms_.first(); i != NULL; i = i->next) addAtomToCuboid(i);
	// Rebond within the cuboids
	rebond();
	// Free bonding cuboids
	freeBondingCuboids();
	msg.print(Messenger::Verbose, "Done.\n");
	msg.exit("Model::calculateBonding");
}

// Calculate Bonding within Patterns
void Model::patternCalculateBonding()
{
	msg.enter("Model::patternCalculateBonding");
	Atom *i,*j;
	int ii, jj, el, m;
	double dist;
	double tolerance = prefs.bondTolerance();
	double radius_i, radsum;
	clearBonding();
	msg.print("Calculating bonds within patterns (tolerance = %5.2f)...",tolerance);
	// For all the pattern nodes currently defined, bond within molecules
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next)
	{
		// Loop over molecules
		i = p->firstAtom();
		for (m=0; m<p->nMolecules(); m++)
		{
			for (ii = 0; ii < p->nAtoms()-1; ii ++)
			{
				// Check for excluded elements
				el = i->element();
				if (el == 0)
				{
					i = i->next;
					continue;
				}
				radius_i = elements().atomicRadius(el);
				// Start loop over second atom in molecule
				j = i->next;
				for (jj = ii+1; jj < p->nAtoms(); jj ++)
				{
					el = j->element();
					if (el == 0)
					{
						j = j->next;
						continue;
					}
					dist = cell_.distance(i,j);
				//printf("i %i j %i dist %8.3f\n",i->id(),j->id(),dist);
					radsum = radius_i + elements().atomicRadius(el);
					if (dist < radsum*tolerance) bondAtoms(i,j,Bond::Single);
					j = j->next;
				}
				i = i->next;
			}
			// Skip on one more atom, since the i loop ran from 0 to natoms-1
			i = i->next;
		}
	}
	msg.print(" Done.\n");
	msg.exit("Model::patternCalculateBonding");
}

// Calculate Bonding in current selection
void Model::selectionCalculateBonding()
{
	msg.enter("Model::selectionCalculateBonding");
	double tolerance = prefs.bondTolerance();
	double radsum, dist;
	Atom *i, *j;
	// Create cuboid lists
	initialiseBondingCuboids();
	// Add all atoms to cuboid list
	for (i = atoms_.first(); i != NULL; i = i->next) if (i->isSelected()) addAtomToCuboid(i);
	// Rebond within the cuboids
	rebond();
	// Free bonding cuboids
	freeBondingCuboids();
	msg.exit("Model::selectionCalculateBonding");
}

// Bond all atoms in current selection
void Model::selectionBondAll()
{
	// Add bonds between all atoms in current selection
	msg.enter("Model::selectionBondAll");
	Atom *i, *j;
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isSelected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->isSelected())
					if (i->findBond(j) == NULL) bondAtoms(i,j,Bond::Single);
			}
		}
	}
	msg.exit("Model::selectionBondAll");
}

// Clear Bonding in current selection
void Model::selectionClearBonding()
{
	// Clear all bonds between currently selected atoms
	msg.enter("Model::selectionClearBonding");
	Atom *i, *j;
	for (i = atoms_.first(); i != NULL; i = i->next)
	{
		if (i->isSelected())
		{
			for (j = i->next; j != NULL; j = j->next)
			{
				if (j->isSelected())
					if (i->findBond(j) != NULL) unbondAtoms(i,j);
			}
		}
	}
	msg.exit("Model::selectionClearBonding");
}

// Alter type of bond
void Model::changeBond(Bond *b, Bond::BondType bt)
{
	Bond::BondType oldorder = b->type();
	b->setType(bt);
	changeLog.add(Log::Structure);
	// Add the change to the undo state (if there is one)
	if (recordingState_ != NULL)
	{
		BondTypeEvent *newchange = new BondTypeEvent;
		newchange->set(b->atomI()->id(), b->atomJ()->id(), oldorder, bt);
		recordingState_->addEvent(newchange);
	}
}

// Augment bonding for all model patterns
void Model::augmentBonding()
{
	msg.enter("Model::augmentBonding");
	if (!autocreatePatterns())
	{
		msg.print("Can't augment bonding without a valid pattern.\n");
		msg.exit("Model::augmentBonding");
		return;
	}
	describeAtoms();
	for (Pattern *p = patterns_.first(); p != NULL; p = p->next) p->augment();
	msg.exit("Model::augmentBonding");
}
