/*
	*** Model build functions
	*** src/model/build.cpp
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

#include "model/model.h"
#include "base/elements.h"

/*
// Sketching functions
*/

// Add hydrogens to model
void Model::hydrogenSatisfy(Atom *target)
{
	// Cycles over atoms in model (or only the atom supplied), and works out how many hydrogens (and in which geometry) should be added to each
	dbgBegin(Debug::Calls,"Model::hydrogenSatisfy");
	int numh, tbo, nsingle, ndouble;
	Atom *i, *endatom;
	i = (target == NULL ? atoms_.first() : target);
	endatom = (target == NULL ? NULL : target->next);
	for (i = i; i != endatom; i = i->next)
	{
		// Step 1 - Work out how many single-bonds (i.e. hydrogens) we need to add to satisfy the atom's valency
		// Calculate total bond order of atom and work out single bond deficit
		tbo = i->totalBondOrder();
		numh = (elements.valency(i)*2 - tbo) / 2;
		// Step 2 - Work out geometry that we'll add hydrogens in, based on the atom's valency
		nsingle = i->countBonds(Bond::Single);
		ndouble = i->countBonds(Bond::Double);
		if (numh != 0)
		{
			// Simplest cases - atom has no bonds or all single bonds - we add in a tetrahedral geometry
			if (i->nBonds() == 0 || i->nBonds() == nsingle) addHydrogens(i,numh,Atom::TetrahedralHydrogen);
			// Otherwise, must work out the correct geometry to add hydrogens in...
			else if (ndouble != 0) addHydrogens(i,numh,Atom::PlanarHydrogen);
			else addHydrogens(i,numh,Atom::LinearHydrogen);
		}
	}
	projectAll();
	dbgEnd(Debug::Calls,"Model::hydrogenSatisfy");
}

// Iteratively add hydrogens to specified atom (giving supplied geometry)
void Model::addHydrogens(Atom *target, int nhydrogen, Atom::HAddGeom geometry)
{
	// Iteratively add hydrogens to the molecule conforming to the desired geometry specified
	dbgBegin(Debug::Calls,"atom::addHydrogens");
	Atom *a1, *a2, *a3;
	Atom *newh;
	Vec3<double> mim_a1, mim_a2, mim_a3, perp, perp2, newhpos, tempv;
	double bondlength = 1.08, theta;
	int minel, onebelow, oneabove;
	Refitem<Bond,int> *firstbond;
	// Add new hydrogens based on the geometry type, and then work out from what bonds there are already...
	switch (geometry)
	{
		case (Atom::LinearHydrogen): theta = 180.0 / DEGRAD; break;
		case (Atom::PlanarHydrogen): theta = 120.0 / DEGRAD; break;
		case (Atom::TetrahedralHydrogen): theta = 109.5 / DEGRAD; break;
	}
	// Switches put new coordinates in 'newhpos' - an atom is created an placed here at the end
	for (int n=0; n<nhydrogen; n++)
	{
		firstbond = target->bonds();
		switch (target->nBonds())
		{
			case (0):
				// No bonds, so add at arbitrary position along y-axis
				newhpos.set(0.0,bondlength,0.0);
				break;
			case (1):
				// Only one bond, so add atom at arbitrary position with angle of required geometry
				a1 = firstbond->item->partner(target);
				mim_a1 = cell_.mimd(target,a1);
				mim_a1.normalise();
				// Create perpendicular vector to X-i...
				minel = mim_a1.absMinElement();
				onebelow = (minel+2)%3;
				oneabove = (minel+1)%3;
				perp.set(minel,0.0);
				perp.set(onebelow,mim_a1.get(oneabove));
				perp.set(oneabove,-mim_a1.get(onebelow));
				perp.normalise();
				newhpos = mim_a1 * -bondlength * cos(theta) + perp * bondlength * sin(theta);
				break;
			case (2):
				// Two bonds, so work out 'pointing' vector and adjust to desired angle (if !Atom::PlanarHydrogen)
				// Get mim coordinates of the two bound atoms
				a1 = firstbond->item->partner(target);
				a2 = firstbond->next->item->partner(target);
				mim_a1 = cell_.mimd(a1,target);
				mim_a1.normalise();
				mim_a2 = cell_.mimd(a2,target);
				mim_a2.normalise();
				perp = mim_a1 * mim_a2;
				// Pathological case where the two bonds are exactly opposite
				if (perp.magnitude() < 0.0001)
				{
					perp.set(1,0,0);
					geometry = Atom::PlanarHydrogen;
					tempv.set(1,0,0);
				}
				else
				{
					perp.normalise();
					tempv = mim_a1 + mim_a2;
					tempv.normalise();
				}
				if (geometry != Atom::PlanarHydrogen)
					newhpos = tempv * -bondlength * cos(theta*0.5) + perp * -bondlength * sin(theta*0.5);
				else newhpos = tempv * -bondlength;
				break;
			case (3):
				// Three bonds, so work out negative vector of the average of the three bonds
				a1 = firstbond->item->partner(target);
				a2 = firstbond->next->item->partner(target);
				a3 = firstbond->next->next->item->partner(target);
				mim_a1 = cell_.mimd(a1,target);
				mim_a1.normalise();
				mim_a2 = cell_.mimd(a2,target);
				mim_a2.normalise();
				mim_a3 = cell_.mimd(a3,target);
				mim_a3.normalise();
				newhpos = mim_a1 + mim_a2 + mim_a3;
				newhpos.normalise();
				newhpos *= -bondlength;
				break;
		}
		// Now add the atom at the position specified in newhpos.
		newh = addAtom(1, newhpos + target->r());
		bondAtoms(newh,target,Bond::Single);
		projectAtom(newh);
	}
	dbgEnd(Debug::Calls,"Model::addHydrogens");
}
