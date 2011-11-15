/*
	*** Model build functions
	*** src/model/build.cpp
	Copyright T. Youngs 2007-2011

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
#include "classes/prefs.h"
#include "base/elements.h"

// Add hydrogens to model
void Model::hydrogenSatisfy(Atom *target)
{
	// Cycles over atoms in model (or only the atom supplied), and works out how many hydrogens (and in which geometry) should be added to each
	msg.enter("Model::hydrogenSatisfy");
	int numh, tbo, nsingle, ndouble, valency, n;
	Atom *i, *endatom;
	double bondLength = prefs.hydrogenDistance();
	i = (target == NULL ? atoms_.first() : target);
	endatom = (target == NULL ? NULL : target->next);
	for (i = i; i != endatom; i = i->next)
	{
		// Step 1 - Work out how many single-bonds (i.e. hydrogens) we need to add to satisfy the atom's valency
		switch (i->element())
		{
			// Single hydrogens - H, F, Cl, Br, I
			case (1):
			case (9):
			case (17):
			case (35):
			case (53):
				valency = 2;
				break;
			// Two hydrogens - O
			case (8):
				valency = 4;
				break;
			// Three hydrogens - N
			case (7):
				valency = 6;
				break;
			// Four hydrogens - C, Si
			case (6):
			case (14):
				valency = 8;
				break;
			default:
				valency = 0;
				break;
		}
		// Calculate total bond order of atom and work out single bond deficit
		tbo = i->totalBondOrder();
		numh = (valency - tbo) / 2;
		// Step 2 - Work out geometry that we'll add hydrogens in, based on the atom's valency
		nsingle = countBondsToAtom(i,Bond::Single);
		ndouble = countBondsToAtom(i,Bond::Double);

		// Simplest cases - atom has no bonds or all single bonds - we add in a tetrahedral geometry
		// Otherwise, must work out the correct geometry to add hydrogens in...
		if (i->nBonds() == 0 || i->nBonds() == nsingle) for (n=0; n<numh; ++n) growAtom(i,1,bondLength, Atom::TetrahedralGeometry);
		else if (ndouble != 0) for (n=0; n<numh; ++n) growAtom(i,1,bondLength, Atom::TrigPlanarGeometry);
		else for (n=0; n<numh; ++n) growAtom(i,1, bondLength, Atom::LinearGeometry);
	}
	msg.exit("Model::hydrogenSatisfy");
}

// Add a single atom of the type specified to the atom specified
Atom *Model::growAtom(Atom *i, int element, double distance, Atom::AtomGeometry geometry, bool bound)
{
	msg.enter("Model::growAtom");

	// Were we given a valid geometry?
	switch (geometry)
	{
		case (Atom::SquarePlanarGeometry):
		case (Atom::TShapeGeometry):
		case (Atom::TrigBipyramidGeometry):
		case (Atom::OctahedralGeometry):
		case (Atom::NoGeometry):
		case (Atom::UnboundGeometry):
			msg.print("Unsuitable/unsopported atom geometry (%s) given to Model::growAtom\n", Atom::atomGeometry(geometry));
			msg.exit("Model::growAtom");
			return NULL;
			break;
	}
	
	// Now, find the next position for the required geometry
	Atom *j, *k, *newAtom = NULL;
	Vec3<double> r = i->r(), u, v;
	double theta;
	switch (i->nBonds())
	{
		// No bonds yet - just add in arbitrary position (along Z)
		case (0):
			r.z += distance;
			newAtom = addAtom(element, r);
			break;
		// Single bond - add new atom at correct angle for geometry
		case (1):
			// Get only bond vector present and create perpendicular vector
			j = i->bonds()->item->partner(target);
			u = cell_.mimd(i, j);
			u.normalise();
			r = u.orthogonal() * distance;
			newAtom = addAtom(element, r);
			if (geometry == Atom::LinearGeometry) setAtomicAngle(j, i, newAtom, 180.0);
			else if (geometry == Atom::TrigPlanarGeometry) setAtomicAngle(j, i, newAtom, 120.0);
			else if (geometry == Atom::TetrahedralGeometry) setAtomicAngle(j, i, newAtom, 109.5);
			break;
		// XXX
	}
	switch (geometry)
	{
		case (Atom::OneBondGeometry):
			if (i->nBonds() == 0)
			{
				r.z += distance;
				j = addAtom(element, r);
			}
		case (Atom::LinearGeometry):
			theta = 180.0 / DEGRAD;
			break;
		case (Atom::TrigPlanarGeometry):
			theta = 120.0 / DEGRAD;
			break;
		case (Atom::TetrahedralGeometry):
			theta = 109.5 / DEGRAD;
			break;
	}
	
	// Bond atoms if requested
	if (bound && j) bondAtoms(i, j);

	msg.exit("Model::growAtom");
	return j;
}

// // Iteratively add hydrogens to specified atom (giving supplied geometry)
// void Model::addHydrogens(Atom *target, int nhydrogen, Atom::HAddGeom geometry)
// {
// 	// Iteratively add hydrogens to the molecule conforming to the desired geometry specified
// 	msg.enter("Model::addHydrogens");
// 	Atom *a1, *a2, *a3;
// 	Atom *newh;
// 	Vec3<double> mim_a1, mim_a2, mim_a3, perp, perp2, newhpos, tempv;
// 	double bondlength = prefs.hydrogenDistance(), theta = 0.0;
// 	int minel, onebelow, oneabove;
// 	Refitem<Bond,int> *firstbond;
// 	// Add new hydrogens based on the geometry type, and then work out from what bonds there are already...
// 	switch (geometry)
// 	{
// 	}
// 	// Switches put new coordinates in 'newhpos' - an atom is created an placed here at the end
// 	for (int n=0; n<nhydrogen; n++)
// 	{
// 		firstbond = target->bonds();
// 		switch (target->nBonds())
// 		{
// 
// 			case (1):
// 				// Only one bond, so add atom at arbitrary position with angle of required geometry
// 				a1 = firstbond->item->partner(target);
// 				mim_a1 = cell_.mimd(target,a1);
// 				mim_a1.normalise();
// 				// Create perpendicular vector to X-i...
// 				minel = mim_a1.absMinElement();
// 				onebelow = (minel+2)%3;
// 				oneabove = (minel+1)%3;
// 				perp.set(minel,0.0);
// 				perp.set(onebelow,mim_a1.get(oneabove));
// 				perp.set(oneabove,-mim_a1.get(onebelow));
// 				perp.normalise();
// 				newhpos = mim_a1 * -bondlength * cos(theta) + perp * bondlength * sin(theta);
// 				break;
// 			case (2):
// 				// Two bonds, so work out 'pointing' vector and adjust to desired angle (if !Atom::PlanarHydrogen)
// 				// Get mim coordinates of the two bound atoms
// 				a1 = firstbond->item->partner(target);
// 				a2 = firstbond->next->item->partner(target);
// 				mim_a1 = cell_.mimd(a1,target);
// 				mim_a1.normalise();
// 				mim_a2 = cell_.mimd(a2,target);
// 				mim_a2.normalise();
// 				perp = mim_a1 * mim_a2;
// 				// Pathological case where the two bonds are exactly opposite
// 				if (perp.magnitude() < 0.0001)
// 				{
// 					perp.set(1,0,0);
// 					geometry = Atom::PlanarHydrogen;
// 					tempv.set(1,0,0);
// 				}
// 				else
// 				{
// 					perp.normalise();
// 					tempv = mim_a1 + mim_a2;
// 					tempv.normalise();
// 				}
// 				if (geometry != Atom::PlanarHydrogen)
// 					newhpos = tempv * -bondlength * cos(theta*0.5) + perp * -bondlength * sin(theta*0.5);
// 				else newhpos = tempv * -bondlength;
// 				break;
// 			case (3):
// 				// Three bonds, so work out negative vector of the average of the three bonds
// 				a1 = firstbond->item->partner(target);
// 				a2 = firstbond->next->item->partner(target);
// 				a3 = firstbond->next->next->item->partner(target);
// 				mim_a1 = cell_.mimd(a1,target);
// 				mim_a1.normalise();
// 				mim_a2 = cell_.mimd(a2,target);
// 				mim_a2.normalise();
// 				mim_a3 = cell_.mimd(a3,target);
// 				mim_a3.normalise();
// 				newhpos = mim_a1 + mim_a2 + mim_a3;
// 				newhpos.normalise();
// 				newhpos *= -bondlength;
// 				break;
// 		}
// 		// Now add the atom at the position specified in newhpos.
// 		newh = addAtom(1, newhpos + target->r());
// 		bondAtoms(newh,target,Bond::Single);
// 	}
// 	msg.exit("Model::addHydrogens");
// }

// Return the pen orientation matrix
Matrix Model::penOrientation() const
{
	return penOrientation_;
}

// Rotate the pen orientation matrix about the specified axis
void Model::rotatePenAxis(int axis, double degrees)
{
	Matrix rotmat;
	double theta = degrees / DEGRAD;
	Vec3<double> v = penOrientation_.columnAsVec3(axis);
	// Define quaternion components
	Vec4<double> q;
	q.set(v.x * sin(theta/2.0), v.y*sin(theta/2.0), v.z*sin(theta/2.0), cos(theta/2.0));
	q.normalise();
	// Create rotation matrix from quaternion
	rotmat.setColumn(0, 1.0 - 2.0*q.y*q.y - 2.0*q.z*q.z, 2.0*q.x*q.y + 2.0*q.w*q.z, 2.0*q.x*q.z - 2.0*q.w*q.y, 0.0);
	rotmat.setColumn(1, 2.0*q.x*q.y - 2.0*q.w*q.z, 1.0 - 2.0*q.x*q.x - 2.0*q.z*q.z, 2.0*q.y*q.z + 2.0*q.w*q.x, 0.0);
	rotmat.setColumn(2, 2.0*q.x*q.z + 2.0*q.w*q.y, 2.0*q.y*q.z - 2.0*q.w*q.x, 1.0 - 2.0*q.x*q.x - 2.0*q.y*q.y, 0.0);
	penOrientation_ *= rotmat;
}

// Reset pen axis system
void Model::resetPenOrientation()
{
	penOrientation_.setIdentity();
}

// Return the current pen position
Vec3<double> Model::penPosition() const
{
	return penPosition_;
}

// Move the pen position in its axis system
void Model::movePenPosition(Vec3<double> v)
{
	penPosition_ += penOrientation_ * v;
}

// Set the pen position absolutely
void Model::setPenPosition(Vec3<double> v)
{
	penPosition_ = v;
}

// Set distance between atoms, moving atom j
void Model::setAtomicDistance(Atom *i, Atom *j, double newdistance)
{
	Vec3<double> v = cell_.mimd(j, i);
	double delta = newdistance - v.magnitude();
	v.normalise();
	v *= delta;
	translateAtom(j, v);
}

// Set angle between atoms, moving atom k
void Model::setAtomicAngle(Atom *i, Atom *j, Atom *k, double newangle)
{
	Matrix r, u, ut, gr, Igr;
	double ang = angle(k,j,i);

	// Get cross product of bond vectors to define rotation axis
	Vec3<double> v = cell_.mimd(j,k) * cell_.mimd(j,i);
	v.normalise();
	double delta = newangle - ang;
	
	u.setColumn(0, v, 0.0);
	u.setColumn(1, v.orthogonal(), 0.0);
	u.setColumn(2, v * u.columnAsVec3(1), 0.0);
	u.columnNormalise(2);
	
	ut = u.transpose();

	// Create rotation matrix
	r.createRotationX(delta);

	// Create grand rotation matrix
	gr = ut * r * u;
	Igr.setIdentity();
	Igr = Igr - gr;

	Vec3<double> tempv = gr.transform(k->r()) + Igr.transform(j->r());
	positionAtom(k, tempv);
}

// Set torsion between atoms, moving atom l
void Model::setAtomicTorsion(Atom *i, Atom *j, Atom *k, Atom *l, double newtorsion)
{
	Matrix r, u, ut, gr, Igr;
	double ang = torsion(l,k,j,i);
	// Rotation vector will be vector j->k
	Vec3<double> v = cell_.mimd(j,k);
	v.normalise();
	double delta = newtorsion - ang;
	
	u.setColumn(0, v, 0.0);
	u.setColumn(1, v.orthogonal(), 0.0);
	u.setColumn(2, v * u.columnAsVec3(1), 0.0);
	u.columnNormalise(2);
	
	ut = u.transpose();

	// Create rotation matrix
	r.createRotationX(delta);

	// Create grand rotation matrix
	gr = ut * r * u;
	Igr.setIdentity();
	Igr = Igr - gr;

	Vec3<double> tempv = gr.transform(l->r()) + Igr.transform(k->r());
	positionAtom(l, tempv);
}
