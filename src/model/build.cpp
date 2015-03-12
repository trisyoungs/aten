/*
	*** Model build functions
	*** src/model/build.cpp
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

ATEN_USING_NAMESPACE

// Add hydrogens to model
void Model::hydrogenSatisfy(Atom* target)
{
	// Cycles over atoms in model (or only the atom supplied), and works out how many hydrogens (and in which geometry) should be added to each
	Messenger::enter("Model::hydrogenSatisfy");
	int numh, tbo, nsingle, ndouble, valency, n;
	Atom* i, *endatom;
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
	Messenger::exit("Model::hydrogenSatisfy");
}

// Add a single atom of the type specified to the atom specified
Atom* Model::growAtom(Atom* i, short int element, double distance, Atom::AtomGeometry geometry, bool bound)
{
	Messenger::enter("Model::growAtom");

	// Attempt to find suitable bond vector
	Vec3<double> newVec;
	if (!i->nextBondVector(newVec, geometry))
	{
		Messenger::print("Failed to find suitable vector for new atom.");
		return NULL;
	}
	
	// Check distance - if negative, work out new distance based on radii of elements involved
	if (distance < 0.0) distance = Elements().atomicRadius(i) + Elements().atomicRadius(element);
	newVec *= distance;
	Atom* newAtom = addAtom(element, i->r() + newVec);

	// Bond atoms if requested
	if (bound) bondAtoms(i, newAtom, Bond::Single);

	Messenger::exit("Model::growAtom");
	return newAtom;
}

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
void Model::setAtomicDistance(Atom* i, Atom* j, double newdistance)
{
	Vec3<double> vij = cell_.mimVector(i, j);
	double delta = newdistance - vij.magnitude();
	vij.normalise();
	vij *= delta;
	translateAtom(j, vij);
}

// Set angle between atoms, moving atom k
void Model::setAtomicAngle(Atom* i, Atom* j, Atom* k, double newangle)
{
	Matrix r, u, ut, gr, Igr;
	double ang = angle(i,j,k);

	// Get cross product of bond vectors to define rotation axis
	Vec3<double> v = cell_.mimVector(j,k) * cell_.mimVector(j,i);
	v.normalise();
	double delta = newangle - ang;
	
	u.setColumn(0, v, 0.0);
	u.setColumn(1, v.orthogonal(TRUE), 0.0);
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
void Model::setAtomicTorsion(Atom* i, Atom* j, Atom* k, Atom* l, double newtorsion)
{
	Matrix r, u, ut, gr, Igr;
	double ang = torsion(l,k,j,i);
	// Rotation vector will be vector j->k
	Vec3<double> v = cell_.mimVector(j,k);
	v.normalise();
	double delta = newtorsion - ang;
	
	u.setColumn(0, v, 0.0);
	u.setColumn(1, v.orthogonal(TRUE), 0.0);
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
