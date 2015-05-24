/*
	*** Atom geometry routine
	*** src/base/atom_geometry.cpp
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

#include "base/atom.h"
#include "base/bond.h"
#include "base/sysfunc.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Determine bonding geometry
Atom::AtomGeometry Atom::geometry()
{
	Messenger::enter("Atom::geometry");
	Atom::AtomGeometry result = Atom::UnboundGeometry;
	double angle, largest;
	Bond* b1, *b2;
	Refitem<Bond,int>* bref1, *bref2;
	result = Atom::NoGeometry;
	// Separate the tests by number of bound atoms...
	switch (nBonds())
	{
		// 'Simple' cases first
		case (0):
			result = Atom::UnboundGeometry;
			break;
		case (1):
			result = Atom::OneBondGeometry;
			break;
		case (5):
			result = Atom::TrigBipyramidGeometry;
			break;
		case (6):
			result = Atom::OctahedralGeometry;
			break;
		// For the remaining types, take averages of bond angles about the atom
		case (2):
			b1 = bonds()->item;
			b2 = bonds()->next->item;
			angle = parent_->angle(b1->partner(this),this,b2->partner(this));
			if (angle> 170.0) result = Atom::LinearGeometry;
			else if ((angle > 100.0) && (angle < 115.0)) result = Atom::TetrahedralGeometry;
			break;
		case (3):
			bref1 = bonds();
			bref2 = bonds()->next;
			b1 = bref1->item;
			b2 = bref2->item;
			angle = parent_->angle(b1->partner(this),this,b2->partner(this));
			largest = angle;
			b2 = bref2->next->item;
			angle = parent_->angle(b1->partner(this),this,b2->partner(this));
			if (angle > largest) largest = angle;
			b1 = bref1->next->item;
			angle = parent_->angle(b1->partner(this),this,b2->partner(this));
			if (angle > largest) largest = angle;
			if (largest > 170.0) result = Atom::TShapeGeometry;
			else if ((largest > 115.0) && (largest < 125.0)) result = Atom::TrigPlanarGeometry;
			else if ((largest < 115.0) && (largest > 100.0)) result = Atom::TetrahedralGeometry;
			break;
		case (4):
			// Two possibilities - tetrahedral or square planar. Tetrahedral will have an
			// average of all angles of ~ 109.5, for square planar (1/6) * (4*90 + 2*180) = 120
			angle = 0.0;
			bref1 = bonds();
			while (bref1->next != NULL)
			{
				bref2 = bref1->next;
				while (bref2 != NULL)
				{
					angle += parent_->angle(bref1->item->partner(this),this,bref2->item->partner(this));
					//printf("Case 4: added an angle.\n");
					bref2 = bref2->next;
				}
				bref1 = bref1->next;
			}
			angle /= 6.0;
			if ((angle > 100.0) && (angle < 115.0)) result = Atom::TetrahedralGeometry;
			else if ((angle >= 115.0) && (angle < 125.0)) result = Atom::SquarePlanarGeometry;
			break;
	}
	Messenger::exit("Atom::geometry");
	return result;
}

// Return if the local bound geometry of the atom is planar (within a certain tolerance)
bool Atom::isPlanar(double tolerance)
{
	Messenger::enter("Atom::isPlanar");
	// Simple cases first
	if (bonds_.nItems() == 1)
	{
		Messenger::exit("Atom::isPlanar");
		return false;
	}
	if (bonds_.nItems() == 2)
	{
		Messenger::exit("Atom::isPlanar");
		return true;
	}
	// Any other case is more complex.
	bool result = true;
	Refitem<Bond,int>* ri = bonds_.first();
	// Take the first two bound atom vectors and get the cross product to define the plane's normal
	Vec3<double> v1 = parent_->cell().mimVector(this, ri->item->partner(this));
	v1.normalise();
	ri = ri->next;
	Vec3<double> v2 = parent_->cell().mimVector(this, ri->item->partner(this));
	v2.normalise();
	Vec3<double> normal = v1*v2;
	double angle;
	// Cycle over remaining bound neighbours and determine angle with plane normal
	for (ri = ri->next; ri != NULL; ri = ri->next)
	{
		// Calculate angle
		v1 = parent_->cell().mimVector(this, ri->item->partner(this));
		v1.normalise();
		angle = fabs(acos(normal.dp(v1)) * DEGRAD - 90.0);
// 		printf("Out-of-plane bond angle is %f degrees\n", angle);
		if (angle > tolerance)
		{
			result = false;
			break;
		}
	}
	Messenger::exit("Atom::isPlanar");
	return result;
}

// Calculate bond plane (unit) vector
Vec3<double> Atom::findBondPlane(Atom* other, Bond* excludedBond, const Vec3<double>& vij, bool vijIsNormalised)
{
	// Given this atom, another (j), and a bond node on 'this' between them, determine the plane of the bond if possible.
	Vec3<double> rk, xp, vijnorm;
	Refitem<Bond,int>* bref;
	Atom* origin;

	vijnorm = vij;
	if (!vijIsNormalised) vijnorm.normalise();

	// Determine which bond list and atom partner to use
	if (bonds_.nItems() > 1)
	{
		// Can define from another bond on 'this'
		bref = bonds_.first();
		origin = this;
	}
	else
	{
		// Must define from a bond on 'other'
		if (other == NULL) return vijnorm.orthogonal(true);
		bref = other->bonds_.first();
		origin = other;
		vijnorm = -vijnorm;
	}
	
	// Find suitable second bond
	for (bref = bref; bref != NULL; bref = bref->next)
	{
		if (excludedBond == bref->item) continue;
		// Found a suitable bond - is it linear?
		rk = bref->item->partner(origin)->r_ - origin->r_;
		rk.normalise();
		if (fabs(rk.dp(vijnorm)) > 0.999) continue;
		// Get cross-products to determine vector in bond *plane*
 		xp = (vijnorm * rk) * vijnorm;
		xp.normalise();
		break;
	}

	// Default, just in case
	if (bref == NULL) return vijnorm.orthogonal(true);

	return xp;
}

// Return next best vector for addition of new atom
bool Atom::nextBondVector(Vec3<double>& vector, Atom::AtomGeometry geometry)
{
	Messenger::enter("Atom::nextBondVector");

	// Were we given a valid geometry?
	switch (geometry)
	{
		case (Atom::NoGeometry):
		case (Atom::UnboundGeometry):
			Messenger::print("Unsuitable atom geometry (%s) given to Model::growAtom", Atom::atomGeometry(geometry));
			Messenger::exit("Atom::nextBondVector");
			return false;
			break;
	}
	
	// Only try to find a new bond if we have free bonds to add...
	if (this->nBonds() >= Atom::atomGeometryNBonds(geometry))
	{
		Messenger::print("Attempted to grow an atom on an existing atom which already has the correct (or greater) number of bonds (%i) for the requested geometry (%s)", this->nBonds(), Atom::atomGeometry(geometry));
		Messenger::exit("Atom::nextBondVector");
		return false;
	}

	// Now, find the next position for the required geometry
	Atom* atoms[5];
	static Vec3<double> vec[5], u, v;
	Matrix rotMat;
	static double** angleArray = NULL;
	int n, m, o, p;
	UnitCell& cell = parent_->cell();
	bool foundAngle;
	
	// Create angle array if it doesn't already exist
	if (angleArray == NULL)
	{
		angleArray = new double*[6];
		for (n=0; n<6; ++n) angleArray[n] = new double[6];
	}
	for (n=0; n<6; ++n) for (m=0; m<6; ++m) angleArray[n][m] = (n == m ? 0.0 : -1.0);

	// Set relevant basic angle before we begin...
	double theta;
	if (this->nBonds() == 0) theta = 0.0;
	else switch (geometry)
	{
		case (Atom::LinearGeometry):
			theta = 180.0 / DEGRAD;
			break;
		case (Atom::TrigPlanarGeometry):
		case (Atom::TrigBipyramidGeometry):
			theta = 120.0 / DEGRAD;
			break;
		case (Atom::TetrahedralGeometry):
			theta = 109.5 / DEGRAD;
			break;
		case (Atom::TShapeGeometry):
		case (Atom::OctahedralGeometry):
		case (Atom::SquarePlanarGeometry):
			theta = 90.0 / DEGRAD;
			break;
		default:
			printf("Horribly unknown geometry type found here.\n");
			break;
	}
	
	// Determine position of the grown atom - we generate two vectors:
	// u = new bond vector, perpendicular to 'v' but in correct plane
	// v = is a 'reference' vector, generated from existing bonds, so that u can be rotated to form the desired angle with it
	switch (this->nBonds())
	{
		// No bonds yet - just add in arbitrary position (along Y)
		case (0):
			vector.set(0.0,1.0,0.0);
			u.set(1.0,0.0,0.0);
			break;
		// Single bond - add new atom at correct angle for geometry
		case (1):
			// Get only bond vector present and create perpendicular vector
			atoms[0] = this->bonds()->item->partner(this);
			v = cell.mimVector(this, atoms[0]);
			v.normalise();
			u = v.orthogonal(true);
			vector = (v * cos(theta) + u * sin(theta));
			break;
		// Two bonds already present
		case (2):
			// For tetrahedal geometry, rotate one vector and around the axis defined by the other
			// For all other geometries, rotate one vector around the perpendicular vector
			atoms[0] = this->bonds()->item->partner(this);
			atoms[1] = this->bonds()->next->item->partner(this);
			u = cell.mimVector(this, atoms[0]);
			u.normalise();
			v = cell.mimVector(this, atoms[1]);
			v.normalise();
			// Check for pathological case where bonds are opposite each other (just select 90degree vector)
			if (fabs(u.dp(v)) > 0.99) vector = u.orthogonal(true);
			else
			{
				if (geometry == Atom::TetrahedralGeometry) rotMat.createRotationAxis(u.x, u.y, u.z, 120.0, false);
				else
				{
					u = v * u;
					u.normalise();
					rotMat.createRotationAxis(u.x, u.y, u.z, theta*DEGRAD, false);
				}
				vector = rotMat * v;
			}
			break;
		// Three bonds already present
		case (3):
			for (n=0; n<3; ++n)
			{
				atoms[n] = bonds_[n]->item->partner(this);
				vec[n] = cell.mimVector(this, atoms[n]);
				vec[n].normalise();
			}
			switch (geometry)
			{
				case (Atom::TetrahedralGeometry):
					// Pick a vector and rotate it 120 around the other
					u = cell.mimVector(this, atoms[0]);
					u.normalise();
					v = cell.mimVector(this, atoms[1]);
					v.normalise();
					rotMat.createRotationAxis(u.x, u.y, u.z, 120.0, false);
					vector = rotMat * v;
					// Check we have not overlapped with the other atom
					u = cell.mimVector(this, atoms[2]);
					u.normalise();
					if (vector.dp(u) > 0.75) vector = rotMat * vector;
					break;
				case (Atom::TrigBipyramidGeometry):
					// If we have an angle of 180deg between any two atoms, then add next atom in central plane
					foundAngle = false;
					for (n=0; n<2; ++n)
					{
						for (m=n+1; m<3; ++m) if (vec[n].dp(vec[m]) < -0.75) { foundAngle = true; break; }
						if (foundAngle) break;
					}
					if (foundAngle)
					{
						// Next vector will be the sole 'in-plane' atom rotated around one of the other bonds
						rotMat.createRotationAxis(vec[n].x, vec[n].y, vec[n].z, 120.0, false);
						o = (m+1)%3;
						vector = rotMat * vec[o != n ? 0 : (o+1)%3];
					}
					else
					{
						// All atoms appear to be in plane, so add a perpendicular atom to this plane
						vector = vec[0] * vec[1];
					}
					break;
				case (Atom::OctahedralGeometry):
				case (Atom::SquarePlanarGeometry):
					// With three bonds, there is at least one atom not involved in a 180degree angle...
					for (n=0; n<2; ++n) for (m=n+1; m<3; ++m) if (vec[n].dp(vec[m]) < -0.75)
					{
						angleArray[0][n] = 1.0;
						angleArray[0][m] = 1.0;
					}
					// Search for element in array which is still negative (indicating no 180degree angle)
					for (n=0; n<3; ++n)
					{
						if (angleArray[0][n] < 0.0)
						{
							vector = -vec[n];
							break;
						}
					}
					break;
			}
			break;
		// Four bonds already present
		case (4):
			for (n=0; n<4; ++n)
			{
				atoms[n] = bonds_[n]->item->partner(this);
				vec[n] = cell.mimVector(this, atoms[n]);
				vec[n].normalise();
			}
			if (geometry == Atom::TrigBipyramidGeometry)
			{
				// Same as before: if we have an angle of 180deg between any two atoms, then add final atom in central plane
				// Calculate all angles first
				for (n=0; n<4; ++n) for (m=0; m<4; ++m) if (n != m) angleArray[n][m] = vec[n].dp(vec[m]);
				foundAngle = false;
				for (n=0; n<3; ++n)
				{
					for (m=n+1; m<4; ++m)
					{
						if (angleArray[n][m] < -0.75)
						{
							foundAngle = true;
							// Find the other two atoms which aren't in the 180degree bond
							o = n;
							do { o = (o+1)%4; } while (o == m);
							p = o;
							do { p = (p+1)%4; } while (p == m);
							rotMat.createRotationAxis(vec[n].x, vec[n].y, vec[n].z, 120.0, false);
							vector = rotMat * vec[o];
							// Check we have not overlapped with the other atom
							if (vector.dp(vec[p]) > 0.75) vector = rotMat * vector;
						}
					}
					if (foundAngle) break;
				}
				if (!foundAngle)
				{
					// No sign of 180degree angle, so find bond which makes smallest average angle with the rest...
					for (n=0; n<4; ++n) for (m=0; m<4; ++m) if (m != n) angleArray[n][n] += angleArray[n][m];
					for (n=0; n<4; ++n) angleArray[n][n] = fabs(angleArray[n][n] / 3.0);
					o = 0;
					for (n=1; n<4; ++n) if (angleArray[n][n] < angleArray[o][o]) o = n;
					vector = -vec[o];
				}
			}
			else if (geometry == Atom::OctahedralGeometry)
			{
				// TODO
			}
			break;
		// Five bonds already present
		case (5):
			// Only octahedral geometry relevant at this point
			// TODO
			break;
	}

	// Normalise vector
	vector.normalise();
// 	printf("Final vector is "); vector.print();

	Messenger::exit("Atom::nextBondVector");
	return true;
}

// Return whether specified atoms form a (bound) angle
bool Atom::formAngle(Atom* atoms[3], Atom* orderedAtoms[3])
{
	// Reset ordered atoms array
	orderedAtoms[0] = NULL;
	orderedAtoms[1] = NULL;
	orderedAtoms[2] = NULL;

	int j = -1;
	if (atoms[0]->findBond(atoms[1]))
	{
		if (atoms[1]->findBond(atoms[2])) j = 1;
		else if (atoms[0]->findBond(atoms[2])) j = 0;
	}
	else if (atoms[1]->findBond(atoms[2]) && atoms[2]->findBond(atoms[0])) j = 2;

	if (j == -1) return false;

	// Set atoms in order
	orderedAtoms[1] = atoms[j];
	orderedAtoms[0] = atoms[(j+1)%3];
	orderedAtoms[2] = atoms[(j+2)%3];

	return true;
}

// Return whether specified atoms form a (bound) torsion
bool Atom::formTorsion(Atom* atoms[4], Atom* orderedAtoms[4])
{
	// Reset ordered atoms array
	orderedAtoms[0] = NULL;
	orderedAtoms[1] = NULL;
	orderedAtoms[2] = NULL;
	orderedAtoms[3] = NULL;

	// Loop over all pairs of atoms to see if we can find a bond
	int i, j, k, l;
	for (j=0; j<3; ++j)
	{
		orderedAtoms[1] = atoms[j];
		for (k = j+1; k<4; ++k)
		{
			orderedAtoms[2] = atoms[k];

			// Is there a bond between atoms j and k?
			if (!orderedAtoms[1]->findBond(orderedAtoms[2])) continue;

			// We have a bond between j and k, so see if we have the necessary other bonds
			for (int n=0; n<4; ++n) if ((n != j) && (n != k)) { i = n; break; }
			for (int n=0; n<4; ++n) if ((n != j) && (n != k) && (n != i) ) { l = n; break; }
			if (orderedAtoms[1]->findBond(atoms[i]) && orderedAtoms[2]->findBond(atoms[l]))
			{
				orderedAtoms[0] = atoms[i];
				orderedAtoms[3] = atoms[l];
				return true;
			}
			else if (orderedAtoms[1]->findBond(atoms[l]) && orderedAtoms[2]->findBond(atoms[i]))
			{
				orderedAtoms[0] = atoms[l];
				orderedAtoms[3] = atoms[i];
				return true;
			}
		}
	}
	return false;
}
