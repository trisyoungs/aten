/*
	*** Atom geometry routine
	*** src/base/atom_geometry.cpp
	Copyright T. Youngs 2007-2012

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

// Return next best vector for addition of new atom
bool Atom::nextBondVector(Vec3<double> &vector, Atom::AtomGeometry geometry)
{
	msg.enter("Atom::nextBondVector");

	// Were we given a valid geometry?
	switch (geometry)
	{
		case (Atom::NoGeometry):
		case (Atom::UnboundGeometry):
			msg.print("Unsuitable atom geometry (%s) given to Model::growAtom\n", Atom::atomGeometry(geometry));
			msg.exit("Atom::nextBondVector");
			return FALSE;
			break;
	}
	
	// Only try to find a new bond if we have free bonds to add...
	if (this->nBonds() >= Atom::atomGeometryNBonds(geometry))
	{
		msg.print("Attempted to grow an atom on an existing atom which already has the correct (or greater) number of bonds (%i) for the requested geometry (%s)\n", this->nBonds(), Atom::atomGeometry(geometry));
		msg.exit("Atom::nextBondVector");
		return FALSE;
	}

	// Now, find the next position for the required geometry
	Atom *atoms[5];
	static Vec3<double> vec[5], u, v;
	Matrix rotMat;
	static double **angleArray = NULL;
	int n, m, o, p;
	UnitCell *cell = parent_->cell();
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
			v = cell->mimVector(this, atoms[0]);
			v.normalise();
			u = v.orthogonal(TRUE);
			vector = (v * cos(theta) + u * sin(theta));
			break;
		// Two bonds already present
		case (2):
			// For tetrahedal geometry, rotate one vector and around the axis defined by the other
			// For all other geometries, rotate one vector around the perpendicular vector
			atoms[0] = this->bonds()->item->partner(this);
			atoms[1] = this->bonds()->next->item->partner(this);
			u = cell->mimVector(this, atoms[0]);
			u.normalise();
			v = cell->mimVector(this, atoms[1]);
			v.normalise();
			// Check for pathological case where bonds are opposite each other (just select 90degree vector
			if (fabs(u.dp(v)) > 0.99) vector = u.orthogonal(TRUE);
			else
			{
				if (geometry == Atom::TetrahedralGeometry) rotMat.createRotationAxis(u.x, u.y, u.z, 120.0, FALSE);
				else
				{
					u = v * u;
					u.normalise();
					rotMat.createRotationAxis(u.x, u.y, u.z, theta*DEGRAD, FALSE);
				}
				vector = rotMat * v;
			}
			break;
		// Three bonds already present
		case (3):
			for (n=0; n<3; ++n)
			{
				atoms[n] = bonds_[n]->item->partner(this);
				vec[n] = cell->mimVector(this, atoms[n]);
				vec[n].normalise();
			}
			switch (geometry)
			{
				case (Atom::TetrahedralGeometry):
					// Pick a vector and rotate it 120 around the other
					u = cell->mimVector(this, atoms[0]);
					u.normalise();
					v = cell->mimVector(this, atoms[1]);
					v.normalise();
					rotMat.createRotationAxis(u.x, u.y, u.z, 120.0, FALSE);
					vector = rotMat * v;
					// Check we have not overlapped with the other atom
					u = cell->mimVector(this, atoms[2]);
					u.normalise();
					if (vector.dp(u) > 0.75) vector = rotMat * vector;
					break;
				case (Atom::TrigBipyramidGeometry):
					// If we have an angle of 180deg between any two atoms, then add next atom in central plane
					foundAngle = FALSE;
					for (n=0; n<2; ++n)
					{
						for (m=n+1; m<3; ++m) if (vec[n].dp(vec[m]) < -0.75) { foundAngle = TRUE; break; }
						if (foundAngle) break;
					}
					if (foundAngle)
					{
						// Next vector will be the sole 'in-plane' atom rotated around one of the other bonds
						rotMat.createRotationAxis(vec[n].x, vec[n].y, vec[n].z, 120.0, FALSE);
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
				vec[n] = cell->mimVector(this, atoms[n]);
				vec[n].normalise();
			}
			if (geometry == Atom::TrigBipyramidGeometry)
			{
				// Same as before: if we have an angle of 180deg between any two atoms, then add final atom in central plane
				// Calculate all angles first
				for (n=0; n<4; ++n) for (m=0; m<4; ++m) if (n != m) angleArray[n][m] = vec[n].dp(vec[m]);
				foundAngle = FALSE;
				for (n=0; n<3; ++n)
				{
					for (m=n+1; m<4; ++m)
					{
						if (angleArray[n][m] < -0.75)
						{
							foundAngle = TRUE;
							// Find the other two atoms which aren't in the 180degree bond
							o = n;
							do { o = (o+1)%4; } while (o == m);
							p = o;
							do { p = (p+1)%4; } while (p == m);
							rotMat.createRotationAxis(vec[n].x, vec[n].y, vec[n].z, 120.0, FALSE);
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

	msg.exit("Atom::nextBondVector");
	return TRUE;
}
