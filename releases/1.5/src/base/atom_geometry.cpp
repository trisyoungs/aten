/*
	*** Atom geometry routine
	*** src/base/atom_geometry.cpp
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

#include "base/atom.h"
#include "base/bond.h"
#include "base/sysfunc.h"
#include "model/model.h"
#include "base/elements.h"

// Return suitable vector based on one supplied bond
Vec3<double> Atom::oneBondVector(Bond *b, double angle)
{
	Atom *j = b->partner(this);
	Vec3<double> mim_a1 = parent_->cell()->mimd(this,j);
	mim_a1.normalise();
	// Create perpendicular vector to X-i...
	int minel = mim_a1.absMinElement();
	int onebelow = (minel+2)%3;
	int oneabove = (minel+1)%3;
	Vec3<double> perp;
	perp.set(minel,0.0);
	perp.set(onebelow,mim_a1.get(oneabove));
	perp.set(oneabove,-mim_a1.get(onebelow));
	perp.normalise();
	// Final orientation depends on supplied angle
	double theta = angle / DEGRAD;
	return (-mim_a1 * cos(theta) + perp * sin(theta));
}

// Return suitable vector based on two supplied bonds
Vec3<double> Atom::twoBondVector(Bond *b1, Bond *b2, double angle)
{
	// Get mim coordinates of the two bound atoms
	Atom *a1 = b1->partner(this);
	Atom *a2 = b2->partner(this);
	Vec3<double> mim_a1, mim_a2, perp, tempv;
	mim_a1 = parent_->cell()->mimd(a1,this);
	mim_a1.normalise();
	mim_a2 = parent_->cell()->mimd(a2,this);
	mim_a2.normalise();
	perp = mim_a1 * mim_a2;
	// Pathological case where the two bonds are exactly opposite
	if (perp.magnitude() < 0.0001) return oneBondVector(b1, 90.0);
	// Otherwise, we're ok
	perp.normalise();
	tempv = mim_a1 + mim_a2;
	tempv.normalise();
	// Final orientation depends on supplied angle
	double theta = angle / DEGRAD;
	return (tempv * -cos(theta*0.5) + perp * -sin(theta*0.5));
}

// Return suitable vector based on three supplied bonds
Vec3<double> Atom::threeBondVector(Bond *b1, Bond *b2, Bond *b3, double angle, Atom::AtomGeometry expgeom)
{
	// Work out the three angles between the bonds
	Vec3<double> result, v1;
	Atom *a1 = b1->partner(this);
	Atom *a2 = b2->partner(this);
	Atom *a3 = b3->partner(this);
	double angle1 = parent_->cell()->angle(a1,this,a3);
	double angle2 = parent_->cell()->angle(a2,this,a3);
	double angle3 = parent_->cell()->angle(a1,this,a2);
	double avg = (angle1+angle2+angle3) / 3.0;
	if (fabs(avg-90.0) < 10.0)
	{
		// Three bonds all at near-right angles
		// Resulting vector can be opposite of any bond vector
		result = oneBondVector(b1, 180.0);
	}
	else if (fabs(avg-120.0) < 10.0)
	{
		// T-shape geometry, more or less
		// Resulting vector will be XP of two bonds forming a 90deg angle
		if (angle1 < 120.0) result = parent_->cell()->mimd(a1,this) * parent_->cell()->mimd(a3,this);
		else result = parent_->cell()->mimd(a2,this) * parent_->cell()->mimd(a3,this);
		result.normalise();
	}
	else
	{
		result = parent_->cell()->mimd(a1,this);
		result.normalise();
		v1 = parent_->cell()->mimd(a2,this);
		v1.normalise();
		result += v1;
		v1 = parent_->cell()->mimd(a3,this);
		v1.normalise();
		result += v1;
		result *= -1.0;
		result.normalise();
	}
	return result;
}

// Return next bext vector for addition of new atom
Vec3<double> Atom::nextBondVector()
{
	msg.enter("Atom::nextBondVector");
	Vec3<double> vector;
	int nsingle = parent_->countBondsToAtom(this,Bond::Single);
	int ndouble = parent_->countBondsToAtom(this,Bond::Double);
	int ntriple = parent_->countBondsToAtom(this,Bond::Triple);
	switch (el_)
	{
		case (0):
			break;
		// Hydrogen
		case (1):
			if (nBonds() == 0) vector.set(1.0,0.0,0.0);
			break;
		// Carbon
		case (6):
			switch (nBonds())
			{
				case (0):
					vector.set(1.0,0.0,0.0);
					break;
				case (1):
					if (ntriple == 1) vector = oneBondVector(bonds()->item, 180.0);
					else if (ndouble == 1) vector = oneBondVector(bonds()->item, 120.0);
					else vector = oneBondVector(bonds()->item, 109.5);
					break;
				case (2):
					if (ndouble == 1) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 120.0);
					else if (nsingle == 2) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 109.5);
					break;
				case (3):
					if (nsingle == 3) vector = threeBondVector(bonds_[0]->item, bonds_[1]->item, bonds_[2]->item, 109.5, Atom::TetrahedralGeometry);
			}
			break;
		// Nitrogen
		case (7):
			switch (nBonds())
			{
				case (0):
					vector.set(1.0,0.0,0.0);
					break;
				case (1):
					if (ndouble == 1) vector = oneBondVector(bonds()->item, 120.0);
					else vector = oneBondVector(bonds()->item, 109.5);
					break;
				case (2):
					if (ndouble == 1) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 120.0);
					else if (nsingle == 2) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 109.5);
					break;
				case (3):
					// Still allow one more bond, even though it would give a charged nitrogen
					if (nsingle == 3) vector = threeBondVector(bonds_[0]->item, bonds_[1]->item, bonds_[2]->item, 109.5, Atom::TetrahedralGeometry);
					break;
			}
			break;
		// Oxygen
		case (8):
			switch (nBonds())
			{
				case (0):
					vector.set(1.0,0.0,0.0);
					break;
				case (1):
					if (ndouble == 1) vector = oneBondVector(bonds()->item, 120.0);
					else vector = oneBondVector(bonds()->item, 109.5);
					break;
				case (2):
					if (ndouble == 1) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 120.0);
					else if (nsingle == 2) vector = twoBondVector(bonds_[0]->item, bonds_[1]->item, 109.5);
					break;
			}
			break;
		default:
			vector.set(1.0,0.0,0.0);
			break;
	}
	msg.exit("Atom::nextBondVector");
	return vector;
}
