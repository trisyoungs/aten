/*
	*** Atomic bond
	*** src/base/bond.cpp
	Copyright T. Youngs 2007-2018

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

#include "base/elementmap.h"
#include "base/bond.h"
#include "base/atom.h"
#include "base/sysfunc.h"

ATEN_USING_NAMESPACE

// BondType enum
const char* BondTypeKeywords[Bond::nBondTypes] = { "_ANY_", "single", "double", "triple", "aromatic" };
Bond::BondType Bond::bondType(QString s, bool reportError)
{
	Bond::BondType bt = (Bond::BondType) enumSearch("bond type",Bond::nBondTypes,BondTypeKeywords,s,reportError);
	if ((bt == Bond::nBondTypes) && reportError) enumPrintValid(Bond::nBondTypes,BondTypeKeywords);
	return bt;
}
const char* Bond::bondType(Bond::BondType bt)
{
	return BondTypeKeywords[bt];
}
Bond::BondType Bond::bondType(double order)
{
	static double tolerance = 0.1;
	if ((order - Bond::Single) < tolerance) return Bond::Single;
	else if ((order - Bond::Double) < tolerance) return Bond::Double;
	else if ((order - Bond::Triple) < tolerance) return Bond::Triple;
	else if ((order - Bond::Aromatic) < tolerance) return Bond::Aromatic;
	else 
	{
		Messenger::print("Order %f doesn't readily convert to a BondType - assuming Single.", order);
		return Bond::Single;
	}
}
Bond::BondType Bond::increase(BondType btype)
{
	if (btype == Bond::Single) return Bond::Double;
	else if (btype == Bond::Double) return Bond::Triple;
	return Bond::Single;
}
Bond::BondType Bond::decrease(BondType btype)
{
	if (btype == Bond::Single) return Bond::Triple;
	else if (btype == Bond::Double) return Bond::Single;
	return Bond::Double;
}

// Constructor
Bond::Bond() : ListItem<Bond>()
{
	// Private variables
	type_ = Bond::Any;
	atomI_ = NULL;
	atomJ_ = NULL;
}

// Set atoms for bond
void Bond::setAtoms(Atom* i, Atom* j)
{
	atomI_ = i;
	atomJ_ = j;
}

// Set atom I for bond
void Bond::setAtomI(Atom* i)
{
	atomI_ = i;
}

// Set atom I for bond
void Bond::setAtomJ(Atom* j)
{
	atomJ_ = j;
}

// Set bond type
void Bond::setType(Bond::BondType bt)
{
	type_ = bt;
}

// Return order of bond
Bond::BondType Bond::type() const
{
	return type_;
}

// Return real-valued bond order (static function)
double Bond::order(Bond::BondType bt)
{
	switch (bt)
	{
		case (Bond::Any):
			return 0.0;
		case (Bond::Single):
			return 1.0;
		case (Bond::Double):
			return 2.0;
		case (Bond::Triple):
			return 3.0;
		case (Bond::Aromatic):
			return 1.5;
		default:
			return 0.0;
	}
	return 0.0;
}

// Return real-valued bond order
double Bond::order() const
{
	return order(type_);
}

// Return first atom in bond
Atom* Bond::atomI()
{
	return atomI_;
}

// Return second atom in bond
Atom* Bond::atomJ()
{
	return atomJ_;
}

// Returns the partner of the specified atom in the bond structure
Atom* Bond::partner(Atom* i)
{
	return (i == atomI_ ? atomJ_ : atomI_);
}

// Return bond type that best satisfies the valencies of the atoms involved
Bond::BondType Bond::augmented() const
{
	Messenger::enter("Bond::augmented");
	// If the bond type is aromatic, we never change it here since it has been set from another source
	if (type_ == Bond::Aromatic)
	{
		Messenger::exit("Bond::augmented");
		return type_;
	}
	int lowest, n, tbi, tbj, penalty[4], eli, elj;
	// Get total bond orders of atoms. We divide by two, but should never be missing a half somewhere since aromatic bonds (BO=1.5 (*2 = 3)) will always come in pairs
	tbi = atomI_->totalBondOrder() / 2;
	tbj = atomJ_->totalBondOrder() / 2;
	eli = atomI_->element();
	elj = atomJ_->element();
	// Get current penalty associated with the two bound elements - if its zero, there is nothing more to be done
	penalty[0] = ElementMap::bondOrderPenalty(eli, tbi) + ElementMap::bondOrderPenalty(elj, tbj);
//	printf("Total bond orders of IDs %i (%s) and %i (%s) are %i and %i - connection is %ibond with penalty %i\n", atomI_->id()+1, ElementMap::symbol(atomI_), atomJ_->id()+1, ElementMap::symbol(atomJ_), tbi, tbj, type_, penalty[0]);
	if (penalty[0] == 0)
	{
		Messenger::exit("Bond::augmented");
		return type_;
	}
	// Get penalties associated with single, double, and triple bonds
	for (n=1; n<4; n++) penalty[n] = ElementMap::bondOrderPenalty(eli, tbi - type_ + n) + ElementMap::bondOrderPenalty(elj, tbj - type_ + n);
//	for (n=1; n<4; n++) printf(" -- Conversion to %ibond has penalty %i - %i (%s) + %i (%s)\n", n, penalty[n], ElementMap::bondOrderPenalty(eli, tbi - type_ + n), ElementMap::symbol(atomI_), ElementMap::bondOrderPenalty(elj, tbj - type_ + n), ElementMap::symbol(atomJ_));
	// Find lowest penalty value
	lowest = 0;
	for (n=1; n<4; n++) if (penalty[n] < penalty[lowest]) lowest = n;
//	if (lowest == 0) printf(" -- Bond type will not be changed\n");
//	else printf(" -- Connection will be changed to %ibond\n", lowest);
	Messenger::exit("Bond::augmented");
	return (lowest == 0 ? type_ : Bond::bondType(lowest));
}
