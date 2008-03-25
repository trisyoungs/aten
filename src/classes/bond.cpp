/*
	*** Atomic bond
	*** src/classes/bond.cpp
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

#include "base/elements.h"
#include "classes/bond.h"
#include "classes/ring.h"
#include <math.h>

// BondType enum
const char *BondTypeKeywords[Bond::nItems] = { "none", "single", "double", "triple" };
Bond::BondType Bond::bondType(const char *s)
{
	return (Bond::BondType) enumSearch("bond type",Bond::nItems,BondTypeKeywords,s);
}
const char *Bond::bondTypeKeyword(Bond::BondType bt)
{
	return BondTypeKeywords[bt];
}
Bond::BondType Bond::increaseBondType(BondType btype)
{
	if (btype == Bond::Single) return Bond::Double;
	else if (btype == Bond::Double) return Bond::Triple;
	return Bond::Single;
}

// Constructors
Bond::Bond()
{
	// Private variables
	order_ = Bond::Unspecified;
	atomI_ = NULL;
	atomJ_ = NULL;
}

// Set atoms for bond
void Bond::setAtoms(Atom *i, Atom *j)
{
	atomI_ = i;
	atomJ_ = j;
}

// Set atom I for bond
void Bond::setAtomI(Atom *i)
{
	atomI_ = i;
}

// Set atom I for bond
void Bond::setAtomJ(Atom *j)
{
	atomJ_ = j;
}

// Set bond order
void Bond::setOrder(Bond::BondType bt)
{
	order_ = bt;
}

// Return order of bond
Bond::BondType Bond::order()
{
	return order_;
}

// Return first atom in bond
Atom *Bond::atomI()
{
	return atomI_;
}

// Return second atom in bond
Atom *Bond::atomJ()
{
	return atomJ_;
}

// Returns the partner of the specified atom in the bond structure
Atom *Bond::partner(Atom *i)
{
	return (i == atomI_ ? atomJ_ : atomI_);
}
