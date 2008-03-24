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

// Bond types
const char *BT_keywords[BT_NITEMS] = { "none", "single", "double", "triple" };
BondType BT_from_text(const char *s)
	{ return (BondType) enumSearch("bond type",BT_NITEMS,BT_keywords,s); }
BondType operator++(BondType &btype,int)
{
	if (btype == BT_SINGLE) btype = BT_DOUBLE;
	else if (btype == BT_DOUBLE) btype = BT_TRIPLE;
	else if (btype == BT_TRIPLE) btype = BT_SINGLE;
	return btype;
}
BondType operator--(BondType &btype,int)
{
	if (btype == BT_TRIPLE) btype = BT_DOUBLE;
	else if (btype == BT_DOUBLE) btype = BT_SINGLE;
	else if (btype == BT_SINGLE) btype = BT_TRIPLE;
	return btype;
}

// Constructors
Bond::Bond()
{
	// Private variables
	order_ = BT_UNSPECIFIED;
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
void Bond::setOrder(BondType bt)
{
	order_ = bt;
}

// Return order of bond
BondType Bond::order()
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
