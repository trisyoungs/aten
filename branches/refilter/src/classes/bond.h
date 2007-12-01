/*
	*** Atomic bond
	*** src/classes/bond.h
	Copyright T. Youngs 2007

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

#ifndef H_BOND_H
#define H_BOND_H

// Bond types
enum bond_type { BT_UNSPECIFIED, BT_SINGLE, BT_DOUBLE, BT_TRIPLE, BT_NITEMS };
bond_type BT_from_text(const char *);

// Forward declarations
class atom;

// Basic Bond Definition
class bond
{
	public:
	// Constructor / Destructor
	bond();
	~bond();
	// Pointers to both atoms involved in the bond
	atom *bondi, *bondj;
	// Bond order
	bond_type type;
	// Returns the partner of the specified atom in the bond structure
	atom *get_partner(atom *i) { return (i == bondi ? bondj : bondi); }
};

// Linkable Bond
class linkbond : public bond
{
	public:
	// Constructor / Destructor
	linkbond();
	~linkbond();
	// List pointer
	linkbond *next, *prev;
};

#endif
