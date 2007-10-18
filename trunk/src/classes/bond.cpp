/*
	*** Atomic bond
	*** src/classes/bond.cpp

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
bond_type BT_from_text(const char *s)
	{ return (bond_type) enum_search("bond type",BT_NITEMS,BT_keywords,s); }
bond_type operator++(bond_type &btype,int)
{
	if (btype == BT_SINGLE) btype = BT_DOUBLE;
	else if (btype == BT_DOUBLE) btype = BT_TRIPLE;
	else if (btype == BT_TRIPLE) btype = BT_SINGLE;
	return btype;
}
bond_type operator--(bond_type &btype,int)
{
	if (btype == BT_TRIPLE) btype = BT_DOUBLE;
	else if (btype == BT_DOUBLE) btype = BT_SINGLE;
	else if (btype == BT_SINGLE) btype = BT_TRIPLE;
	return btype;
}

// Constructors
bond::bond()
{
	type = BT_UNSPECIFIED;
	bondi = NULL;
	bondj = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_BOND] ++;
	#endif
}

linkbond::linkbond()
{
	next = NULL;
	prev = NULL;
	#ifdef MEMDEBUG
	memdbg.create[MD_LINKBOND] ++;
	#endif
}

// Destructors
bond::~bond()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_BOND] ++;
	#endif
}

linkbond::~linkbond()
{
	#ifdef MEMDEBUG
	memdbg.destroy[MD_LINKBOND] ++;
	#endif
}

// Augment ring atom
void ring::augment_atom(refitem<atom> *refatom)
{
	dbg_begin(DM_CALLS,"ring::augment_atom");
	// Assumes current bond order differences are in i->tempi
	atom *i, *j;
	i = refatom->item;
	if (i->tempi < 0)
	{
		// Atom has fewer bonds than expected, so try to augment within ring.
		j = get_next(refatom)->item;
		if (j->tempi < 0) i->alter_bondorder(j,+1);
		if (i->tempi != 0)
		{
			j = get_prev(refatom)->item;
			if (j->tempi < 0) i->alter_bondorder(j,+1);
		}
	}
	else if (i->tempi > 0)
	{
		// Atom has more bonds than expected, so try to de-augment within ring.
		j = get_next(refatom)->item;
		if (j->tempi > 0) i->alter_bondorder(j,-1);
		if (i->tempi != 0)
		{
			j = get_prev(refatom)->item;
			if (j->tempi > 0) i->alter_bondorder(j,-1);
		}
	}
	dbg_end(DM_CALLS,"ring::augment_atom");
}

