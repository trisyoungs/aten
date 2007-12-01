/*
	*** Basic atom
	*** src/classes/atom.cpp
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

#include "classes/atom.h"
#include "classes/bond.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/debug.h"

// Atom drawing styles
const char *DS_strings[DS_NITEMS] = { "Stick", "Tube", "Sphere", "Scaled", "Individual" };
draw_style DS_from_text(const char *s)
	{ return (draw_style) enum_search("rendering style",DS_NITEMS,DS_strings,s); }
const char **get_DS_strings()
	{ return DS_strings; }
const char *text_from_DS(draw_style i)
	{ return DS_strings[i]; }

// Atom labels
const char *AL_keywords[AL_NITEMS] = { "id", "element", "type", "ffequiv", "charge" };
atom_label AL_from_text(const char *s)
	{ return (atom_label) int(pow(2,enum_search("atom label type",AL_NITEMS,AL_keywords,s))); }

// Constructors
atom::atom()
{
	q = 0.0;
	el = 0;
	os = 0;
	env = AE_UNSPECIFIED;
	type = NULL;
	typefixed = FALSE;
	next = NULL;
	prev = NULL;
	tempi = 0;
	selected = FALSE;
	hidden = FALSE;
	screenrad = 0.0;
	style = DS_STICK;
	fixed = FALSE;
	labels = 0;
	#ifdef MEMDEBUG
		memdbg.create[MD_ATOM] ++;
	#endif
}

// Destructors
atom::~atom()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_ATOM] ++;
	#endif
}

// Get next selected
atom *atom::get_next_selected()
{
	atom *result = NULL;
	atom *i = next;
	while (i != NULL)
	{
		if (i->selected)
		{
			result = i;
			break;
		}
		i = i->next;
	}
	return result;
}

// Reset data in structure
void atom::reset()
{
	el = 0;
	rr.zero();
	ff.zero();
	vv.zero();
	id = -1;
}

// Copy atom data
void atom::copy(atom *source)
{
	rr = source->rr;
	ff = source->ff;
	vv = source->vv;
	q = source->q;
	el = source->el;
	style = source->style;
	env = source->env;
	type = source->type;
	typefixed = source->typefixed;
}

// Copy atom style
void atom::copy_style(atom *source)
{
	style = source->style;
	hidden = source->hidden;
}

// Print
void atom::print()
{
	msg(DM_NONE,"Atom ID %i (%s):\n",id,elements.name(this));
	msg(DM_NONE,"    Selected : %s,  Hidden : %s\n",(selected ? "Yes" : "No"),(hidden ? "Yes" : "No"));
	msg(DM_NONE,"  Draw Style : %s\n",text_from_DS(style));
	msg(DM_NONE," Model Coord : %8.4f %8.4f %8.4f\n",rr.x,rr.y,rr.z);
	msg(DM_NONE," World Coord : %8.4f %8.4f %8.4f\n",rr_world.x,rr_world.y,rr_world.z);
	msg(DM_NONE,"Screen Coord : %8.4f %8.4f \n",rr_screen.x,rr_screen.y,rr_screen.z);
	msg(DM_NONE,"  Velocities : %8.4f %8.4f %8.4f\n",vv.x,vv.y,vv.z);
	msg(DM_NONE,"      Forces : %8.4f %8.4f %8.4f\n",ff.x,ff.y,ff.z);
	msg(DM_NONE,"      Charge : %8.4f\n",q);
	msg(DM_NONE,"      FFType : %s\n",(type != NULL ? type->get_name() : "None"));
	msg(DM_NONE," Environment : %s\n",text_from_AE(env));
	msg(DM_NONE,"        O.S. : %i\n",os);
}

// Print summary
void atom::print_summary()
{
	// Print format : " Id     El   FFType         X             Y             Z              Q        S"
	msg(DM_NONE," %-5i  %-3s  %-8s",id,elements.symbol(this),(type != NULL ? type->get_name() : "None"));
	msg(DM_NONE," %13.6e %13.6e %13.6e  %13.6e  ",rr.x,rr.y,rr.z,q);
	msg(DM_NONE,"%c  \n",(selected ? 'x' : ' '));
}

/*
// Bonds / Bonding
*/

// Detach bond
void atom::detach_bond(bond *xbond)
{
	// Remove the reference to the bond from the reflist on the atom.
	dbg_begin(DM_MORECALLS,"atom::detach_bond");
	// Mark pointer as NULL. If both are NULL, delete the bond.
	bonds.remove(xbond);
	if (xbond->bondi == this)
	{
		xbond->bondi = NULL;
		if (xbond->bondj == NULL) delete xbond;
	}
	else
	{
		xbond->bondj = NULL;
		if (xbond->bondi == NULL) delete xbond;
	}
	dbg_end(DM_MORECALLS,"atom::detach_bond");
}

// Total bond order
int atom::total_bond_order()
{
	// Calculate the total bond order of the atom
	// Returned result is 2*actual bond order (to account for resonant bonds [BO = 1.5])
	dbg_begin(DM_CALLS,"atom::total_bond_order");
	int result;
	result = 0;
	refitem<bond> *bref = get_bonds();
	while (bref != NULL)
	{
		result += (2 * bref->item->type);
		bref = bref->next;
	}
	dbg_end(DM_CALLS,"atom::total_bond_order");
	return result;
}

// Count bonds of specific type
int atom::count_bonds(bond_type type)
{
	dbg_begin(DM_CALLS,"atom::count_bonds");
	int count = 0;
	refitem<bond> *bref = get_bonds();
	while (bref != NULL)
	{
		if (bref->item->type == type) count ++;
		bref = bref->next;
	}
	dbg_end(DM_CALLS,"atom::count_bonds");
	return count;
}

// Find bond to atom 'j'
bond *atom::find_bond(atom *j)
{
	dbg_begin(DM_MORECALLS,"atom::find_bond");
	bond *result = NULL;
	refitem<bond> *bref = get_bonds();
	while (bref != NULL)
	{
		if (bref->item->get_partner(this) == j) result = bref->item;
		bref = bref->next;
	}
	dbg_end(DM_MORECALLS,"atom::find_bond");
	return result;
}

// Calculate bond order with specified partner
double atom::get_bond_order(atom *j)
{
	// Returns the (fractional) bond order of the bond between this atom and j.
	// Aromatic bonds are given a bond order of 1.5.
	dbg_begin(DM_CALLS,"atom::get_bond_order");
	double order;
	// First, find the bond
	bond *b = find_bond(j);
	// Criticality check
	if (b == NULL)
	{
		printf("get_bond_order : Failed to find bond between atoms!\n");
		dbg_end(DM_CALLS,"atom::get_bond_order");
		return 0.0;
	}
	// Get the enum'd type of the bond and 'convert' it to the bond order
	order = b->type;
	// Special case where both atoms are AE_AROMATIC - bond order is then 1.5.
	if ((env == AE_AROMATIC) && (j->get_env() == AE_AROMATIC)) order = 1.5;
	dbg_end(DM_CALLS,"atom::get_bond_order");
	return order;
}

// Determine bonding geometry
atom_geom atom::get_geometry(model *parent)
{
	dbg_begin(DM_CALLS,"atom::get_geometry");
	static atom_geom result;
	static double angle, largest;
	static bond *b1, *b2;
	static refitem<bond> *bref1, *bref2;
	result = AG_UNSPECIFIED;
	// Separate the tests by number of bound atoms...
	switch (get_nbonds())
	{
		// Simple cases first
		case (0):
			result = AG_UNBOUND;
			break;
		case (1):
			result = AG_ONEBOND;
			break;
		case (5):
			result = AG_TRIGBIPYRAMID;
			break;
		case (6):
			result = AG_OCTAHEDRAL;
			break;
		// For the remaining types, take averages of bond angles about the atom
		case (2):
			b1 = get_bonds()->item;
			b2 = get_bonds()->next->item;
			angle = parent->angle(b1->get_partner(this),this,b2->get_partner(this)) * DEGRAD;
			result = (angle > 170.0 ? AG_LINEAR : AG_TETRAHEDRAL);
			break;
		case (3):
			bref1 = get_bonds();
			bref2 = get_bonds()->next;
			b1 = bref1->item;
			b2 = bref2->item;
			angle = parent->angle(b1->get_partner(this),this,b2->get_partner(this)) * DEGRAD;
			largest = angle;
			b2 = bref2->next->item;
			angle = parent->angle(b1->get_partner(this),this,b2->get_partner(this)) * DEGRAD;
			if (angle > largest) largest = angle;
			b1 = bref1->next->item;
			angle = parent->angle(b1->get_partner(this),this,b2->get_partner(this)) * DEGRAD;
			if (angle > largest) largest = angle;
			result = (largest > 170.0 ? AG_TSHAPE : (largest > 115.0 ? AG_TRIGPLANAR : AG_TETRAHEDRAL));
			break;
		case (4):
			// Two possibilities - tetrahedral or square planar. Tetrahedral will have an
			// average of all angles of ~ 109.5, for square planar (1/6) * (4*90 + 2*180) = 120
			angle = 0.0;
			bref1 = get_bonds();
			while (bref1->next != NULL)
			{
				bref2 = bref1->next;
				while (bref2 != NULL)
				{
					angle += parent->angle(bref1->item->get_partner(this),this,bref2->item->get_partner(this)) * DEGRAD;
					//printf("Case 4: added an angle.\n");
					bref2 = bref2->next;
				}
				bref1 = bref1->next;
			}
			result = ((angle/6.0) > 115.0 ? AG_SQPLANAR : AG_TETRAHEDRAL);
			break;
	}
	dbg_end(DM_CALLS,"atom::get_geometry");
	return result;
}

// Add bound neighbours to reflist
void atom::add_bound_to_reflist(reflist<atom> *rlist)
{
	// Add all atoms bound to the supplied atom to the atomreflist.
	refitem<bond> *bref = get_bonds();
	while (bref != NULL)
	{
		rlist->add(bref->item->get_partner(this),0,bref->item->type);
		bref = bref->next;
	}
}

// Find plane of three atoms
vec3<double> atom::find_bond_plane(atom *j, bond *b, const vec3<double> &rij)
{
	// Given this atom, another (j), and a bond node on 'this' between them, determine the plane of the bond if possible.
	static vec3<double> rk, xp1, xp2;
	refitem<bond> *brefi = bonds.first();
	refitem<bond> *brefj = j->bonds.first();
	if (bonds.size() != 1)	// Can define from another bond on 'this'
		b == brefi->item ? rk = brefi->next->item->get_partner(this)->rr : rk = brefi->item->get_partner(this)->rr;
	else if (j->bonds.size() != 1)// Can define from another bond on j
		this == brefj->item->get_partner(j) ? rk = brefj->next->item->get_partner(j)->rr : rk = brefj->item->get_partner(j)->rr;
	else rk.zero();		// Default, just in case
	// Now, take cross product of rij and (repositioned) rk -> perpendicular vector
	rk -= rr;
	xp1 = rij * rk;
	// Cross product of this new vector with rij -> bond plane vector.
	// Get this vector, normalise and take a fraction of it
	xp2 = xp1 * rij;
	xp2.normalise();
	return xp2;
}
