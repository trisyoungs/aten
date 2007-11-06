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
const char *AL_keywords[AL_NITEMS] = { "id", "element", "fftype", "ffequiv", "charge" };
atom_label AL_from_text(const char *s)
	{ return (atom_label) pow(2,enum_search("atom label type",AL_NITEMS,AL_keywords,s)); }

// Constructors
atom::atom()
{
	q = 0.0;
	el = 0;
	os = 0;
	env = AE_UNSPECIFIED;
	fftype = NULL;
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
	clear_bonds();
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
	r.zero();
	f.zero();
	v.zero();
	id = -1;
}

// Copy atom data
void atom::copy(atom *source)
{
	r = source->r;
	f = source->f;
	v = source->v;
	q = source->q;
	el = source->el;
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
	msg(DM_NONE," Model Coord : %8.4f %8.4f %8.4f\n",r.x,r.y,r.z);
	msg(DM_NONE," World Coord : %8.4f %8.4f %8.4f\n",worldr.x,worldr.y,worldr.z);
	msg(DM_NONE,"Screen Coord : %8.4f %8.4f \n",screenr.x,screenr.y,screenr.z);
	msg(DM_NONE,"  Velocities : %8.4f %8.4f %8.4f\n",v.x,v.y,v.z);
	msg(DM_NONE,"      Forces : %8.4f %8.4f %8.4f\n",f.x,f.y,f.z);
	msg(DM_NONE,"      Charge : %8.4f\n",q);
	msg(DM_NONE,"      FFType : %s\n",(fftype != NULL ? fftype->get_name() : "None"));
	msg(DM_NONE," Environment : %s\n",text_from_AE(env));
	msg(DM_NONE,"        O.S. : %i\n",os);
}

// Print summary
void atom::print_summary()
{
	// Print format : " Id     El   FFType         X             Y             Z              Q        S"
	msg(DM_NONE," %-5i  %-3s  %-8s",id,elements.symbol(this),(fftype != NULL ? fftype->get_name() : "None"));
	msg(DM_NONE," %13.6e %13.6e %13.6e  %13.6e  ",r.x,r.y,r.z,q);
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

// Delete All Bonds To Specific Atom
void atom::clear_bonds()
{
	dbg_begin(DM_MORECALLS,"atom::clear_bonds");
	refitem<bond> *bref = get_bonds();
        while (bref != NULL)
        {
		// Need to detach the bond from both atoms involved
		bond *oldbond = bref->item;
		atom *j = oldbond->get_partner(this);
		j->detach_bond(oldbond);
		detach_bond(oldbond);
		bref = get_bonds();
        }
	dbg_end(DM_MORECALLS,"atom::clear_bonds");
}

void atom::add_hydrogens(int nhydrogen, hadd_geom geometry, model *xmodel)
{
	// Iteratively add hydrogens to the molecule conforming to the desired geometry specified
	dbg_begin(DM_CALLS,"atom::add_hydrogens");
	atom *a1, *a2, *a3;
	atom *newh;
	vec3<double> mim_a1, mim_a2, mim_a3, perp, perp2, newhpos, tempv;
	double bondlength = 1.08, theta, tets;
	int minel, onebelow, oneabove;
	// Add new hydrogens based on the geometry type, and then work out from what bonds there are already...
	switch (geometry)
	{
		case (HG_LINEAR): theta = 180.0 / DEGRAD; break;
		case (HG_PLANAR): theta = 120.0 / DEGRAD; break;
		case (HG_TETRAHEDRAL): theta = 109.5 / DEGRAD; break;
	}
	// Switches put new coordinates in 'newhpos' - an atom is created an placed here at the end
	for (int n=0; n<nhydrogen; n++)
	{
		switch (get_nbonds())
		{
			case (0):
				// No bonds, so add at arbitrary position along y-axis
				newhpos.set(0.0,bondlength,0.0);
				break;
			case (1):
				// Only one bond, so add atom at arbitrary position with angle of required geometry
				a1 = get_bonds()->item->get_partner(this);
				mim_a1 = xmodel->cell.mimd(this,a1);
				mim_a1.normalise();
				// Create perpendicular vector to X-i...
				minel = mim_a1.absminelement();
				onebelow = (minel+2)%3;
				oneabove = (minel+1)%3;
				perp.set(minel,0.0);
				perp.set(onebelow,mim_a1.get(oneabove));
				perp.set(oneabove,-mim_a1.get(onebelow));
				perp.normalise();
				newhpos = mim_a1 * -bondlength * cos(theta) + perp * bondlength * sin(theta);
				break;
			case (2):
				// Two bonds, so work out 'pointing' vector and adjust to desired angle (if !HG_PLANAR)
				// Get mim coordinates of the two bound atoms
				a1 = get_bonds()->item->get_partner(this);
				a2 = get_bonds()->next->item->get_partner(this);
				mim_a1 = xmodel->cell.mimd(a1,this);
				mim_a1.normalise();
				mim_a2 = xmodel->cell.mimd(a2,this);
				mim_a2.normalise();
				perp = mim_a1 * mim_a2;
				perp.normalise();
				tempv = mim_a1 + mim_a2;
				tempv.normalise();
				if (geometry != HG_PLANAR)
					newhpos = tempv * -bondlength * cos(theta*0.5) + perp * -bondlength * sin(theta*0.5);
				else newhpos = tempv * -bondlength;
				break;
			case (3):
				// Three bonds, so work out negative vector of the average of the three bonds
				a1 = get_bonds()->item->get_partner(this);
				a2 = get_bonds()->next->item->get_partner(this);
				a3 = get_bonds()->next->next->item->get_partner(this);
				mim_a1 = xmodel->cell.mimd(a1,this);
				mim_a1.normalise();
				mim_a2 = xmodel->cell.mimd(a2,this);
				mim_a2.normalise();
				mim_a3 = xmodel->cell.mimd(a3,this);
				mim_a3.normalise();
				newhpos = mim_a1 + mim_a2 + mim_a3;
				newhpos.normalise();
				newhpos *= -bondlength;
				break;
		}
		// Now add the atom at the position specified in newhpos.
		newh = xmodel->add_atom(1);
		xmodel->bond_atoms(newh,this,BT_SINGLE);
		newh->r = newhpos + r;
	}
	dbg_end(DM_CALLS,"atom::add_hydrogens");
}

// Determine bonding geometry
atom_geom atom::get_geometry(model *parent)
{
	dbg_begin(DM_CALLS,"atom::get_geometry");
	static atom_geom result;
	static double angle;
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
			angle = parent->cell.angle(b1->get_partner(this),this,b2->get_partner(this));
			result = (angle > 170.0 ? AG_LINEAR : AG_TETRAHEDRAL);
			break;
		case (3):
			bref1 = get_bonds();
			bref2 = get_bonds()->next;
			b1 = bref1->item;
			b2 = bref2->item;
			angle = parent->cell.angle(b1->get_partner(this),this,b2->get_partner(this));
			b2 = bref2->next->item;
			angle += parent->cell.angle(b1->get_partner(this),this,b2->get_partner(this));
			b1 = bref1->next->item;
			angle += parent->cell.angle(b1->get_partner(this),this,b2->get_partner(this));
			result = (angle > 118.0 ? AG_TRIGPLANAR : (angle < 100.0 ? AG_TSHAPE : AG_TETRAHEDRAL));
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
					angle += parent->cell.angle(bref1->item->get_partner(this),this,bref2->item->get_partner(this));
					printf("Case 4: added an angle.\n");
					bref2 = bref2->next;
				}
				bref1 = bref1->next;
			}
			result = (angle > 115.0 ? AG_SQPLANAR : AG_TETRAHEDRAL);
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

// Augment atom
void atom::augment()
{
	// Augment the current atom by increasing the bond order to its neighbours.
	// Assumes current bond order differences are in i->tempi.
	// Cycle through the bound atoms increasing / decreasing the bond order as much as we can for each.
	// Stop when we have no more atoms or the bond order difference is zero.
	dbg_begin(DM_CALLS,"atom::augment");
	refitem<bond> *bref = get_bonds();
	while (bref != NULL)
	{
		if (tempi == 0) break;
		if (tempi < 0) alter_bondorder(bref->item->get_partner(this),+1);
		else if (tempi > 0) alter_bondorder(bref->item->get_partner(this),-1);
		bref = bref->next;
	}
	dbg_end(DM_CALLS,"atom::augment");
}

// Change bond order between this atom and 'j'
void atom::alter_bondorder(atom *j, int change)
{
	// Increase the type of the bond between this atom and 'j' by as much as both atoms will allow.
	// Assumes current bond order differences are held in i->tempi.
	dbg_begin(DM_CALLS,"atom::alter_bondorder");
	short int maxchg;
	int n;
	// Calc max difference that we can (must) change the bond by...
	abs(tempi) < abs(j->tempi) ? maxchg = tempi : maxchg = j->tempi;
	maxchg /= 2;
	// Sanity check
	if (change == +1 && maxchg >= 0)
	{
		dbg_end(DM_CALLS,"atom::alter_bondorder");
		return;
	}
	if (change == -1 && maxchg <= 0)
	{
		dbg_end(DM_CALLS,"atom::alter_bondorder");
		return;
	}
	// Find bond between this atom and 'j'
	bond *b = find_bond(j);
	if (b != NULL)
	{
		// Store current bond order
		int bo = b->type;
		for (n=0; n<abs(maxchg); n++)
		{
			change == +1 ? bo ++ : bo --;
			j->tempi -= (2*maxchg);
			tempi -= (2*maxchg);
			//change == +1 ? bo ++ : bo --;
		}
		b->type = (bond_type) bo;
	}
	else printf("Augmenting failed to find bond to j on atom i.\n");
	dbg_end(DM_CALLS,"atom::alter_bondorder");
}

// Find plane of three atoms
vec3<double> atom::find_bond_plane(atom *j, bond *b, const vec3<double> &rij)
{
	// Given this atom, another (j), and a bond node on 'this' between them, determine the plane of the bond if possible.
	static vec3<double> rk, xp1, xp2;
	refitem<bond> *brefi = bonds.first();
	refitem<bond> *brefj = j->bonds.first();
	if (bonds.size() != 1)	// Can define from another bond on 'this'
		b == brefi->item ? rk = brefi->next->item->get_partner(this)->r : rk = brefi->item->get_partner(this)->r;
	else if (j->bonds.size() != 1)// Can define from another bond on j
		this == brefj->item->get_partner(j) ? rk = brefj->next->item->get_partner(j)->r : rk = brefj->item->get_partner(j)->r;
	else rk.zero();		// Default, just in case
	// Now, take cross product of rij and (repositioned) rk -> perpendicular vector
	rk -= r;
	xp1 = rij * rk;
	// Cross product of this new vector with rij -> bond plane vector.
	// Get this vector, normalise and take a fraction of it
	xp2 = xp1 * rij;
	xp2.normalise();
	return xp2;
}
