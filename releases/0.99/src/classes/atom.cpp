/*
	*** Basic atom
	*** src/classes/atom.cpp
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

#include "classes/atom.h"
#include "classes/bond.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/debug.h"

// Atom drawing styles
const char *DrawStyleKeywords[Atom::nDrawStyles] = { "Stick", "Tube", "Sphere", "Scaled", "Individual" };
Atom::DrawStyle Atom::drawStyle(const char *s)
{
	return (Atom::DrawStyle) enumSearch("draw style", Atom::nDrawStyles, DrawStyleKeywords, s);
}
const char *Atom::drawStyle(Atom::DrawStyle i)
{
	return DrawStyleKeywords[i];
}

// Atom labels
const char *AtomLabelKeywords[Atom::nLabelItems] = { "id", "element", "type", "ffequiv", "charge" };
Atom::AtomLabel Atom::atomLabel(const char *s)
{
	return (Atom::AtomLabel) power(2,enumSearch("atom label", Atom::nLabelItems, AtomLabelKeywords, s));
}

// Constructor
Atom::Atom()
{
	// Private variables
	charge_ = 0.0;
	el_ = 0;
	os_ = 0;
	environment_ = Atomtype::NoEnvironment;
	type_ = NULL;
	fixedType_ = FALSE;
	fixedPosition_ = FALSE;
	selected_ = FALSE;
	hidden_ = FALSE;
	screenRadius_ = 0.0;
	style_ = StickStyle;
	labels_ = 0;
	// Public variables
	next = NULL;
	prev = NULL;
	tempi = 0;
}

/*
// Coordinates
*/
Vec3<double> &Atom::r()
{
	return r_;
}

/*
// Forces
*/
Vec3<double> &Atom::f()
{
	return f_;
}

/*
// Velocities
*/
Vec3<double> &Atom::v()
{
	return v_;
}

/*
// Character
*/

// Sets the atom charge
void Atom::setCharge(double d)
{
	charge_ = d;
}

// Return the atom charge
double Atom::charge()
{
	return charge_; 
}

// Set the element type of the atom
void Atom::setElement(short int newel)
{
	el_ = newel;
}

// Return the element of the atom
short int Atom::element()
{
	return el_;
}

// Check element against the supplied value
bool Atom::isElement(short int n)
{
	return (n == el_ ? TRUE : FALSE);
}

// Check oxidation state against supplied value
bool Atom::isOs(short int n)
{
	return (n == os_ ? TRUE : FALSE);
}

// Return the oxidation state of the atom
short int Atom::os()
{
	return os_;
}

// Set the forcefield type of the atom
void Atom::setType(ForcefieldAtom *ffa)
{
	type_ = ffa;
}

// Return the forcefield type of the atom
ForcefieldAtom *Atom::type()
{
	return type_;
}

// Set the fixed status of the assigned atom type
void Atom::setTypeFixed(bool b)
{
	fixedType_ = b;
}

// Return the fixed status of the assigned atom type
bool Atom::hasFixedType()
{
	return fixedType_;
}

// Set whether the atom's position is fixed
void Atom::setPositionFixed(bool b)
{
	fixedPosition_ = b;
}

// Return whether the atom's position is fixed
bool Atom::hasFixedPosition()
{
	return fixedPosition_;
}

// Check the ff type of the atom against the supplied value
bool Atom::typeIs(ForcefieldAtom *type)
{
	return (type == type ? TRUE : FALSE);
}

// Set the environment of the atom
void Atom::setEnvironment(Atomtype::AtomEnvironment ae)
{
	environment_ = ae;
}

// Return the environment of the atom
Atomtype::AtomEnvironment Atom::environment()
{
	return environment_;
}

// Check the environment of the atom against the supplied value
bool Atom::isEnvironment(Atomtype::AtomEnvironment ae)
{
	return (environment_ == ae ? TRUE : FALSE);
}

// Get next selected
Atom *Atom::nextSelected()
{
	Atom *i;
	for (i = this->next; i != NULL; i = i->next) if (i->selected_) break;
	return i;
}

// Reset data in structure
void Atom::reset()
{
	el_ = 0;
	r_.zero();
	f_.zero();
	v_.zero();
	id_ = -1;
}

// Copy atom data
void Atom::copy(Atom *source)
{
	r_ = source->r_;
	f_ = source->f_;
	v_ = source->v_;
	charge_ = source->charge_;
	el_ = source->el_;
	style_ = source->style_;
	environment_ = source->environment_;
	type_ = source->type_;
	fixedType_ = source->fixedType_;
}

// Copy atom style
void Atom::copyStyle(Atom *source)
{
	style_ = source->style_;
	hidden_ = source->hidden_;
}

// Print
void Atom::print()
{
	// Note: We print the 'visual' id (id_ + 1) and not the internal id (id_)
	msg(Debug::None,"Atom ID %i (%s):\n", id_+1, elements.name(this));
	msg(Debug::None," %s, %s, individual style is %s.\n", (selected_ ? "Selected" : "Not selected"), (hidden_ ? "hidden" : "not hidden"), drawStyle(style_));
	msg(Debug::None," Model Coord : %8.4f %8.4f %8.4f\n",r_.x,r_.y,r_.z);
	msg(Debug::None," World Coord : %8.4f %8.4f %8.4f\n",rWorld_.x,rWorld_.y,rWorld_.z);
	msg(Debug::None,"Screen Coord : %8.4f %8.4f \n",rScreen_.x,rScreen_.y,rScreen_.z);
	msg(Debug::None,"  Velocities : %8.4f %8.4f %8.4f\n",v_.x,v_.y,v_.z);
	msg(Debug::None,"      Forces : %8.4f %8.4f %8.4f\n",f_.x,f_.y,f_.z);
	msg(Debug::None,"      Charge : %8.4f\n",charge_);
	msg(Debug::None,"      FFType : %s\n",(type_ != NULL ? type_->name() : "None"));
	msg(Debug::None,"       Bonds : %i\n",bonds_.nItems());
	msg(Debug::None," Environment : %s\n",Atomtype::atomEnvironment(environment_));
	msg(Debug::None,"        O.S. : %i\n",os_);
}

// Print summary
void Atom::printSummary()
{
	// Print format : " Id     El   FFType         X             Y             Z              Q        S"
	// Note: We print the 'visual' id (id_ + 1) and not the internal id (id_)
	msg(Debug::None," %-5i  %-3s  %-8s", id_ + 1, elements.symbol(this),(type_ != NULL ? type_->name() : "None"));
	msg(Debug::None," %13.6e %13.6e %13.6e  %13.6e  ",r_.x, r_.y, r_.z, charge_);
	msg(Debug::None,"%c  \n",(selected_ ? 'x' : ' '));
}

/*
// Bonds / Bonding
*/

// Return the number of bonds to the atom
int Atom::nBonds()
{
	return bonds_.nItems();
}

// Return the current bond list
Refitem<Bond,int> *Atom::bonds()
{
	return bonds_.first();
}

// Check the number of bonds against the supplied value
bool Atom::isNBonds(int n)
{
	return (bonds_.nItems() == n ? TRUE : FALSE);
}

// Accept the specified bond to the atom's local reference list
void Atom::acceptBond(Bond *b)
{
	bonds_.add(b);
}

// Detach bond
void Atom::detachBond(Bond *xbond)
{
	// Remove the reference to the bond from the Reflist on the atom.
	dbgBegin(Debug::MoreCalls,"Atom::detachBond");
	// Mark pointer as NULL. If both are NULL, delete the bond.
	bonds_.remove(xbond);
	if (xbond->atomI() == this)
	{
		xbond->setAtomI(NULL);
		if (xbond->atomJ() == NULL) delete xbond;
	}
	else
	{
		xbond->setAtomJ(NULL);
		if (xbond->atomI() == NULL) delete xbond;
	}
	dbgEnd(Debug::MoreCalls,"Atom::detachBond");
}

// Total bond order
int Atom::totalBondOrder()
{
	// Calculate the total bond order of the atom
	// Returned result is 2*actual bond order (to account for resonant bonds [BO = 1.5])
	dbgBegin(Debug::Calls,"Atom::totalBondOrder");
	int result = 0;
	for (Refitem<Bond,int> *bref = bonds(); bref != NULL; bref = bref->next)
		result += (2 * bref->item->order());
	dbgEnd(Debug::Calls,"Atom::totalBondOrder");
	return result;
}

// Count bonds of specific type
int Atom::countBonds(Bond::BondType type)
{
	dbgBegin(Debug::Calls,"Atom::countBonds");
	int count = 0;
	for (Refitem<Bond,int> *bref = bonds(); bref != NULL; bref = bref->next)
		if (bref->item->order() == type) count ++;
	dbgEnd(Debug::Calls,"Atom::countBonds");
	return count;
}

// Find bond to atom 'j'
Bond *Atom::findBond(Atom *j)
{
	dbgBegin(Debug::MoreCalls,"Atom::findBond");
	Bond *result = NULL;
	Refitem<Bond,int> *bref = bonds();
	while (bref != NULL)
	{
		if (bref->item->partner(this) == j) result = bref->item;
		bref = bref->next;
	}
	dbgEnd(Debug::MoreCalls,"Atom::findBond");
	return result;
}

// Calculate bond order with specified partner
double Atom::bondOrder(Atom *j)
{
	// Returns the (fractional) bond order of the bond between this atom and j.
	// Aromatic bonds are given a bond order of 1.5.
	dbgBegin(Debug::Calls,"Atom::bondOrder");
	double order;
	// First, find the bond
	Bond *b = findBond(j);
	// Criticality check
	if (b == NULL)
	{
		printf("bondOrder : Failed to find bond between atoms!\n");
		dbgEnd(Debug::Calls,"Atom::bondOrder");
		return 0.0;
	}
	// Get the enum'd type of the bond and 'convert' it to the bond order
	order = b->order();
	// Special case where both atoms are AtomEnvironment::AromaticEnvironment - bond order is then 1.5.
	if ((environment_ == Atomtype::AromaticEnvironment) && (j->environment_ == Atomtype::AromaticEnvironment)) order = 1.5;
	dbgEnd(Debug::Calls,"Atom::bondOrder");
	return order;
}

// Determine bonding geometry
Atomtype::AtomGeometry Atom::geometry(Model *parent)
{
	dbgBegin(Debug::Calls,"Atom::geometry");
	static Atomtype::AtomGeometry result;
	static double angle, largest;
	static Bond *b1, *b2;
	static Refitem<Bond,int> *bref1, *bref2;
	result = Atomtype::NoGeometry;
	// Separate the tests by number of bound atoms...
	switch (nBonds())
	{
		// Simple cases first
		case (0):
			result = Atomtype::UnboundGeometry;
			break;
		case (1):
			result = Atomtype::OneBondGeometry;
			break;
		case (5):
			result = Atomtype::TrigBipyramidGeometry;
			break;
		case (6):
			result = Atomtype::OctahedralGeometry;
			break;
		// For the remaining types, take averages of bond angles about the atom
		case (2):
			b1 = bonds()->item;
			b2 = bonds()->next->item;
			angle = parent->angle(b1->partner(this),this,b2->partner(this)) * DEGRAD;
			result = (angle > 170.0 ? Atomtype::LinearGeometry : Atomtype::TetrahedralGeometry);
			break;
		case (3):
			bref1 = bonds();
			bref2 = bonds()->next;
			b1 = bref1->item;
			b2 = bref2->item;
			angle = parent->angle(b1->partner(this),this,b2->partner(this)) * DEGRAD;
			largest = angle;
			b2 = bref2->next->item;
			angle = parent->angle(b1->partner(this),this,b2->partner(this)) * DEGRAD;
			if (angle > largest) largest = angle;
			b1 = bref1->next->item;
			angle = parent->angle(b1->partner(this),this,b2->partner(this)) * DEGRAD;
			if (angle > largest) largest = angle;
			result = (largest > 170.0 ? Atomtype::TShapeGeometry : (largest > 115.0 ? Atomtype::TrigPlanarGeometry : Atomtype::TetrahedralGeometry));
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
					angle += parent->angle(bref1->item->partner(this),this,bref2->item->partner(this)) * DEGRAD;
					//printf("Case 4: added an angle.\n");
					bref2 = bref2->next;
				}
				bref1 = bref1->next;
			}
			result = ((angle/6.0) > 115.0 ? Atomtype::SquarePlanarGeometry : Atomtype::TetrahedralGeometry);
			break;
	}
	dbgEnd(Debug::Calls,"Atom::geometry");
	return result;
}

// Add bound neighbours to Reflist
void Atom::addBoundToReflist(Reflist<Atom,int> *rlist)
{
	// Add all atoms bound to the supplied atom to the atomReflist.
	for (Refitem<Bond,int> *bref = bonds(); bref != NULL; bref = bref->next)
		rlist->add(bref->item->partner(this),bref->item->order());
}

// Find plane of three atoms
Vec3<double> Atom::findBondPlane(Atom *j, Bond *b, const Vec3<double> &rij)
{
	// Given this atom, another (j), and a bond node on 'this' between them, determine the plane of the bond if possible.
	static Vec3<double> rk, xp1, xp2;
	Refitem<Bond,int> *brefi = bonds_.first();
	Refitem<Bond,int> *brefj = j->bonds_.first();
	if (bonds_.nItems() != 1)	// Can define from another bond on 'this'
		b == brefi->item ? rk = brefi->next->item->partner(this)->r_ : rk = brefi->item->partner(this)->r_;
	else if (j->bonds_.nItems() != 1)// Can define from another bond on j
		this == brefj->item->partner(j) ? rk = brefj->next->item->partner(j)->r_ : rk = brefj->item->partner(j)->r_;
	else rk.zero();		// Default, just in case
	// Now, take cross product of rij and (repositioned) rk -> perpendicular vector
	rk -= r_;
	xp1 = rij * rk;
	// Cross product of this new vector with rij -> bond plane vector.
	// Get this vector, normalise and take a fraction of it
	xp2 = xp1 * rij;
	xp2.normalise();
	return xp2;
}

/*
// Selection
*/

// Sets the selected flag of the atom
void Atom::setSelected(bool b)
{
	selected_ = b;
}

// Returns the current selection state of the atom
bool Atom::isSelected()
{
	return selected_;
}

// Sets the hidden flag of the atom
void Atom::setHidden(bool b)
{
	hidden_ = b;
}

// Return whether the atom is hidden
bool Atom::isHidden()
{
	return hidden_;
}

/*
// Identity
*/

// Sets the atom id
void Atom::setId(int newid)
{
	id_ = newid;
}

// Decreases the id of the atom by 1
void Atom::decreaseId()
{
	id_ --;
}

// Return the id of the atom
int Atom::id()
{
	return id_;
}

/*
// Rendering Coordinates
*/

// World (GL Transformed) coordinates
Vec3<double> &Atom::rWorld()
{
	return rWorld_;
}

// Screen (two-dimensional) coordinates
Vec3<double> &Atom::rScreen()
{
	return rScreen_;
}

// Set the screen radius of the atom
void Atom::setScreenRadius(double radius)
{
	screenRadius_ = radius;
}

// Return the screen radius of the atom
double Atom::screenRadius()
{
	return screenRadius_;
}

/*
// Rendering
*/

// Sets the drawing style of the atom
void Atom::setStyle(Atom::DrawStyle style)
{
	style_ = style;
}

// Returns the drawing style of the atom
Atom::DrawStyle Atom::style()
{
	return style_;
}

// Returns TRUE id the atom has at least one label specified
bool Atom::hasLabels()
{
	return (labels_ == 0 ? FALSE : TRUE);
}

// Set label bitvector to specified value
void Atom::setLabels(short int l)
{
	labels_ = l;
}

// Returns the label bitmask of the atom
short int Atom::labels()
{
	return labels_;
}

// Set the bit for the specified label (if it is not set already)
void Atom::addLabel(Atom::AtomLabel label) { if (!(labels_&label)) labels_ += (short int) label;
}

// Unsets the bit for the specified label (if it is not unset already)
void Atom::removeLabel(Atom::AtomLabel label) { if (labels_&label) labels_ -= (short int) label;
}

// Clear all labels from the atom
void Atom::clearLabels() { labels_ = 0;
}
