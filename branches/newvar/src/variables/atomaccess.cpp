/*
	*** Atom Access
	*** src/variables/atomaccess.cpp
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

#include "variables/atomaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/messenger.h"

AtomAccessors atomAccessors;

// Constructor
AtomAccessors::AtomAccessors()
{
 	accessorPointers[AtomAccessors::Charge] = addAccessor("charge",		VTypes::RealData, FALSE);
 	accessorPointers[AtomAccessors::Element] = addAccessor("element",		VTypes::IntegerData, TRUE);
 	accessorPointers[AtomAccessors::FX] = addAccessor("fx",		VTypes::RealData,	FALSE);
 	accessorPointers[AtomAccessors::FY] = addAccessor("fy",		VTypes::RealData,	FALSE);
 	accessorPointers[AtomAccessors::FZ] = addAccessor("fz",		VTypes::RealData,	FALSE);
	accessorPointers[AtomAccessors::Id] = addAccessor("id",		VTypes::IntegerData,	TRUE);
 	accessorPointers[AtomAccessors::Mass] = addAccessor("mass",		VTypes::RealData,	TRUE);
 	accessorPointers[AtomAccessors::RX] = addAccessor("rx",		VTypes::RealData,	FALSE);
 	accessorPointers[AtomAccessors::RY] = addAccessor("ry",		VTypes::RealData,	FALSE);
 	accessorPointers[AtomAccessors::RZ] = addAccessor("rz",		VTypes::RealData,	FALSE);
 	accessorPointers[AtomAccessors::Symbol] = addAccessor("symbol",		VTypes::CharacterData,	TRUE);
  	accessorPointers[AtomAccessors::Type] = addAccessor("type",		VTypes::ForcefieldAtomData,	FALSE);
};

// Retrieve specified data
bool AtomAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("AtomAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Atom*
	Atom *i = (Atom*) classptr;
	if (i == NULL) printf("Warning - NULL Atom pointer passed to AtomAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to AtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > AtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to AtomAccessors::set.\n", vid);
		msg.exit("AtomAccessors::retrieve");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("AtomAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (AtomAccessors::Charge):
			rv.set(i->charge());
			break;
		case (AtomAccessors::Element):
			rv.set(i->element());
			break;
		case (AtomAccessors::FX):
		case (AtomAccessors::FY):
		case (AtomAccessors::FZ):
			rv.set(i->f().get(vid - AtomAccessors::FX));
			break;
		case (AtomAccessors::Id):
			rv.set(i->id());
			break;
		case (AtomAccessors::Mass):
			rv.set(elements.atomicMass(i));
			break;
		case (AtomAccessors::RX):
		case (AtomAccessors::RY):
		case (AtomAccessors::RZ):
			rv.set(i->r().get(vid - AtomAccessors::RX));
			break;
		case (AtomAccessors::Symbol):
			rv.set(elements.symbol(i));
			break;
		default:
			printf("AtomAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("AtomAccessors::retrieve");
	return result;
}

// Set specified data
bool AtomAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("AtomAccessors::set");
	bool result = TRUE;
	// Cast pointer into Atom*
	Atom *i = (Atom*) classptr;
	if (i == NULL) printf("Warning - NULL Atom pointer passed to AtomAccessors::set.\n");
// 	printf("Enumerated ID supplied to AtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > AtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to AtomAccessors::set.\n", vid);
		msg.exit("AtomAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("AtomAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (AtomAccessors::Charge):
			i->setCharge(srcvar->asDouble());
			break;
		case (AtomAccessors::Element):
			i->setElement(srcvar->asInteger());
			break;
		case (AtomAccessors::FX):
		case (AtomAccessors::FY):
		case (AtomAccessors::FZ):
			i->f().set(vid - AtomAccessors::FX, srcvar->asDouble());
			break;
		case (AtomAccessors::RX):
		case (AtomAccessors::RY):
		case (AtomAccessors::RZ):
			i->r().set(vid - AtomAccessors::RX, srcvar->asDouble());
			break;
		case (AtomAccessors::Symbol):
		case (AtomAccessors::Mass):
		case (AtomAccessors::Id):
			msg.print("Member '%s' in Atom is read-only.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
		default:
			printf("AtomAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("AtomAccessors::set");
	return result;
}
